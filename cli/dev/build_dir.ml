(* See README.md (the package map). Per-profile BUILD DIRECTORIES — cargo's `target/{debug,release}`
   for dune.

   The dev loop builds in the `fastdev` profile (byte, fast relink + the warm test worker); `dune build`
   / `dune runtest` / `fennec test` build in `dev` (native). With ONE shared `_build/default` those two
   flag-sets invalidate each other, so every switch between `fennec dev` and a normal build forced a
   full workspace rebuild. The fix is to give each profile its OWN build root, so both stay warm.

   The mechanism is dune's [DUNE_BUILD_DIR] env var (honoured by EVERY dune invocation — the watch, the
   worker build, `dune describe`), so the supervisor sets it once and nothing has to thread a
   [--build-dir] flag. These helpers compute the matching on-disk paths the supervisor and the warm
   worker machinery read back. [root] is always the absolute workspace root ([Sys.getcwd ()]).

   Layout (cargo-style, all gitignored under `_build/`):
     dev / default  ->  _build/default/…        (DUNE_BUILD_DIR unset: dune's own default; shared with
                                                  `dune build` so they never rebuild each other)
     fastdev        ->  _build/fastdev/default/… (isolated; an ABSOLUTE DUNE_BUILD_DIR — dune rejects a
                                                  RELATIVE build dir nested under `_build`) *)

(* The active build profile, from FENNEC_DEV_PROFILE. Default is `dev` — the STANDARD `_build/default`
   that `dune build` / `dune runtest` / `fennec test` use. Only `fennec dev` opts into `fastdev` (it
   sets FENNEC_DEV_PROFILE explicitly before booting the supervisor), so every OTHER entry point —
   `fennec test`, the inline tests, the warm-worker drive test — resolves the default dir with no env
   fiddling. That keeps this module the single source of truth for "which build dir am I in". *)
let profile () = match Sys.getenv_opt "FENNEC_DEV_PROFILE" with Some p when p <> "" -> p | _ -> "dev"

let is_default_profile p = p = "dev" || p = "default"

(* the profile `fennec dev` runs under: `fastdev` (dev minus -g, fast relinks + isolated build dir) by
   default, or `dev` with --debug (restores -g backtraces, shares `_build/default`). The dev command
   exports this as FENNEC_DEV_PROFILE before booting the supervisor; {!profile} then reads it back. This
   is the ONLY place the "fastdev" name lives. *)
let dev_loop_profile ~debug = if debug then "dev" else "fastdev"

(* what to export as DUNE_BUILD_DIR, or [None] to leave dune on its standard `_build` (the [dev] case,
   so the dev loop SHARES artifacts with `dune build`). For any other profile, an ABSOLUTE
   `<root>/_build/<profile>` — absolute because dune refuses a relative build dir under `_build`. *)
let dune_build_dir ~root =
  let p = profile () in
  if is_default_profile p then None else Some (Filename.concat (Filename.concat root "_build") p)

(* the build CONTEXT directory (where artifacts actually land), ALWAYS absolute — used to find runner
   exes, the worker .bc, and to prefix the worker's workspace-relative load chain. *)
let context_dir ~root =
  match dune_build_dir ~root with Some d -> Filename.concat d "default" | None -> Filename.concat (Filename.concat root "_build") "default"

(* the prefix to strip off a [dune describe] [source_dir] to recover the workspace-relative path.
   describe echoes paths in the SAME form as DUNE_BUILD_DIR: absolute when we set it (fastdev), or the
   relative `_build/default/` when we don't (dev). So this must match {!dune_build_dir}. *)
let describe_prefix ~root =
  match dune_build_dir ~root with Some d -> Filename.concat d "default" ^ "/" | None -> "_build/default/"

(* export DUNE_BUILD_DIR into THIS process's env so every dune child (watch, worker build, describe)
   builds into the per-profile root. A no-op for the [dev] profile (dune's default is already right). *)
let export ~root = match dune_build_dir ~root with Some d -> Unix.putenv "DUNE_BUILD_DIR" d | None -> ()

(* re-point a path under the standard `_build/default/` at the active build context:
   `…/_build/default/x` -> `<context_dir>/x`. Left unchanged off the default build dir, or for a path
   that isn't under `_build/default/`.

   CRITICAL: discovery hands the supervisor an ABSOLUTE exe (`<root>/_build/default/<src>/<name>.bc`,
   see {!Discover}) and the dev command chdirs to that same root before running, so the ABSOLUTE form
   is the one that actually re-points the running server (and, via `dirname exe`, the webroot it
   serves). Matching only the workspace-relative `_build/default/` form silently ran the server out of
   `_build/default` while the watch rebuilt `_build/fastdev` — so frontend edits never reached the
   served bundle (livereload was dead). We handle both forms. *)
let retarget ~root path =
  match dune_build_dir ~root with
  | None -> path
  | Some _ ->
    let ctx = context_dir ~root in
    let starts_with s pfx = String.length s >= String.length pfx && String.sub s 0 (String.length pfx) = pfx in
    let move pfx = Filename.concat ctx (String.sub path (String.length pfx) (String.length path - String.length pfx)) in
    let abs_pfx = root ^ "/_build/default/" in
    if starts_with path abs_pfx then move abs_pfx
    else if starts_with path "_build/default/" then move "_build/default/"
    else path

(* ──── tests ──── *)

(* drive [retarget] under a chosen profile, restoring the env afterwards (inline tests share a process) *)
let with_profile p f =
  let saved = Sys.getenv_opt "FENNEC_DEV_PROFILE" in
  Fun.protect
    ~finally:(fun () -> match saved with Some v -> Unix.putenv "FENNEC_DEV_PROFILE" v | None -> Unix.putenv "FENNEC_DEV_PROFILE" "")
    (fun () -> Unix.putenv "FENNEC_DEV_PROFILE" p; f ())

let%test "retarget: ABSOLUTE _build/default exe -> active context (the livereload fix)" =
  with_profile "fastdev" (fun () ->
      retarget ~root:"/ws" "/ws/_build/default/examples/site/server.bc"
      = "/ws/_build/fastdev/default/examples/site/server.bc")

let%test "retarget: workspace-relative form still works" =
  with_profile "fastdev" (fun () -> retarget ~root:"/ws" "_build/default/a/b.bc" = "/ws/_build/fastdev/default/a/b.bc")

let%test "retarget: a path outside _build/default is unchanged" =
  with_profile "fastdev" (fun () -> retarget ~root:"/ws" "/somewhere/else/x" = "/somewhere/else/x")

let%test "retarget: default profile is a no-op (shares _build/default)" =
  with_profile "dev" (fun () -> retarget ~root:"/ws" "/ws/_build/default/x.bc" = "/ws/_build/default/x.bc")
