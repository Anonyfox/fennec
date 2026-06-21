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

(* the dev-loop build profile: FENNEC_DEV_PROFILE, default `fastdev` *)
let profile () = match Sys.getenv_opt "FENNEC_DEV_PROFILE" with Some p when p <> "" -> p | _ -> "fastdev"

let is_default_profile p = p = "dev" || p = "default"

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

(* re-point a `_build/…`-relative path (e.g. the server exe the user passed, or discover found) at the
   active build root: `_build/default/x` -> `<context_dir>/x`. Left unchanged if it isn't under
   `_build/` or if we're on the default build dir. *)
let retarget ~root path =
  match dune_build_dir ~root with
  | None -> path
  | Some _ ->
    let pfx = "_build/default/" in
    if String.length path > String.length pfx && String.sub path 0 (String.length pfx) = pfx then
      Filename.concat (context_dir ~root) (String.sub path (String.length pfx) (String.length path - String.length pfx))
    else path
