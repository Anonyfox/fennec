(* Discover and run inline test runners in the dev loop — the realtime test lane.

   The runners are the exes dune generates from (inline_tests) stanzas:
     _build/default/<dir>/.<lib>.inline-tests/inline-test-runner.exe
   Building them (as explicit dune targets) compiles+links without running — the supervisor
   runs them itself after each green settle, so tests never gate the page.

   Discovery is LAZY: the runner dirs only exist after the first `dune build @runtest` (or
   after the first settle that includes them). So we discover on the first run_changed call,
   not at init. *)

type runner = {
  lib : string;
  exe : string;
  target : string;
}

type t = {
  root : string;
  watch_roots : string list;
  mutable runners : runner list option;  (* None = not yet discovered *)
  mtimes : (string, float) Hashtbl.t;
  (* warm-path state (all optional; absence ⇒ the cold path is used, unchanged) *)
  mutable worker : Test_worker.t option;       (* set by the supervisor after it spawns the worker *)
  mutable describe : string option;            (* cached `dune describe workspace`; refreshed lazily *)
  chains : (string, (Test_chain.t, string) Stdlib.result) Hashtbl.t;  (* per-target derived chain (or why not) *)
}

(* ═══════════════════════════════════════════════════════════════════════════ *)
(*  Discovery                                                                *)
(* ═══════════════════════════════════════════════════════════════════════════ *)

let ends_with suffix s =
  let ls = String.length s and lsuf = String.length suffix in
  ls >= lsuf && String.sub s (ls - lsuf) lsuf = suffix

let starts_with s prefix =
  let lp = String.length prefix in
  String.length s >= lp && String.sub s 0 lp = prefix

let uniq xs =
  let rec go seen = function
    | [] -> List.rev seen
    | x :: xs -> if List.mem x seen then go seen xs else go (x :: seen) xs
  in
  go [] xs

let normalize_rel path =
  let path = String.trim path in
  if starts_with path "./" then String.sub path 2 (String.length path - 2) else path

let build_dir root rel = Filename.concat (Filename.concat root "_build/default") rel

(* recursively find .<lib>.inline-tests dirs under _build/default *)
let rec find_dirs acc dir depth =
  if depth > 6 then acc (* don't recurse too deep *)
  else
    let entries = try Sys.readdir dir with _ -> [||] in
    Array.fold_left (fun acc entry ->
      let path = Filename.concat dir entry in
      if ends_with ".inline-tests" entry && entry.[0] = '.' && Sys.is_directory path then begin
        let inner = String.sub entry 1 (String.length entry - 1 - String.length ".inline-tests") in
        let exe_path = Filename.concat path "inline-test-runner.exe" in
        if Sys.file_exists exe_path then begin
          (* target is workspace-relative: strip the root/_build/default/ prefix *)
          let prefix = Filename.concat (Filename.concat "" "_build") "default" in
          let full_prefix = Filename.concat "" prefix in
          let target =
            let lp = String.length full_prefix in
            let le = String.length exe_path in
            if le > lp + 1 && String.sub exe_path 0 lp = full_prefix then
              String.sub exe_path (lp + 1) (le - lp - 1)
            else exe_path
          in
          { lib = inner; exe = exe_path; target } :: acc
        end else acc
      end
      else if Sys.is_directory path && entry.[0] <> '.' then find_dirs acc path (depth + 1)
      else acc)
      acc entries

let rec source_dirs acc dir depth =
  if depth > 8 then acc
  else
    let entries = try Sys.readdir dir with _ -> [||] in
    let acc =
      if Array.exists (( = ) "dune") entries then dir :: acc else acc
    in
    Array.fold_left
      (fun acc entry ->
        let path = Filename.concat dir entry in
        if entry <> "_build" && entry <> ".git" && Sys.file_exists path && Sys.is_directory path then
          source_dirs acc path (depth + 1)
        else acc)
      acc entries

let find_balanced_after s open_pos =
  let rec go depth i =
    if i >= String.length s then None
    else
      match s.[i] with
      | '(' -> go (depth + 1) (i + 1)
      | ')' ->
        let depth = depth - 1 in
        if depth = 0 then Some i else go depth (i + 1)
      | _ -> go depth (i + 1)
  in
  go 0 open_pos

let test_names_from_dune text =
  let rec find_from acc i =
    match Dune_watch.find_sub (String.sub text i (String.length text - i)) "(test" with
    | None -> List.rev acc
    | Some rel ->
      let open_pos = i + rel in
      let next = open_pos + 5 in
      let stop = find_balanced_after text open_pos in
      let stanza =
        match stop with
        | None -> String.sub text open_pos (String.length text - open_pos)
        | Some j -> String.sub text open_pos (j - open_pos + 1)
      in
      let name =
        match Dune_watch.find_sub stanza "(name" with
        | None -> None
        | Some p ->
          let i = ref (p + 5) in
          while !i < String.length stanza && (stanza.[!i] = ' ' || stanza.[!i] = '\n' || stanza.[!i] = '\t') do incr i done;
          let j = ref !i in
          while
            !j < String.length stanza
            && stanza.[!j] <> ' ' && stanza.[!j] <> '\n' && stanza.[!j] <> '\t' && stanza.[!j] <> ')'
          do
            incr j
          done;
          if !j > !i then Some (String.sub stanza !i (!j - !i)) else None
      in
      find_from (match name with Some n -> n :: acc | None -> acc) (match stop with Some j -> j + 1 | None -> next)
  in
  find_from [] 0

let rel_of_abs ~root path =
  let prefix = root ^ "/" in
  if starts_with path prefix then String.sub path (String.length prefix) (String.length path - String.length prefix)
  else path

let conventional_unit_test_runners ~root source_root =
  source_dirs [] source_root 0
  |> List.filter (fun dir -> ends_with "_test" (Filename.basename dir))
  |> List.concat_map (fun dir ->
       let dune = Filename.concat dir "dune" in
       let text = try In_channel.with_open_text dune In_channel.input_all with _ -> "" in
       let rel_dir = rel_of_abs ~root dir |> normalize_rel in
       test_names_from_dune text
       |> List.map (fun name ->
            let exe = Filename.concat (build_dir root rel_dir) (name ^ ".exe") in
            { lib = Filename.basename dir; exe; target = Filename.concat rel_dir (name ^ ".exe") }))

(* exclude fennec-hunt's own runner — it's a build-time dep, not an app library, and its
   runner has a self-referential structure dune can't build as a standalone target *)
let is_hunt_internal lib =
  lib = "fennec_hunt" || lib = "fennec_hunt_unit"

let discover root watch_roots =
  let build_default = Filename.concat root "_build/default" in
  let prefix = build_default ^ "/" in
  let lp = String.length prefix in
  let build_roots =
    match watch_roots with
    | [] -> [ build_default ]
    | xs -> List.map (build_dir root) xs
  in
  let inline =
    List.fold_left (fun acc dir -> find_dirs acc dir 0) [] build_roots
  |> List.filter (fun r -> not (is_hunt_internal r.lib))
  |> List.map (fun r ->
    (* the target dune needs is workspace-relative (no _build/default/ prefix) *)
    let target =
      let le = String.length r.exe in
      if le > lp && String.sub r.exe 0 lp = prefix then String.sub r.exe lp (le - lp)
      else r.target
    in
    { r with target })
  in
  let conventional =
    watch_roots
    |> List.concat_map (fun rel -> conventional_unit_test_runners ~root (Filename.concat root rel))
  in
  inline @ conventional |> List.sort_uniq (fun a b -> compare a.target b.target)

let create ?(watch_roots = []) ~root () =
  { root;
    watch_roots = List.map normalize_rel watch_roots |> uniq;
    runners = None;
    mtimes = Hashtbl.create 8;
    worker = None;
    describe = None;
    chains = Hashtbl.create 16 }

(* discover lazily, returning the runner list *)
let ensure t =
  match t.runners with
  | Some rs -> rs
  | None -> let rs = discover t.root t.watch_roots in t.runners <- Some rs; rs

let mtime path = try (Unix.stat path).Unix.st_mtime with _ -> 0.0

(* the targets to add to dune --watch, discovered lazily after the first build *)
let targets t = List.map (fun (r : runner) -> r.target) (ensure t)

(* The warm worker loads a BYTECODE build of the test, which a native-only [(test)] stanza doesn't
   produce. The convention: a conventional test [<dir>/<name>.exe] has a byte MIRROR exe at
   [<dir>/byte/<name>.exe] (the same source compiled byte-only into a sibling dir — see e.g.
   examples/site/frontend_test/dune). That keeps `@runtest`/`fennec test` running ONLY the native test
   (a byte (test) would double-run the suite + need CAML_LD_LIBRARY_PATH). A test WITHOUT a byte mirror
   simply has no warm chain ⇒ cold fallback. *)
let byte_mirror_target tg =
  (* <dir>/<name>.exe → <dir>/byte/<name>.exe *)
  let dir = Filename.dirname tg and base = Filename.basename tg in
  Filename.concat (Filename.concat dir "byte") base

(* the BYTECODE targets the warm worker needs built alongside the native runners: each conventional
   [(test)] runner's byte-mirror [.bc]. It transitively pulls the test's own .cmo AND the app libs'
   .cma's. Inline-test runners (…/inline-test-runner.exe) are skipped (no mirror ⇒ cold). Adding these
   to the watch means a green settle has the byte artifacts ready when the worker loads them. Only
   meaningful when a worker is active; the supervisor merges them into the watch only then. *)
let byte_targets t =
  ensure t
  |> List.filter_map (fun (r : runner) ->
         let tg = r.target in
         if ends_with "/inline-test-runner.exe" tg || tg = "inline-test-runner.exe" then None
         else if ends_with ".exe" tg then
           let m = byte_mirror_target tg in
           Some (String.sub m 0 (String.length m - 4) ^ ".bc")
         else None)

let prime t =
  ensure t
  |> List.iter (fun (r : runner) -> Hashtbl.replace t.mtimes r.exe (mtime r.exe))

(* ═══════════════════════════════════════════════════════════════════════════ *)
(*  Execution                                                                *)
(* ═══════════════════════════════════════════════════════════════════════════ *)

(* how a result was produced — for the UI/verdict + so the dev knows the warm path engaged *)
type via = Cold | Warm

type result = {
  lib : string;
  passed : int;
  failed : int;
  output : string;
  ms : float;          (* wall time for this runner (cold: process; warm: fork→reap) *)
  via : via;
  dynlink_ms : float;  (* warm only: framework-.cma Dynlink time in the child (0 cold) *)
  run_ms : float;      (* warm only: the test run (test-.cmo load) time (0 cold) *)
}

type summary = { results : result list; total_passed : int; total_failed : int; ms : float }

(* parse a runner's pass/fail tally out of its captured output + exit code. Shared by the cold and
   warm paths so both report identically. (The hunt runner prints "<n> tests passed" on success and a
   failing tally otherwise; we also fall back to counting "ok" lines.) *)
let parse_tally ~code ~output =
  let lines = String.split_on_char '\n' output |> List.filter (fun s -> String.trim s <> "") in
  let last = match List.rev lines with l :: _ -> String.trim l | [] -> "" in
  if code = 0 then
    match int_of_string_opt (try String.sub last 0 (String.index last ' ') with _ -> "0") with
    | Some n when n > 0 -> (n, 0)
    | _ ->
      let ok_lines =
        List.fold_left
          (fun n line ->
            let s = String.trim line in
            if String.length s >= 2 && String.sub s 0 2 = "ok" then n + 1 else n)
          0 lines
      in
      (ok_lines, 0)
  else
    let nums = String.split_on_char ' ' last |> List.filter_map int_of_string_opt in
    match nums with f :: _ :: _ -> (0, f) | _ -> (0, 1)

(* run one runner the COLD way: exec its native exe, capture output, parse the tally. This is the
   always-available fallback; the warm worker path (run_via_worker) is tried first by run_changed. *)
let run_one (r : runner) : result =
  let t0 = Unix.gettimeofday () in
  let tmp = Filename.temp_file "fennec-dev-test-" ".log" in
  let cmd = Printf.sprintf "%s >%s 2>&1" (Filename.quote r.exe) (Filename.quote tmp) in
  let code = Sys.command cmd in
  let ms = (Unix.gettimeofday () -. t0) *. 1000.0 in
  let output = try In_channel.with_open_bin tmp In_channel.input_all with _ -> "" in
  (try Sys.remove tmp with _ -> ());
  let passed, failed = parse_tally ~code ~output in
  { lib = r.lib; passed; failed; output; ms; via = Cold; dynlink_ms = 0.; run_ms = 0. }

(* ═══════════════════════════════════════════════════════════════════════════ *)
(*  Warm path — try the resident worker, fall back to run_one on ANY miss       *)
(* ═══════════════════════════════════════════════════════════════════════════ *)

(* the supervisor hands us the worker handle (and clears it when the worker dies / a framework lib
   changes). [None] ⇒ every runner takes the cold path. *)
let set_worker t w =
  t.worker <- w;
  (* a fresh worker generation can change what is preloaded; the derived chains are still valid (they
     are app-local), so we keep them — only a describe/lib-structure change invalidates, below *)
  ()

(* drop the cached describe + per-target chains. Call when a dune file or the lib graph may have
   changed (the same trigger that restarts the worker), so the next warm run re-derives. *)
let invalidate_chains t =
  (* keep the boot-primed describe. Re-fetching it lazily FAILS while `dune --watch` holds the build
     lock (only the boot window, before the watch starts, is lock-free), and dropping it would force
     EVERY test cold. Only the per-target chains reset — they re-derive from the cached describe, and
     a genuinely stale chain (after a dune-file edit) degrades safely to cold via the worker digest. *)
  Hashtbl.reset t.chains

(* test-only: inject a describe string directly (so an integrated test can exercise the full
   chain→worker→tally path without spawning `dune describe`, which would deadlock under dune runtest). *)
let set_describe_for_test t d = t.describe <- Some d; Hashtbl.reset t.chains

(* test-only: set the discovered runners directly, bypassing the filesystem scan, so an integrated
   test is hermetic about which runner runs. *)
let set_runners_for_test t rs = t.runners <- Some rs

(* fetch `dune describe workspace` once and cache it. Honour FENNEC_DUNE_DESCRIBE (a path to a
   pre-captured describe) for environments where plain `dune describe` can't resolve the intended root
   (e.g. a worktree nested inside another dune project) or for tests. Runs dune the same plain way
   Dune_watch runs `dune build --watch` (root auto-resolved from cwd). *)
let fetch_describe t =
  match t.describe with
  | Some d -> Some d
  | None ->
    let d =
      match Sys.getenv_opt "FENNEC_DUNE_DESCRIBE" with
      | Some path when path <> "" -> (try Some (In_channel.with_open_text path In_channel.input_all) with _ -> None)
      | _ -> (
        try
          let ic = Unix.open_process_in "dune describe workspace 2>/dev/null" in
          let s = In_channel.input_all ic in
          (match Unix.close_process_in ic with Unix.WEXITED 0 when String.trim s <> "" -> Some s | _ -> None)
        with _ -> None)
    in
    (match d with Some _ -> t.describe <- d | None -> ());
    d

(* Prime the describe cache at dev-server BOOT — before `dune --watch` takes the build lock, after
   which `dune describe` errors with "build directory is locked". The supervisor calls this once
   before starting the watch; without it the first warm run would invoke describe under the lock,
   fail, and fall back to cold for the whole session. Best-effort: a failure just leaves the warm
   path to cold-fallback (safe). *)
let prime_describe t = ignore (fetch_describe t)

(* parse the direct (libraries …) of the stanza in [dune_text] that builds the exe named [exe].
   Handles (test/executable (name X) …) and (tests/executables (names … X …) …). Returns [] if the
   stanza or its libraries field isn't found (⇒ the caller can't warm-path → cold). *)
let libraries_from_dune dune_text ~exe =
  let n = String.length dune_text in
  (* split [dune_text] into top-level stanzas (balanced parens) *)
  let stanzas =
    let rec collect acc i =
      (* find the next '(' *)
      match String.index_from_opt dune_text i '(' with
      | None -> List.rev acc
      | Some open_pos -> (
        match find_balanced_after dune_text open_pos with
        | None -> List.rev acc
        | Some close_pos -> collect (String.sub dune_text open_pos (close_pos - open_pos + 1) :: acc) (close_pos + 1))
    in
    collect [] 0
  in
  ignore n;
  (* does [stanza] name [exe] via (name exe) or (names … exe …)? *)
  let names_exe stanza =
    let field_words key =
      match Dune_watch.find_sub stanza key with
      | None -> []
      | Some p ->
        (match find_balanced_after stanza p with
         | None -> []
         | Some close ->
           let inner = String.sub stanza (p + String.length key) (close - (p + String.length key)) in
           String.split_on_char ' ' (String.map (fun c -> if c = '\n' || c = '\t' || c = ')' then ' ' else c) inner)
           |> List.filter_map (fun w -> let w = String.trim w in if w = "" then None else Some w))
    in
    List.mem exe (field_words "(name") || List.mem exe (field_words "(names")
  in
  let lib_words stanza =
    match Dune_watch.find_sub stanza "(libraries" with
    | None -> []
    | Some p ->
      (match find_balanced_after stanza p with
       | None -> []
       | Some close ->
         let inner = String.sub stanza (p + String.length "(libraries") (close - (p + String.length "(libraries")) in
         String.split_on_char ' ' (String.map (fun c -> if c = '\n' || c = '\t' || c = ')' then ' ' else c) inner)
         |> List.filter_map (fun w -> let w = String.trim w in if w = "" then None else Some w))
  in
  match List.find_opt names_exe stanzas with Some s -> lib_words s | None -> []

(* derive (and cache) the runner's app-local Dynlink chain, or a string saying why we can't. *)
let chain_for t (r : runner) : (Test_chain.t, string) Stdlib.result =
  match Hashtbl.find_opt t.chains r.target with
  | Some cached -> cached
  | None ->
    let result =
      match fetch_describe t with
      | None -> Error "no dune describe"
      | Some describe -> (
        (* libs come from the NATIVE test's dune (the (test … (libraries …)) stanza); the test .cmo
           comes from the BYTE MIRROR (<dir>/byte/<name>.exe → its eobjs), since the native test has no
           bytecode object. The mirror must exist (built via byte_targets) or the worker run fails →
           cold fallback. *)
        let dune_path = Filename.concat (Filename.concat t.root (Filename.dirname r.target)) "dune" in
        let dune_text = try In_channel.with_open_text dune_path In_channel.input_all with _ -> "" in
        match libraries_from_dune dune_text ~exe:(Filename.remove_extension (Filename.basename r.target)) with
        | [] -> Error "could not read the test's (libraries …)"
        | test_libs -> (
          match
            Test_chain.derive ~describe ~watch_roots:t.watch_roots ~preloaded:Test_worker.preloaded ~test_libs
              ~target:(byte_mirror_target r.target)
          with
          | Ok chain -> Ok chain
          | Error e -> Error (Test_chain.error_to_string e)))
    in
    (* breadcrumb: when a worker is up but a runner can't warm-path, say why ONCE (cached ⇒ once per
       target). Makes "why did my test run cold?" debuggable instead of silent. *)
    (match result with
     | Error why -> Printf.eprintf "[fennec dev] %s: warm worker declined, ran cold (%s)\n%!" r.lib why
     | Ok _ -> ());
    Hashtbl.replace t.chains r.target result;
    result

(* run one runner via the warm worker, or [None] to signal the caller to fall back to run_one.
   Object paths are made absolute against the build root so the worker (whatever its cwd) resolves
   them. A worker child exit_code <> 0 is a real test FAILURE (returned, not a fallback). *)
let run_via_worker t (r : runner) : result option =
  match t.worker with
  | None -> None
  | Some w -> (
    if not (Test_worker.healthy w) then None
    else
      match chain_for t r with
      | Error _ -> None
      | Ok chain ->
        let abs rel = if Filename.is_relative rel then Filename.concat (Filename.concat t.root "_build/default") rel else rel in
        let objs = List.map abs (Test_chain.objects chain) in
        (match Test_worker.run w ~chain:objs with
         | None -> None
         (* 75 = the worker's load-failed sentinel (chain couldn't Dynlink: framework CRC mismatch /
            missing dep). Fall back to the cold native exe — never surface it as a red test. *)
         | Some res when res.Test_worker.exit_code = 75 ->
           Printf.eprintf "[fennec dev] %s: warm worker couldn't load the chain (CRC/dep mismatch) — running cold\n%!" r.lib;
           None
         | Some res ->
           let passed, failed = parse_tally ~code:res.Test_worker.exit_code ~output:res.Test_worker.output in
           Some
             { lib = r.lib;
               passed;
               failed;
               output = res.Test_worker.output;
               ms = res.Test_worker.total_ms;
               via = Warm;
               dynlink_ms = res.Test_worker.dynlink_ms;
               run_ms = res.Test_worker.run_ms }))

(* run one runner: warm if the worker can take it, else cold. NEVER fails — cold is always available,
   and ANY unexpected exception on the warm path drops to cold (the feature must never break the loop). *)
let run_one_smart t (r : runner) : result =
  match (try run_via_worker t r with _ -> None) with Some res -> res | None -> run_one r

let run_changed t =
  let runners = ensure t in
  let changed = List.filter (fun (r : runner) ->
    let cur = mtime r.exe in
    let seen = Hashtbl.mem t.mtimes r.exe in
    let prev = try Hashtbl.find t.mtimes r.exe with Not_found -> 0.0 in
    Hashtbl.replace t.mtimes r.exe cur;
    cur > prev && seen (* skip an unprimed first observation; primed missing exes are allowed to appear *)
  ) runners in
  if changed = [] then None
  else begin
    let t0 = Unix.gettimeofday () in
    let results = List.map (run_one_smart t) changed in
    let ms = (Unix.gettimeofday () -. t0) *. 1000.0 in
    let total_passed = List.fold_left (fun a r -> a + r.passed) 0 results in
    let total_failed = List.fold_left (fun a r -> a + r.failed) 0 results in
    Some { results; total_passed; total_failed; ms }
  end

let%test "parses conventional dune test names" =
  test_names_from_dune "(test\n (name test_components)\n (libraries fennec.fur))\n"
  = [ "test_components" ]

let%test "discovers conventional unit test targets under watched roots" =
  let root = Filename.concat (Filename.get_temp_dir_name ()) ("fennec-dev-tests-" ^ string_of_int (Unix.getpid ())) in
  let app = Filename.concat root "examples/site/frontend_test" in
  let rec mkdir_p dir =
    if dir = "" || dir = "." || dir = "/" || Sys.file_exists dir then ()
    else (mkdir_p (Filename.dirname dir); Unix.mkdir dir 0o755)
  in
  mkdir_p app;
  Out_channel.with_open_text (Filename.concat app "dune") (fun oc ->
      output_string oc "(test\n (name test_components)\n (modules test_components))\n");
  let runners = discover root [ "examples/site" ] in
  let ok =
    List.exists
      (fun (r : runner) ->
        r.lib = "frontend_test"
        && r.target = "examples/site/frontend_test/test_components.exe"
        && Filename.basename r.exe = "test_components.exe")
      runners
  in
  (try Sys.remove (Filename.concat app "dune") with _ -> ());
  (try Unix.rmdir app with _ -> ());
  (try Unix.rmdir (Filename.dirname app) with _ -> ());
  (try Unix.rmdir (Filename.dirname (Filename.dirname app)) with _ -> ());
  (try Unix.rmdir root with _ -> ());
  ok

let%test "libraries_from_dune reads a (test) stanza's libraries by exe name" =
  let text =
    "(test\n (name test_components)\n (modes byte exe)\n (modules test_components)\n (libraries fennec.fur fennec.fur.server site_components site_store))\n\
     (executable (name snapshot_html) (libraries other_lib))\n"
  in
  libraries_from_dune text ~exe:"test_components"
  = [ "fennec.fur"; "fennec.fur.server"; "site_components"; "site_store" ]

let%test "libraries_from_dune picks the RIGHT stanza when several exes share a dune" =
  let text =
    "(test (name test_components) (libraries a b))\n(executable (name snapshot_html) (libraries c d))\n"
  in
  libraries_from_dune text ~exe:"snapshot_html" = [ "c"; "d" ]

let%test "libraries_from_dune handles a (tests (names …)) plural stanza" =
  let text = "(tests (names foo bar) (libraries x y))\n" in
  libraries_from_dune text ~exe:"bar" = [ "x"; "y" ]

let%test "libraries_from_dune returns [] for an unknown exe (⇒ cold fallback)" =
  libraries_from_dune "(test (name a) (libraries z))" ~exe:"nope" = []

let%test "byte_targets maps conventional .exe runners to the byte-mirror .bc and skips inline runners" =
  let t = create ~watch_roots:[ "x" ] ~root:"/r" () in
  t.runners <-
    Some
      [ { lib = "frontend_test"; exe = "/r/_build/default/x/ft/test_components.exe"; target = "x/ft/test_components.exe" };
        { lib = "some_lib"; exe = "/r/_build/default/x/.some_lib.inline-tests/inline-test-runner.exe";
          target = "x/.some_lib.inline-tests/inline-test-runner.exe" } ];
  byte_targets t = [ "x/ft/byte/test_components.bc" ]

let%test "byte_mirror_target puts the byte exe in a sibling byte/ dir" =
  byte_mirror_target "examples/site/frontend_test/test_components.exe"
  = "examples/site/frontend_test/byte/test_components.exe"
