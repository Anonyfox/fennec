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
  mutable runners : runner list option;  (* None = not yet discovered *)
  mtimes : (string, float) Hashtbl.t;
}

(* ═══════════════════════════════════════════════════════════════════════════ *)
(*  Discovery                                                                *)
(* ═══════════════════════════════════════════════════════════════════════════ *)

let ends_with suffix s =
  let ls = String.length s and lsuf = String.length suffix in
  ls >= lsuf && String.sub s (ls - lsuf) lsuf = suffix

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

(* exclude fennec-hunt's own runner — it's a build-time dep, not an app library, and its
   runner has a self-referential structure dune can't build as a standalone target *)
let is_hunt_internal lib =
  lib = "fennec_hunt" || lib = "fennec_hunt_unit"

let discover root =
  let build_default = Filename.concat root "_build/default" in
  let prefix = build_default ^ "/" in
  let lp = String.length prefix in
  find_dirs [] build_default 0
  |> List.filter (fun r -> not (is_hunt_internal r.lib))
  |> List.map (fun r ->
    (* the target dune needs is workspace-relative (no _build/default/ prefix) *)
    let target =
      let le = String.length r.exe in
      if le > lp && String.sub r.exe 0 lp = prefix then String.sub r.exe lp (le - lp)
      else r.target
    in
    { r with target })

let create ~root = { root; runners = None; mtimes = Hashtbl.create 8 }

(* discover lazily, returning the runner list *)
let ensure t =
  match t.runners with
  | Some rs -> rs
  | None -> let rs = discover t.root in t.runners <- Some rs; rs

(* the targets to add to dune --watch, discovered lazily after the first build *)
let targets t = List.map (fun (r : runner) -> r.target) (ensure t)

(* ═══════════════════════════════════════════════════════════════════════════ *)
(*  Execution                                                                *)
(* ═══════════════════════════════════════════════════════════════════════════ *)

type result = { lib : string; passed : int; failed : int; output : string; ms : float }
type summary = { results : result list; total_passed : int; total_failed : int; ms : float }

let mtime path = try (Unix.stat path).Unix.st_mtime with _ -> 0.0

(* run one runner, capture output, parse the tally from the last line *)
let run_one (r : runner) : result =
  let t0 = Unix.gettimeofday () in
  let tmp = Filename.temp_file "fennec-dev-test-" ".log" in
  let cmd = Printf.sprintf "%s >%s 2>&1" (Filename.quote r.exe) (Filename.quote tmp) in
  let code = Sys.command cmd in
  let ms = (Unix.gettimeofday () -. t0) *. 1000.0 in
  let output = try In_channel.with_open_bin tmp In_channel.input_all with _ -> "" in
  (try Sys.remove tmp with _ -> ());
  let lines = String.split_on_char '\n' output |> List.filter (fun s -> String.trim s <> "") in
  let last = match List.rev lines with l :: _ -> String.trim l | [] -> "" in
  let passed, failed =
    if code = 0 then
      (match int_of_string_opt (try String.sub last 0 (String.index last ' ') with _ -> "0") with
       | Some n -> (n, 0) | None -> (0, 0))
    else
      let nums = String.split_on_char ' ' last |> List.filter_map int_of_string_opt in
      match nums with f :: _ :: _ -> (0, f) | _ -> (0, 1)
  in
  { lib = r.lib; passed; failed; output; ms }

let run_changed t =
  let runners = ensure t in
  let changed = List.filter (fun (r : runner) ->
    let cur = mtime r.exe in
    let prev = try Hashtbl.find t.mtimes r.exe with Not_found -> 0.0 in
    Hashtbl.replace t.mtimes r.exe cur;
    cur > prev && prev > 0.0 (* skip the very first observation — seed, don't run *)
  ) runners in
  if changed = [] then None
  else begin
    let t0 = Unix.gettimeofday () in
    let results = List.map run_one changed in
    let ms = (Unix.gettimeofday () -. t0) *. 1000.0 in
    let total_passed = List.fold_left (fun a r -> a + r.passed) 0 results in
    let total_failed = List.fold_left (fun a r -> a + r.failed) 0 results in
    Some { results; total_passed; total_failed; ms }
  end
