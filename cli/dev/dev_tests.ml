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
  { root; watch_roots = List.map normalize_rel watch_roots |> uniq; runners = None; mtimes = Hashtbl.create 8 }

(* discover lazily, returning the runner list *)
let ensure t =
  match t.runners with
  | Some rs -> rs
  | None -> let rs = discover t.root t.watch_roots in t.runners <- Some rs; rs

let mtime path = try (Unix.stat path).Unix.st_mtime with _ -> 0.0

(* the targets to add to dune --watch, discovered lazily after the first build *)
let targets t = List.map (fun (r : runner) -> r.target) (ensure t)

let prime t =
  ensure t
  |> List.iter (fun (r : runner) -> Hashtbl.replace t.mtimes r.exe (mtime r.exe))

(* ═══════════════════════════════════════════════════════════════════════════ *)
(*  Execution                                                                *)
(* ═══════════════════════════════════════════════════════════════════════════ *)

type result = { lib : string; passed : int; failed : int; output : string; ms : float }
type summary = { results : result list; total_passed : int; total_failed : int; ms : float }

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
       | Some n when n > 0 -> (n, 0)
       | _ ->
         let ok_lines =
           List.fold_left
             (fun n line ->
               let s = String.trim line in
               if String.length s >= 2 && String.sub s 0 2 = "ok" then n + 1 else n)
             0 lines
         in
         (ok_lines, 0))
    else
      let nums = String.split_on_char ' ' last |> List.filter_map int_of_string_opt in
      match nums with f :: _ :: _ -> (0, f) | _ -> (0, 1)
  in
  { lib = r.lib; passed; failed; output; ms }

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
    let results = List.map run_one changed in
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
