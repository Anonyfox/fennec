type served_change = Backend_restart | Full_reload | Css_only | No_served_change

type test_verdict =
  | Tests_not_wired
  | Tests_not_changed
  | Tests_passed of { passed : int; libs : int; ms : float }
  | Tests_failed of { passed : int; failed : int; libs : int; ms : float; failures : (string * string) list }

type ready = {
  url : string;
  dir : string;
}

type build_ok = {
  trigger : string list;
  served : served_change;
  build_ms : float option;
  tests : test_verdict;
  affected : Affected.t;
}

type build_failed = {
  trigger : string list;
  diagnostics : Diagnostics.problem list;
  raw : string;
  last_good_serving : bool;
  affected : Affected.t;
}

type t =
  | Ready of ready
  | Build_ok of build_ok
  | Build_failed of build_failed
  | Watcher_restart
  | Watcher_exit
  | Server_crash of string
  | Server_restart of string
  | Stopped

let trigger_label = function
  | [] -> "filesystem change"
  | [ x ] -> x
  | x :: xs -> Printf.sprintf "%s +%d" x (List.length xs)

let served_kind = function
  | Backend_restart -> "reload"
  | Full_reload -> "reload"
  | Css_only -> "css"
  | No_served_change -> "idle"

let served_text = function
  | Backend_restart -> "backend restart"
  | Full_reload -> "reload"
  | Css_only -> "css"
  | No_served_change -> "build ok · no served change"

let tests_text = function
  | Tests_not_wired | Tests_not_changed -> None
  | Tests_passed { passed; libs; _ } ->
    Some (Printf.sprintf "tests %d passed, 0 failed · %d lib%s" passed libs (if libs = 1 then "" else "s"))
  | Tests_failed { passed; failed; libs; _ } ->
    Some (Printf.sprintf "tests %d passed, %d failed · %d lib%s" passed failed libs (if libs = 1 then "" else "s"))

let diagnostic_text (p : Diagnostics.problem) =
  let loc = if p.col > 0 then Printf.sprintf "%s:%d:%d" p.file p.line p.col else Printf.sprintf "%s:%d" p.file p.line in
  let lines = [ loc ] @ p.excerpt @ [ p.message ] @ p.related in
  lines |> List.map String.trim |> List.filter (fun s -> s <> "") |> String.concat "\n"

let first_diagnostic_text diagnostics raw =
  match diagnostics with
  | p :: _ -> diagnostic_text p
  | [] ->
    raw |> String.split_on_char '\n' |> List.map String.trim |> List.filter (fun s -> s <> "") |> fun lines ->
    String.concat "\n" (List.filteri (fun i _ -> i < 8) lines)

let summary = function
  | Ready { url; dir } -> Printf.sprintf "ready · %s · watching %s" url dir
  | Build_ok b ->
    let base =
      match b.served with
      | No_served_change -> served_text b.served
      | _ -> Printf.sprintf "%s %s" (trigger_label b.trigger) (served_text b.served)
    in
    let affected = Affected.short b.affected in
    let lines = if affected = "" then [ base ] else [ base; "affected: " ^ affected ] in
    let lines = match tests_text b.tests with None -> lines | Some t -> lines @ [ t ] in
    String.concat "\n" lines
  | Build_failed b ->
    let head =
      Printf.sprintf "build failed · %s%s" (trigger_label b.trigger)
        (if b.last_good_serving then " · last good build still serving" else " · server not running")
    in
    let affected = Affected.short b.affected in
    let lines = if affected = "" then [ head ] else [ head; "affected: " ^ affected ] in
    String.concat "\n" (lines @ [ ""; first_diagnostic_text b.diagnostics b.raw ])
  | Watcher_restart -> "dune watcher exited · restarting"
  | Watcher_exit -> "dune --watch keeps exiting · restart `fennec dev`"
  | Server_crash msg -> msg
  | Server_restart msg -> msg
  | Stopped -> "stopped"

let test_fields = function
  | Tests_not_wired -> [ ("tests", "not_wired") ]
  | Tests_not_changed -> [ ("tests", "not_changed") ]
  | Tests_passed { passed; libs; ms } ->
    [ ("tests", "passed"); ("tests_passed", string_of_int passed); ("tests_failed", "0"); ("test_libs", string_of_int libs); ("test_ms", Printf.sprintf "%.0f" ms) ]
  | Tests_failed { passed; failed; libs; ms; _ } ->
    [ ("tests", "failed"); ("tests_passed", string_of_int passed); ("tests_failed", string_of_int failed); ("test_libs", string_of_int libs); ("test_ms", Printf.sprintf "%.0f" ms) ]

let fields = function
  | Ready { url; _ } -> [ ("url", url) ]
  | Build_ok b ->
    [ ("served", served_text b.served); ("affected", Affected.short b.affected) ] @ test_fields b.tests
  | Build_failed b ->
    [ ("serving", string_of_bool b.last_good_serving);
      ("affected", Affected.short b.affected);
      ("diagnostics", string_of_int (List.length b.diagnostics)) ]
  | Watcher_restart | Watcher_exit | Server_crash _ | Server_restart _ | Stopped -> []

let agent_event = function
  | Ready _ as v -> ("ready", summary v, [], None, fields v)
  | Build_ok b as v -> (served_kind b.served, summary v, b.trigger, b.build_ms, fields v)
  | Build_failed b as v -> ("build_failed", summary v, b.trigger, None, fields v)
  | Watcher_restart as v -> ("watcher_restart", summary v, [], None, [])
  | Watcher_exit as v -> ("watcher_exit", summary v, [], None, [])
  | Server_crash _ as v -> ("server_crash", summary v, [], None, [])
  | Server_restart _ as v -> ("server_restart", summary v, [], None, [])
  | Stopped as v -> ("stopped", summary v, [], None, [])

let tests_of_summary = function
  | None -> Tests_not_changed
  | Some s when s.Dev_tests.total_failed = 0 ->
    Tests_passed { passed = s.Dev_tests.total_passed; libs = List.length s.Dev_tests.results; ms = s.Dev_tests.ms }
  | Some s ->
    Tests_failed
      { passed = s.Dev_tests.total_passed;
        failed = s.Dev_tests.total_failed;
        libs = List.length s.Dev_tests.results;
        ms = s.Dev_tests.ms;
        failures =
          List.filter_map
            (fun (r : Dev_tests.result) -> if r.Dev_tests.failed > 0 then Some (r.Dev_tests.lib, String.trim r.Dev_tests.output) else None)
            s.Dev_tests.results }

let%test "build ok summary includes affected surface and tests" =
  let affected = Affected.classify [ "examples/site/frontend/components/nav.mlx changed" ] in
  let v = Build_ok { trigger = [ "examples/site/frontend/components/nav.mlx changed" ]; served = Full_reload; build_ms = Some 12.; tests = Tests_passed { passed = 3; libs = 1; ms = 4. }; affected } in
  let s = summary v in
  Fennec_hunt_unit.str_contains s "reload" && Fennec_hunt_unit.str_contains s "affected: component nav"
  && Fennec_hunt_unit.str_contains s "tests 3 passed"

let%test "build failed summary carries focused diagnostic" =
  let diagnostics = Diagnostics.parse "File \"a.ml\", line 2, characters 3-8:\n2 | value\n    ^^^^^\nError: Unbound value value\n" in
  let v = Build_failed { trigger = [ "a.ml changed" ]; diagnostics; raw = ""; last_good_serving = true; affected = Affected.classify [ "a.ml changed" ] } in
  let s = summary v in
  Fennec_hunt_unit.str_contains s "last good build still serving" && Fennec_hunt_unit.str_contains s "a.ml:2:4"
