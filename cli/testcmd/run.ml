(* The `fennec test` entry point: parse the cut + options, dispatch.

   - unit    → delegate to dune's fast gate (`dune build @runtest`)
   - http    → orchestrate the Http suites (per-suite isolated app instance)  [T6]
   - browser → orchestrate the Browser suites (+ Chrome)                       [T7]
   - all     → unit, then http, then browser (fast-to-slow)

   The pure parts (the cut enum + its parsing, the options record) are unit-tested; the
   orchestration is integration-tested against the example app. *)

type suite = Unit | Http | Browser | All

let suite_of_string s =
  match String.lowercase_ascii s with
  | "unit" -> Ok Unit
  | "http" -> Ok Http
  | "browser" -> Ok Browser
  | "all" -> Ok All
  | other -> Error (Printf.sprintf "unknown test suite %S — expected one of: unit, http, browser, all" other)

let suite_to_string = function Unit -> "unit" | Http -> "http" | Browser -> "browser" | All -> "all"

type options = {
  suite : suite;
  grep : string option;       (* narrow to matching suites/cases (passed through) *)
  max_failures : int option;  (* stop after N failures; None + fail_fast = stop at first *)
  fail_fast : bool;           (* default true; --no-fail-fast disables *)
  watch : bool;               (* re-run on change *)
  reporter : string option;   (* e.g. "list", "list,junit" *)
  jobs : int option;          (* parallel suites; None = CPUs *)
  headed : bool;              (* browser cut: show the window *)
  screenshots : string option;(* browser cut: PNG-on-failure dir *)
  base_port : int;            (* per-suite instance block base *)
}

let default_options =
  (* base 8200: clear of dev's 4000 AND of macOS's port 7000 (AirPlay/ControlCenter) *)
  { suite = Unit; grep = None; max_failures = None; fail_fast = true; watch = false;
    reporter = None; jobs = None; headed = false; screenshots = None; base_port = 8200 }

module Discover = Fennec_dev.Discover

(* the fast gate: dune already builds + runs every @runtest test and returns the right exit
   code. fennec is the only dune-aware process here (no nested watcher), so this is safe. *)
let run_unit () = Sys.command "dune build @runtest"

(* run one suite exe against its isolated instance's env; inherit stdout/stderr so the suite's
   own ✓/✗ report reaches the user. Returns the suite's exit code. *)
let run_suite_exe ~exe ~(env : (string * string) list) : int =
  let pid = Unix.create_process_env exe [| exe |] (Boot.env_array env) Unix.stdin Unix.stdout Unix.stderr in
  match Unix.waitpid [] pid with
  | _, Unix.WEXITED c -> c
  | _, (Unix.WSIGNALED _ | Unix.WSTOPPED _) -> 128

(* orchestrate one cut: discover the server + suites, build everything once, then for each
   suite boot a DEDICATED isolated instance, run the suite against it, tear it down. Returns
   the number of failed suites (0 = all passed). Sequential for now — the per-suite isolation
   (distinct ports) makes parallel execution safe to add next. *)
let orchestrate ~(cut : suite) ~dir ~base : int =
  ignore (Sys.command "dune shutdown >/dev/null 2>&1"); (* stop any orphaned dev watcher → no lock clash *)
  match Discover.find () with
  | Error msg -> Printf.eprintf "fennec test: %s\n%!" msg; 1
  | Ok d ->
    let cwd = Sys.getcwd () in
    let suites = Suites.discover ~root:d.Discover.root ~cwd ~dir in
    if suites = [] then (
      Printf.printf "fennec test: no %s suites found (looked in %s)\n%!" (suite_to_string cut) (Filename.concat cwd dir);
      0 (* nothing to run is not a failure *))
    else begin
      (* build the server, its webroot, and the suites in one dune invocation — fennec is the
         sole dune-aware process, so no nested-build lock deadlock. Targets are root-relative,
         so build from the workspace root (like the dev command does). *)
      Sys.chdir d.Discover.root;
      let webroot = Filename.concat d.Discover.src_dir "webroot" in
      let app_targets = d.Discover.targets @ [ webroot ] in
      let build_cmd = "dune build " ^ String.concat " " (List.map Filename.quote (app_targets @ List.map (fun (s : Suites.t) -> s.target) suites)) in
      match Sys.command build_cmd with
      | n when n <> 0 -> Printf.eprintf "fennec test: `dune build` failed (exit %d) — see the errors above\n%!" n; 1
      | _ ->
        let instances = Instance.allocate ~base (List.map (fun (s : Suites.t) -> s.Suites.name) suites) in
        let failed = ref 0 in
        List.iter2
          (fun (suite : Suites.t) (inst : Instance.t) ->
            Printf.printf "\n\027[1m▶ %s\027[0m \027[2m(:%d)\027[0m\n%!" suite.Suites.name inst.Instance.port;
            let boot = Boot.spawn ~exe:d.Discover.exe ~env:inst.Instance.server_env in
            Fun.protect ~finally:(fun () -> Boot.stop boot; Boot.cleanup boot) (fun () ->
                match Boot.wait_ready boot ~port:inst.Instance.port ~timeout:30.0 with
                | Error msg ->
                  incr failed;
                  Printf.eprintf "  \027[31m✗ instance for %s never came up: %s\027[0m\n%!" suite.Suites.name msg;
                  let log = Boot.read_log boot in
                  if log <> "" then Printf.eprintf "     server log:\n%s\n%!" log
                | Ok () ->
                  let code = run_suite_exe ~exe:suite.Suites.exe ~env:inst.Instance.suite_env in
                  if code <> 0 then incr failed))
          suites instances;
        !failed
    end

let run (opts : options) : int =
  match opts.suite with
  | Unit -> run_unit ()
  | Http -> if orchestrate ~cut:Http ~dir:"test/http" ~base:opts.base_port = 0 then 0 else 1
  | Browser ->
    Printf.eprintf "fennec test: the browser cut arrives next (T7)\n%!";
    2
  | All ->
    let u = run_unit () in
    let h = if orchestrate ~cut:Http ~dir:"test/http" ~base:opts.base_port = 0 then 0 else 1 in
    if u = 0 && h = 0 then 0 else 1
