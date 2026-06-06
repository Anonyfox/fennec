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
module Port = Fennec_dev.Port

(* one-line preview of a (possibly long, possibly multi-line) command for diagnostics *)
let preview ?(max = 120) s =
  let s = String.map (fun c -> if c = '\n' || c = '\t' then ' ' else c) s in
  if String.length s <= max then s else String.sub s 0 (max - 1) ^ "\u{2026}"

(* the fast gate: dune already builds + runs every @runtest test and returns the right exit
   code. fennec is the only dune-aware process here (no nested watcher), so this is safe. *)
let run_unit () = Sys.command "dune build @runtest"

(* the argv we hand a suite executable, derived from the options + cut. We pass only what a
   given runner actually honours (both runners ignore unknown argv, but silent no-ops are poor
   DX, so we stay precise):
   - http ([hunt]) honours --grep (filters checks by label substring);
   - browser ([Run.main_cli]) honours --grep + --headed/--screenshots/--jobs/--reporter;
   - unit goes through dune (@runtest), so it takes no argv here. *)
let grep_args (o : options) = match o.grep with Some g -> [ "--grep"; g ] | None -> []

let suite_args ~(cut : suite) (o : options) : string list =
  match cut with
  | Http -> grep_args o
  | Browser ->
    grep_args o
    @ (if o.headed then [ "--headed" ] else [])
    @ (match o.screenshots with Some d -> [ "--screenshots"; d ] | None -> [])
    @ (match o.jobs with Some j -> [ "--jobs"; string_of_int j ] | None -> [])
    @ (match o.reporter with Some r -> [ "--reporter"; r ] | None -> [])
  | Unit | All -> []

(* run one suite exe against its isolated instance's env; inherit stdout/stderr so the suite's
   own ✓/✗ report reaches the user. [args] is argv beyond argv[0]. Returns the exit code. *)
let run_suite_exe ~exe ~(args : string list) ~(env : (string * string) list) : int =
  let pid = Unix.create_process_env exe (Array.of_list (exe :: args)) (Boot.env_array env) Unix.stdin Unix.stdout Unix.stderr in
  match Unix.waitpid [] pid with
  | _, Unix.WEXITED c -> c
  | _, (Unix.WSIGNALED _ | Unix.WSTOPPED _) -> 128

(* one suite end-to-end against a DEDICATED isolated instance; returns [true] iff it passed.
   Before booting we make the port usable, distinguishing the two cases the AirPlay incident
   exposed: a leftover of OURS (reclaim it — SIGKILL, the existing dev self-heal) vs. a FOREIGN
   holder (name it, never touch it — fail this suite with a clear message). Teardown of the
   instance is structural (Fun.protect), so an assertion failure or exception never orphans it. *)
let run_one_suite ~server_exe ~(args : string list) ~(suite : Suites.t) ~(inst : Instance.t) : bool =
  Printf.printf "\n\027[1m\u{25b6} %s\027[0m \027[2m(:%d)\027[0m\n%!" suite.Suites.name inst.Instance.port;
  match Port.foreign_holder ~exe:server_exe inst.Instance.port with
  | Some (pid, cmd) ->
    Printf.eprintf
      "  \027[31m\u{2717} port %d is held by another process (pid %d) \u{2014} not ours, leaving it alone.\027[0m\n     %s\n     free that port, or move the test range with --port.\n%!"
      inst.Instance.port pid (preview cmd);
    false
  | None ->
    ignore (Port.reclaim ~exe:server_exe inst.Instance.port (* clear a leftover of OURS, if any *));
    let boot = Boot.spawn ~exe:server_exe ~env:inst.Instance.server_env in
    Fun.protect
      ~finally:(fun () -> Boot.stop boot; Boot.cleanup boot)
      (fun () ->
        match Boot.wait_ready boot ~port:inst.Instance.port ~timeout:30.0 with
        | Error msg ->
          Printf.eprintf "  \027[31m\u{2717} instance for %s never came up: %s\027[0m\n%!" suite.Suites.name msg;
          let log = Boot.read_log boot in
          if log <> "" then Printf.eprintf "     server log:\n%s\n%!" log;
          false
        | Ok () -> run_suite_exe ~exe:suite.Suites.exe ~args ~env:inst.Instance.suite_env = 0)

(* orchestrate one cut: discover the server + suites, build everything once, then for each
   suite boot a DEDICATED isolated instance, run the suite against it, tear it down. Returns
   the number of failed suites (0 = all passed). Sequential for now — the per-suite isolation
   (distinct ports) makes parallel execution safe to add next. *)
let orchestrate ~(cut : suite) ~dir ~base ~(args : string list) : int =
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
        (* run each suite, accumulating its result in order (List.iter2 is left-to-right, so the
           side effects and the accumulated list stay in suite order) *)
        let results = ref [] in
        List.iter2
          (fun (suite : Suites.t) (inst : Instance.t) ->
            let ok = run_one_suite ~server_exe:d.Discover.exe ~args ~suite ~inst in
            results := { Report.name = suite.Suites.name; port = inst.Instance.port; ok } :: !results)
          suites instances;
        let results = List.rev !results in
        let failed = Report.failures results in
        (* cross-suite footer: one honest roll-up at suite granularity (green iff every suite
           exited 0) — the per-suite check tallies are already printed above by each runner *)
        Printf.printf "\n\027[%sm%s %s\027[0m\n%!"
          (if failed = 0 then "1;32" else "1;31")
          (if failed = 0 then "\u{2714}" else "\u{2717}")
          (Report.summary results);
        failed
    end

let run_http (opts : options) : int = orchestrate ~cut:Http ~dir:"test/http" ~base:opts.base_port ~args:(suite_args ~cut:Http opts)
let run_browser (opts : options) : int = orchestrate ~cut:Browser ~dir:"test/browser" ~base:opts.base_port ~args:(suite_args ~cut:Browser opts)

let run (opts : options) : int =
  match opts.suite with
  | Unit -> run_unit ()
  | Http -> if run_http opts = 0 then 0 else 1
  | Browser -> if run_browser opts = 0 then 0 else 1
  | All ->
    (* fast-to-slow; run every cut and aggregate so one report shows the whole picture (the
       cuts are isolated — a unit failure never poisons http, http never poisons browser) *)
    let u = run_unit () in
    let h = run_http opts in
    let b = run_browser opts in
    if u = 0 && h = 0 && b = 0 then 0 else 1
