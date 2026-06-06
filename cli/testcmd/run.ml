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

(* per-suite wall-clock backstop. Real suites finish well under this — the hunt runners have
   their own per-request / per-step / per-test timeouts — but a wedged suite (e.g. an infinite
   loop in a check body, which no I/O timeout would catch) is killed so the others still run.
   Override with FENNEC_TEST_TIMEOUT=<seconds>. *)
let suite_timeout =
  match Sys.getenv_opt "FENNEC_TEST_TIMEOUT" with
  | Some s -> (match float_of_string_opt s with Some f when f > 0.0 -> f | _ -> 600.0)
  | None -> 600.0

(* run one suite exe against its isolated instance's env. [args] is argv beyond argv[0].
   [out = None] inherits stdout/stderr (live, colour-capable — the serial path). [out = Some fd]
   sends BOTH stdout and stderr there, with stdin from /dev/null, so a parallel run captures the
   suite's whole output to a file (a file, not a pipe, so there's no buffer-fill deadlock).
   Returns the exit code, or [124] if it overran [suite_timeout] and had to be killed. The pid
   is tracked so Ctrl-C tears it down too. *)
let run_suite_exe ~exe ~(args : string list) ~(env : (string * string) list) ~out : int =
  let argv = Array.of_list (exe :: args) and penv = Boot.env_array env in
  let pid, close =
    match out with
    | None -> (Unix.create_process_env exe argv penv Unix.stdin Unix.stdout Unix.stderr, fun () -> ())
    | Some fd ->
      let null = Unix.openfile "/dev/null" [ Unix.O_RDONLY ] 0 in
      (Unix.create_process_env exe argv penv null fd fd, fun () -> try Unix.close null with _ -> ())
  in
  Reaper.track pid;
  Fun.protect
    ~finally:(fun () -> close (); Reaper.untrack pid)
    (fun () ->
      let deadline = Unix.gettimeofday () +. suite_timeout in
      let rec wait () =
        match Unix.waitpid [ Unix.WNOHANG ] pid with
        | 0, _ ->
          if Unix.gettimeofday () > deadline then begin
            (* overran: SIGTERM, give it a moment, then SIGKILL, and reap *)
            (try Unix.kill pid Sys.sigterm with _ -> ());
            Unix.sleepf 0.2;
            (match Unix.waitpid [ Unix.WNOHANG ] pid with
             | 0, _ -> (try Unix.kill pid Sys.sigkill with _ -> ()); (try ignore (Unix.waitpid [] pid) with _ -> ())
             | _ -> ());
            124
          end
          else (Unix.sleepf 0.05; wait ())
        | _, Unix.WEXITED c -> c
        | _, (Unix.WSIGNALED _ | Unix.WSTOPPED _) -> 128
        | exception _ -> 128
      in
      wait ())

let read_file path =
  try
    let ic = open_in_bin path in
    Fun.protect ~finally:(fun () -> close_in_noerr ic) (fun () -> really_input_string ic (in_channel_length ic))
  with _ -> ""

(* one suite end-to-end against a DEDICATED isolated instance; returns [(passed, block)].
   With [~stream:true] the output streams LIVE to stdout/stderr (colour-capable) and [block] is
   ""; with [~stream:false] nothing is printed and [block] is the suite's full self-labelled
   output as one string, for the caller to flush atomically (so parallel suites never tear).
   Before booting we make the port usable, distinguishing the two cases the AirPlay incident
   exposed: a leftover of OURS (reclaim it — SIGKILL, the dev self-heal) vs. a FOREIGN holder
   (name it, never touch it — fail this suite clearly). Teardown is structural (Fun.protect),
   so a failure or exception never orphans the instance. *)
let run_one_suite ~server_exe ~(args : string list) ~stream ~(suite : Suites.t) ~(inst : Instance.t) : bool * string =
  let buf = Buffer.create 256 in
  let emit s = if stream then (print_string s; flush stdout) else Buffer.add_string buf s in
  emit (Printf.sprintf "\n\027[1m\u{25b6} %s\027[0m \027[2m(:%d)\027[0m\n" suite.Suites.name inst.Instance.port);
  let ok =
    match Port.foreign_holder ~exe:server_exe inst.Instance.port with
    | Some (pid, cmd) ->
      emit
        (Printf.sprintf
           "  \027[31m\u{2717} port %d is held by another process (pid %d) \u{2014} not ours, leaving it alone.\027[0m\n     %s\n     free that port, or move the test range with --port.\n"
           inst.Instance.port pid (preview cmd));
      false
    | None ->
      ignore (Port.reclaim ~exe:server_exe inst.Instance.port (* clear a leftover of OURS, if any *));
      let boot = Boot.spawn ~exe:server_exe ~env:inst.Instance.server_env in
      Fun.protect
        ~finally:(fun () -> Boot.stop boot; Boot.cleanup boot)
        (fun () ->
          match Boot.wait_ready boot ~port:inst.Instance.port ~timeout:30.0 with
          | Error msg ->
            emit (Printf.sprintf "  \027[31m\u{2717} instance for %s never came up: %s\027[0m\n" suite.Suites.name msg);
            let log = Boot.read_log boot in
            if log <> "" then emit (Printf.sprintf "     server log:\n%s\n" log);
            false
          | Ok () ->
            let code =
              if stream then run_suite_exe ~exe:suite.Suites.exe ~args ~env:inst.Instance.suite_env ~out:None
              else begin
                let tmp = Filename.temp_file "fennec-suite-" ".log" in
                let fd = Unix.openfile tmp [ Unix.O_WRONLY; Unix.O_TRUNC ] 0o600 in
                let code =
                  Fun.protect
                    ~finally:(fun () -> try Unix.close fd with _ -> ())
                    (fun () -> run_suite_exe ~exe:suite.Suites.exe ~args ~env:inst.Instance.suite_env ~out:(Some fd))
                in
                emit (read_file tmp);
                (try Sys.remove tmp with _ -> ());
                code
              end
            in
            if code = 124 then
              emit (Printf.sprintf "  \027[31m\u{2717} suite timed out after %.0fs — killed; other suites still run.\027[0m\n" suite_timeout);
            code = 0)
  in
  (ok, Buffer.contents buf)

(* orchestrate one cut: discover the server + suites, build everything once, then run each
   suite against its OWN dedicated isolated instance (boot → wait → run → tear down). With
   [jobs > 1] and more than one suite, suites run concurrently (≤ [jobs] at a time) — safe
   precisely because each has its own port + server — and each suite's output is captured and
   flushed as one atomic block, so nothing interleaves. Serial runs stream live, in order.
   Returns the number of failed suites (0 = all passed). *)
let orchestrate ~(cut : suite) ~dir ~base ~jobs ~(args : string list) : int =
  ignore (Sys.command "dune shutdown >/dev/null 2>&1"); (* stop any orphaned dev watcher → no lock clash *)
  match Discover.find () with
  | Error msg -> Printf.eprintf "fennec test: %s\n%!" msg; 1
  | Ok d ->
    let cwd = Sys.getcwd () in
    let suites = Suites.discover ~root:d.Discover.root ~cwd ~dir in
    if suites = [] then (
      Printf.printf "fennec test: no %s suites found (looked in %s)\n%!" (suite_to_string cut) (Filename.concat cwd dir);
      0 (* nothing to run is not a failure *))
    else
      (* build the server, its webroot, and the suites in one dune invocation — fennec is the
         sole dune-aware process, so no nested-build lock deadlock. Targets are root-relative,
         so build from the workspace root, then RESTORE the cwd: `fennec test all` runs
         orchestrate once per cut, and a lingering chdir would make the next cut look for its
         suites under the wrong directory. *)
      Fun.protect ~finally:(fun () -> try Sys.chdir cwd with _ -> ()) @@ fun () ->
      Sys.chdir d.Discover.root;
      let webroot = Filename.concat d.Discover.src_dir "webroot" in
      let app_targets = d.Discover.targets @ [ webroot ] in
      let build_cmd = "dune build " ^ String.concat " " (List.map Filename.quote (app_targets @ List.map (fun (s : Suites.t) -> s.target) suites)) in
      match Sys.command build_cmd with
      | n when n <> 0 -> Printf.eprintf "fennec test: `dune build` failed (exit %d) — see the errors above\n%!" n; 1
      | _ ->
        let instances = Instance.allocate ~base (List.map (fun (s : Suites.t) -> s.Suites.name) suites) in
        let pairs = List.combine suites instances in
        let n = List.length pairs in
        let jobs = max 1 (min jobs n) in (* never spawn more workers than there are suites *)
        let stream = jobs <= 1 in (* serial → live, colour-capable; parallel → atomic blocks *)
        if not stream then Printf.printf "running %d suites, up to %d at a time\n%!" n jobs;
        let pm = Mutex.create () in
        (* Pool.map preserves input order, so [results] stay in suite order for the footer *)
        let results =
          Pool.map ~jobs
            (fun ((suite : Suites.t), (inst : Instance.t)) ->
              let ok, block = run_one_suite ~server_exe:d.Discover.exe ~args ~stream ~suite ~inst in
              if not stream then (Mutex.lock pm; print_string block; flush stdout; Mutex.unlock pm);
              { Report.name = suite.Suites.name; port = inst.Instance.port; ok })
            pairs
        in
        let failed = Report.failures results in
        (* cross-suite footer: one honest roll-up at suite granularity (green iff every suite
           exited 0) — the per-suite check tallies are already printed above by each runner *)
        Printf.printf "\n\027[%sm%s %s\027[0m\n%!"
          (if failed = 0 then "1;32" else "1;31")
          (if failed = 0 then "\u{2714}" else "\u{2717}")
          (Report.summary results);
        failed

(* parallel suites: explicit -j wins, else default to the machine's CPU count (suites are
   isolated, so this is safe); the orchestrator clamps it to the suite count *)
let effective_jobs (o : options) =
  match o.jobs with Some j -> max 1 j | None -> max 1 (Domain.recommended_domain_count ())

let run_http (opts : options) : int =
  orchestrate ~cut:Http ~dir:"test/http" ~base:opts.base_port ~jobs:(effective_jobs opts) ~args:(suite_args ~cut:Http opts)

let run_browser (opts : options) : int =
  orchestrate ~cut:Browser ~dir:"test/browser" ~base:opts.base_port ~jobs:(effective_jobs opts) ~args:(suite_args ~cut:Browser opts)

let run (opts : options) : int =
  Reaper.install_signal_handlers (); (* Ctrl-C / SIGTERM → tear down every spawned instance, no orphans *)
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
