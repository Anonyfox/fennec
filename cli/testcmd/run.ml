(* The `fennec test` entry point: parse the cut + options, dispatch.

   - unit    → delegate to dune's fast gate (`dune build @runtest`)
   - http    → orchestrate the Http suites (per-suite isolated app instance)  [T6]
   - browser → orchestrate the Browser suites (+ Chrome)                       [T7]
   - system  → orchestrate the System suites: each SPAWNS the real `fennec dev` itself and drives
               its process/port/livereload lifecycle, so (unlike http/browser) there is no
               per-suite server to boot — we just build, set FENNEC_* env, and run them serially
               (they share dev's fixed ports). Replaces the old e2e/*.sh scripts.
   - all     → unit, then http, then browser, then system (fast-to-slow)

   The pure parts (the cut enum + its parsing, the options record) are unit-tested; the
   orchestration is integration-tested against the example app. *)

type suite = Unit | Http | Browser | System | All

let suite_of_string s =
  match String.lowercase_ascii s with
  | "unit" -> Ok Unit
  | "http" -> Ok Http
  | "browser" -> Ok Browser
  | "system" -> Ok System
  | "all" -> Ok All
  | other -> Error (Printf.sprintf "unknown test suite %S — expected one of: unit, http, browser, system, all" other)

let suite_to_string = function Unit -> "unit" | Http -> "http" | Browser -> "browser" | System -> "system" | All -> "all"

type options = {
  suite : suite;
  grep : string option;       (* narrow to matching cases (passed through to the runner) *)
  max_failures : int option;  (* stop after N suites fail; None + fail_fast = stop at the first *)
  fail_fast : bool;           (* default true; --no-fail-fast runs every suite regardless *)
  reporter : string option;   (* e.g. "list", "list,junit" — browser cut *)
  jobs : int option;          (* parallel suites; None = CPUs *)
  headed : bool;              (* browser cut: show the window *)
  screenshots : string option;(* browser cut: PNG-on-failure dir *)
  base_port : int;            (* per-suite instance block base *)
}

let default_options =
  (* base 8200: clear of dev's 4000 AND of macOS's port 7000 (AirPlay/ControlCenter) *)
  { suite = Unit; grep = None; max_failures = None; fail_fast = true;
    reporter = None; jobs = None; headed = false; screenshots = None; base_port = 8200 }

(* how many suite failures to tolerate before we stop launching the rest. An explicit
   --max-failures wins; otherwise fail-fast stops at the first failure and --no-fail-fast
   (fail_fast = false) runs everything. Pure. *)
let fail_fast_limit ~fail_fast ~max_failures =
  match max_failures with Some n -> max 1 n | None -> if fail_fast then 1 else max_int

module Discover = Fennec_dev.Discover
module Port = Fennec_dev.Port

(* one-line preview of a (possibly long, possibly multi-line) command for diagnostics *)
let preview ?(max = 120) s =
  let s = String.map (fun c -> if c = '\n' || c = '\t' then ' ' else c) s in
  if String.length s <= max then s else String.sub s 0 (max - 1) ^ "\u{2026}"

(* walk up from [dir] looking for a [name] marker (e.g. dune-project) *)
let rec find_up name dir =
  if Sys.file_exists (Filename.concat dir name) then true
  else
    let parent = Filename.dirname dir in
    if parent = dir then false else find_up name parent

(* the fast gate: dune already builds + runs every @runtest test and returns the right exit
   code. fennec is the only dune-aware process here (no nested watcher), so this is safe. We
   pre-check for a dune project so the common "ran it in the wrong directory" mistake gets a
   fennec-flavored hint instead of dune's raw "cannot find root / dune init project NAME". *)
let run_unit () =
  if not (find_up "dune-project" (Sys.getcwd ())) then (
    Printf.eprintf "fennec test: not inside a project (no dune-project found here or above). Run it from your Fennec app.\n%!";
    1)
  else Sys.command "dune build @runtest"

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
  | System -> grep_args o   (* the system runner honours --grep (substring on the scenario name) *)
  | Unit | All -> []        (* unit goes through dune *)

(* ──── suite_of_string tests ──── *)

let%test "unit" = suite_of_string "unit" = Ok Unit
let%test "http" = suite_of_string "http" = Ok Http
let%test "browser" = suite_of_string "browser" = Ok Browser
let%test "system" = suite_of_string "system" = Ok System
let%test "all" = suite_of_string "all" = Ok All
let%test "case-insensitive" = suite_of_string "HTTP" = Ok Http
let%test "unknown -> error naming the valid set" =
  match suite_of_string "bogus" with Error m -> Fennec_hunt_unit.str_contains m "unit, http, browser, system, all" | Ok _ -> false
let%test "round-trips" =
  List.for_all (fun s -> suite_of_string (suite_to_string s) = Ok s) [ Unit; Http; Browser; System; All ]

(* ──── default_options tests ──── *)

let%test "default is the fast unit cut" = default_options.suite = Unit
let%test "default fail-fast on" = default_options.fail_fast = true
let%test "default base port clears dev's 4000" = default_options.base_port >= 7000

(* ──── suite_args tests ──── *)

let%test "browser: --headed only when set" =
  suite_args ~cut:Browser { default_options with headed = true } = [ "--headed" ]
let%test "browser: no flags by default" =
  suite_args ~cut:Browser default_options = []
let%test "browser: grep passes through" =
  suite_args ~cut:Browser { default_options with grep = Some "checkout" } = [ "--grep"; "checkout" ]
let%test "browser: screenshots dir passes through" =
  suite_args ~cut:Browser { default_options with screenshots = Some "shots" } = [ "--screenshots"; "shots" ]
let%test "browser: jobs + reporter pass through" =
  suite_args ~cut:Browser { default_options with jobs = Some 3; reporter = Some "plain" } = [ "--jobs"; "3"; "--reporter"; "plain" ]
let%test "browser: stable flag order" =
  suite_args ~cut:Browser { default_options with grep = Some "g"; headed = true; screenshots = Some "d"; jobs = Some 2; reporter = Some "pretty" }
  = [ "--grep"; "g"; "--headed"; "--screenshots"; "d"; "--jobs"; "2"; "--reporter"; "pretty" ]
let%test "http: grep passes through, browser-only flags don't" =
  suite_args ~cut:Http { default_options with grep = Some "x"; headed = true; screenshots = Some "d" } = [ "--grep"; "x" ]
let%test "http: no grep -> no argv" =
  suite_args ~cut:Http default_options = []
let%test "unit: no argv" =
  suite_args ~cut:Unit { default_options with grep = Some "x" } = []
let%test "system: grep passes through, browser-only flags don't" =
  suite_args ~cut:System { default_options with grep = Some "x"; headed = true } = [ "--grep"; "x" ]
let%test "system: no grep -> no argv" =
  suite_args ~cut:System default_options = []
let%test "all: no argv (dispatches per-cut)" =
  suite_args ~cut:All { default_options with headed = true } = []

(* ──── fail_fast_limit tests ──── *)

let%test "default fail-fast -> limit 1" = fail_fast_limit ~fail_fast:true ~max_failures:None = 1
let%test "no-fail-fast -> unbounded" = fail_fast_limit ~fail_fast:false ~max_failures:None = max_int
let%test "explicit -x wins over fail-fast" = fail_fast_limit ~fail_fast:true ~max_failures:(Some 3) = 3
let%test "explicit -x wins over no-fail-fast" = fail_fast_limit ~fail_fast:false ~max_failures:(Some 2) = 2
let%test "-x 0 floored to 1" = fail_fast_limit ~fail_fast:true ~max_failures:(Some 0) = 1

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
(* the per-suite exit code: 0 = passed, 3 = the runner had no test matching --grep in THIS suite
   file (not a failure — the filter likely targets another file), anything else = failed. *)
let run_one_suite ~server_exe ~runner_exe ~(args : string list) ~stream ~(suite : Suites.t) ~(inst : Instance.t) : int * string =
  (* one shared runner exe per cut; restrict it to THIS suite's source file so it runs only that
     file's tests — against this suite's own dedicated instance (per-file isolation preserved). *)
  let args = "--only-file" :: suite.Suites.name :: args in
  let buf = Buffer.create 256 in
  let emit s = if stream then (print_string s; flush stdout) else Buffer.add_string buf s in
  emit (Printf.sprintf "\n\027[1m\u{25b6} %s\027[0m \027[2m(:%d)\027[0m\n" suite.Suites.name inst.Instance.port);
  let code =
    match Port.foreign_holder ~exe:server_exe inst.Instance.port with
    | Some (pid, cmd) ->
      emit
        (Printf.sprintf
           "  \027[31m\u{2717} port %d is held by another process (pid %d) \u{2014} not ours, leaving it alone.\027[0m\n     %s\n     free that port, or move the test range with --port.\n"
           inst.Instance.port pid (preview cmd));
      1
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
            1
          | Ok () ->
            let code =
              if stream then run_suite_exe ~exe:runner_exe ~args ~env:inst.Instance.suite_env ~out:None
              else begin
                let tmp = Filename.temp_file "fennec-suite-" ".log" in
                let fd = Unix.openfile tmp [ Unix.O_WRONLY; Unix.O_TRUNC ] 0o600 in
                let code =
                  Fun.protect
                    ~finally:(fun () -> try Unix.close fd with _ -> ())
                    (fun () -> run_suite_exe ~exe:runner_exe ~args ~env:inst.Instance.suite_env ~out:(Some fd))
                in
                emit (read_file tmp);
                (try Sys.remove tmp with _ -> ());
                code
              end
            in
            if code = 124 then
              emit (Printf.sprintf "  \027[31m\u{2717} suite timed out after %.0fs — killed; other suites still run.\027[0m\n" suite_timeout);
            code)
  in
  (code, Buffer.contents buf)

(* orchestrate one cut: discover the server + suites, build everything once, then run each
   suite against its OWN dedicated isolated instance (boot → wait → run → tear down). With
   [jobs > 1] and more than one suite, suites run concurrently (≤ [jobs] at a time) — safe
   precisely because each has its own port + server — and each suite's output is captured and
   flushed as one atomic block, so nothing interleaves. Serial runs stream live, in order.
   Returns the number of failed suites (0 = all passed). *)
let orchestrate ~(cut : suite) ~dir ~base ~jobs ~limit ~(args : string list) : int =
  ignore (Sys.command "dune shutdown >/dev/null 2>&1"); (* stop any orphaned dev watcher → no lock clash *)
  (* the per-suite instance is the bytecode server, which must dlopen its C stubs — `opam env`
     doesn't put them on CAML_LD_LIBRARY_PATH. Reuse fennec dev's fix (same as the system cut). *)
  Fennec_dev.Supervisor.ensure_stublibs ();
  match Discover.find () with
  | Error msg -> Printf.eprintf "fennec test: %s\n%!" msg; 1
  | Ok d ->
    let cwd = Sys.getcwd () in
    (* suites = the source files in the cut dir; we still discover them per-file (for per-suite
       instance allocation + the --only-file token), but they all compile into ONE runner exe. *)
    let suites = Suites.discover ~root:d.Discover.root ~cwd ~dir in
    if suites = [] then (
      Printf.printf "fennec test: no %s suites found (looked in %s)\n%!" (suite_to_string cut) (Filename.concat cwd dir);
      0 (* nothing to run is not a failure *))
    else
      let reldir = Suites.relativize ~root:d.Discover.root ~cwd in
      let runner_exe = Suites.exe_path ~root:d.Discover.root ~reldir ~dir ~name:"run" in
      let runner_target = Suites.build_target ~reldir ~dir ~name:"run" in
      (* build the server, its webroot, and the cut's single runner in one dune invocation — fennec
         is the sole dune-aware process, so no nested-build lock deadlock. Targets are root-relative,
         so build from the workspace root, then RESTORE the cwd: `fennec test all` runs orchestrate
         once per cut, and a lingering chdir would make the next cut look under the wrong directory. *)
      Fun.protect ~finally:(fun () -> try Sys.chdir cwd with _ -> ()) @@ fun () ->
      Sys.chdir d.Discover.root;
      let webroot = Filename.concat d.Discover.src_dir "webroot" in
      let app_targets = d.Discover.targets @ [ webroot ] in
      let build_cmd = "dune build " ^ String.concat " " (List.map Filename.quote (app_targets @ [ runner_target ])) in
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
        (* fail-fast: once [limit] suites have failed, suites not yet started skip (in parallel,
           those already in flight finish — we can't un-start them). The counter is mutex-guarded. *)
        let fmx = Mutex.create () and failed_so_far = ref 0 in
        let over_limit () = Mutex.lock fmx; let f = !failed_so_far in Mutex.unlock fmx; f >= limit in
        let bump () = Mutex.lock fmx; incr failed_so_far; Mutex.unlock fmx in
        (* Pool.map preserves input order, so [results] stay in suite order for the footer *)
        let outcomes =
          Pool.map ~jobs
            (fun ((suite : Suites.t), (inst : Instance.t)) ->
              if over_limit () then None (* fail-fast reached — don't boot this one *)
              else begin
                let code, block = run_one_suite ~server_exe:d.Discover.exe ~runner_exe ~args ~stream ~suite ~inst in
                if code <> 0 && code <> 3 then bump (); (* only real failures count toward fail-fast *)
                if not stream then (Mutex.lock pm; print_string block; flush stdout; Mutex.unlock pm);
                Some (suite.Suites.name, inst.Instance.port, code)
              end)
            pairs
        in
        let results = List.filter_map Fun.id outcomes in
        let skipped = n - List.length results in
        (* a suite that exited 3 ran nothing here — its --grep matched no suite in that file (the
           filter likely targets another file). Such suites are neither pass nor fail. *)
        let ran = List.filter (fun (_, _, c) -> c <> 3) results in
        let no_match_here = List.length results - List.length ran in
        let grep_value = let rec f = function "--grep" :: v :: _ -> Some v | _ :: r -> f r | [] -> None in f args in
        if grep_value <> None && ran = [] then (
          (* a --grep that matched nothing in ANY suite — never a silent green *)
          Printf.printf "\n\027[1;31m\u{2717} no tests matched --grep %s\027[0m\n%!" (Option.value grep_value ~default:"");
          1)
        else begin
          let reports = List.map (fun (name, port, c) -> { Report.name; port; ok = c = 0 }) ran in
          let failed = Report.failures reports in
          (* cross-suite footer: green iff every suite that ran exited 0; the per-suite check
             tallies are already printed above by each runner *)
          let tail =
            (if skipped > 0 then Printf.sprintf " \027[2m(%d skipped after fail-fast)\027[0m" skipped else "")
            ^ (if no_match_here > 0 then Printf.sprintf " \027[2m(%d file(s) with no --grep match)\027[0m" no_match_here else "")
          in
          Printf.printf "\n\027[%sm%s %s\027[0m%s\n%!"
            (if failed = 0 then "1;32" else "1;31")
            (if failed = 0 then "\u{2714}" else "\u{2717}")
            (Report.summary reports) tail;
          failed
        end

(* parallel suites: explicit -j wins, else default to the machine's CPU count (suites are
   isolated, so this is safe); the orchestrator clamps it to the suite count *)
let effective_jobs (o : options) =
  match o.jobs with Some j -> max 1 j | None -> max 1 (Domain.recommended_domain_count ())

let run_http (opts : options) : int =
  orchestrate ~cut:Http ~dir:"test/http" ~base:opts.base_port ~jobs:(effective_jobs opts)
    ~limit:(fail_fast_limit ~fail_fast:opts.fail_fast ~max_failures:opts.max_failures) ~args:(suite_args ~cut:Http opts)

let run_browser (opts : options) : int =
  orchestrate ~cut:Browser ~dir:"test/browser" ~base:opts.base_port ~jobs:(effective_jobs opts)
    ~limit:(fail_fast_limit ~fail_fast:opts.fail_fast ~max_failures:opts.max_failures) ~args:(suite_args ~cut:Browser opts)

(* ──── system cut ──── *)

(* The system cut. Unlike http/browser, the suites SPAWN the real `fennec dev` themselves (the
   System layer puts each process in its own session and reaps the whole group on teardown), so
   there is no per-suite server to boot and no per-suite isolation to arrange. The suites compile
   into ONE runner — [test/system/run.exe] (a [-linkall] library of [let%system] modules + a
   one-line entry) — so we build the server + webroot + that runner once, then run it ONCE with the
   harness env. It executes every registered scenario serially (they share dev's fixed ports),
   honours [--grep], and skips [@manual] scenarios unless [--manual] is passed. The harness contract
   (the env_test_ fields of Fennec_core.Dev_proto): FENNEC_BIN (this binary), FENNEC_APP_DIR (the
   cwd to run `fennec dev` in), FENNEC_SERVER_BC (the built server, for leftover-reclaim), FENNEC_ROOT.
   Returns the runner's exit code (0 = all passed). *)
let orchestrate_system ~(args : string list) : int =
  ignore (Sys.command "dune shutdown >/dev/null 2>&1"); (* stop any orphaned watcher → no lock clash *)
  (* a suite may spawn the bytecode server DIRECTLY (the leftover-reclaim scenario), and a .bc must
     dlopen its C stubs — which `opam env` does NOT put on CAML_LD_LIBRARY_PATH. Reuse fennec dev's
     own fix so the runner inherits it and propagates it to anything it spawns. *)
  Fennec_dev.Supervisor.ensure_stublibs ();
  match Discover.find () with
  | Error msg -> Printf.eprintf "fennec test: %s\n%!" msg; 1
  | Ok d ->
    let cwd = Sys.getcwd () in
    let dir = "test/system" in
    if not (try Sys.is_directory (Filename.concat cwd dir) with _ -> false) then (
      Printf.printf "fennec test: no system suites found (looked in %s)\n%!" (Filename.concat cwd dir);
      0 (* nothing to run is not a failure *))
    else
      let reldir = Suites.relativize ~root:d.Discover.root ~cwd in
      let runner_exe = Suites.exe_path ~root:d.Discover.root ~reldir ~dir ~name:"run" in
      let runner_target = Suites.build_target ~reldir ~dir ~name:"run" in
      (* build from the workspace root (targets are root-relative), then RESTORE cwd: the runner
         runs with cwd = the app dir (= FENNEC_APP_DIR), and `fennec test all` reuses cwd later. *)
      Fun.protect ~finally:(fun () -> try Sys.chdir cwd with _ -> ()) @@ fun () ->
      Sys.chdir d.Discover.root;
      let webroot = Filename.concat d.Discover.src_dir "webroot" in
      let targets = d.Discover.targets @ [ webroot; runner_target ] in
      let build_cmd = "dune build " ^ String.concat " " (List.map Filename.quote targets) in
      (match Sys.command build_cmd with
       | n when n <> 0 -> Printf.eprintf "fennec test: `dune build` failed (exit %d) — see the errors above\n%!" n; 1
       | _ ->
         Sys.chdir cwd; (* run from the app dir (Fun.protect restores either way) *)
         let fennec_bin =
           let e = Sys.executable_name in
           if Filename.is_relative e then Filename.concat cwd e else e
         in
         let module D = Fennec_core.Dev_proto in
         let env =
           [ (D.env_test_bin, fennec_bin);
             (D.env_test_app_dir, cwd);
             (D.env_test_server_bc, d.Discover.exe);
             (D.env_test_root, d.Discover.root) ]
         in
         Printf.printf "\n\027[1m\u{25b6} system\027[0m\n%!";
         let code = run_suite_exe ~exe:runner_exe ~args ~env ~out:None in
         if code = 124 then
           Printf.printf "  \027[31m\u{2717} system suites timed out after %.0fs — killed.\027[0m\n%!" suite_timeout;
         code)

let run_system (opts : options) : int = orchestrate_system ~args:(suite_args ~cut:System opts)

(* `fennec test new <cut> <name>` — scaffold a suite. [args] is the positionals after "new". *)
let scaffold (args : string list) : int =
  match args with
  | cut :: name :: _ ->
    (match Scaffold.create ~cwd:(Sys.getcwd ()) ~cut ~name with
     | Ok created ->
       List.iter (fun f -> Printf.printf "  \027[32m+\027[0m %s\n" f) created;
       Printf.printf "\n\027[2mrun it:\027[0m fennec test %s\n%!" cut;
       0
     | Error m -> Printf.eprintf "fennec test new: %s\n%!" m; 1)
  | _ ->
    Printf.eprintf "usage: fennec test new <cut> <name>   (cut: %s)\n%!" (String.concat ", " Scaffold.cuts);
    1

let run (opts : options) : int =
  Reaper.install_signal_handlers (); (* Ctrl-C / SIGTERM → tear down every spawned instance, no orphans *)
  match opts.suite with
  | Unit -> run_unit ()
  | Http -> if run_http opts = 0 then 0 else 1
  | Browser -> if run_browser opts = 0 then 0 else 1
  | System -> if run_system opts = 0 then 0 else 1
  | All ->
    (* fast-to-slow; run every cut and aggregate so one report shows the whole picture (the cuts
       are isolated — a unit failure never poisons http, etc.). System is last: it spawns the real
       `fennec dev` per suite, the heaviest tier. *)
    let u = run_unit () in
    let h = run_http opts in
    let b = run_browser opts in
    let s = run_system opts in
    if u = 0 && h = 0 && b = 0 && s = 0 then 0 else 1
