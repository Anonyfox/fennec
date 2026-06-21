(* See supervisor.mli. The loop reacts to one {!Dune_watch} event at a time:

     - a successful settle whose backend artifact changed -> restart the server;
     - a successful settle that only touched the web root  -> CSS hot-swap / reload ping;
     - a failed settle -> keep the last good server, print the diagnostics;
     - dune itself exiting -> respawn it;
     - (when idle) the server dying on its own -> rate-limited restart.

   It is TOTAL: every iteration is exception-wrapped, so no syscall, dead child, or parser
   surprise can take the process down. *)

module Dev_proto = Paw.Dev_proto (* the shared CLI<->server wire (env names, stderr line formats, exit code) *)

let ef = Printf.eprintf (* last-resort raw stderr (preflight + the total-loop guard) *)
let now () = Unix.gettimeofday ()
let mtime path = try (Unix.stat path).Unix.st_mtime with _ -> 0.0

(* C-stub loading for directly-spawned bytecode servers (CAML_LD_LIBRARY_PATH) lives in {!Stublibs};
   it is called at SPAWN time (post-build) so the per-lib dll dirs exist to be found. *)

(* ---- child processes ---- *)
(* the server child (spawn, drain, reap, graceful stop) lives in {!Server_proc}; here we only need
   a plain SIGTERM for the OTHER children — dune --watch and the esbuild worker — on shutdown *)
let kill pid = try Unix.kill pid Sys.sigterm with _ -> ()

(* THE supervised server, as a sum type so illegal states are unrepresentable: [Down] (no process,
   hence no dangling pipe and no stale port), or [Up] carrying the live process bundled with the
   facts that belong to THAT instance — [busy_port] (an EADDRINUSE it reported, which therefore
   can't outlive it) and [last_exe] (the artifact mtime it was built from). *)
type running = { proc : Server_proc.t; mutable busy_port : int option; last_exe : float }
type server = Down | Up of running

(* a short, unique unix-socket path in the temp dir (socket paths cap ~100 bytes) *)
let tmp_socket prefix =
  let name = Printf.sprintf "%s-%d.sock" prefix (Unix.getpid ()) in
  let p = Filename.concat (Filename.get_temp_dir_name ()) name in
  if String.length p <= 100 then p else Filename.concat "/tmp" name

let start_esbuild_worker socket =
  let exe = Sys.executable_name in
  Unix.create_process exe [| exe; "__esbuild-worker"; socket |] Unix.stdin Unix.stderr Unix.stderr

(* push one frame to the server's dev control socket (best-effort) *)
let ping control_path frame =
  try
    let fd = Unix.socket Unix.PF_UNIX Unix.SOCK_STREAM 0 in
    Fun.protect
      ~finally:(fun () -> try Unix.close fd with _ -> ())
      (fun () ->
        Unix.connect fd (Unix.ADDR_UNIX control_path);
        let msg = frame ^ "\n" in
        ignore (Unix.write_substring fd msg 0 (String.length msg)))
  with _ -> ()

let run ?port ?agent_dir ~targets ~exe ~assets =
  if not (Sys.file_exists "dune-project") then (ef "fennec dev: run from a dune project root (no dune-project here)\n"; exit 1);
  let ui = Ui.create () in
  Ui.start ui ~dir:(match targets with t :: _ -> Filename.dirname t | [] -> ".");
  (* Per-profile build dir (cargo-style): export DUNE_BUILD_DIR BEFORE any dune runs, so the dev loop's
     `fastdev` artifacts land in their own `_build/fastdev` and never invalidate `dune build`/`fennec
     test`'s `_build/default` (and vice-versa). The server exe (+ its sibling assets dir) is re-pointed
     to the same root. See {!Build_dir}. *)
  let root = Sys.getcwd () in
  (* boot phase timing (FENNEC_DEV_DEBUG=1) — answers "where do the seconds before READY go" *)
  let boot_t = now () in
  let boot_debug = match Sys.getenv_opt "FENNEC_DEV_DEBUG" with Some ("1" | "on" | "true" | "yes") -> true | _ -> false in
  let mark name = if boot_debug then ef "[fennec dev:boot] %-14s +%6.0f ms\n%!" name ((now () -. boot_t) *. 1000.) in
  Build_dir.export ~root;
  let exe = Build_dir.retarget ~root exe in
  Stublibs.ensure ();
  mark "stublibs";

  (* ═══════════ boot: children (esbuild + warm test worker), dune --watch, backend, agent journal ═══════════ *)

  (* warm esbuild worker BEFORE dune, so even the first build delegates to it *)
  let worker_socket = tmp_socket "fennec-esb" in
  let worker_pid = start_esbuild_worker worker_socket in
  let ww = ref 0.0 and worker_dead = ref false in
  while (not (Sys.file_exists worker_socket)) && !ww < 3.0 && not !worker_dead do
    Unix.sleepf 0.05;
    ww := !ww +. 0.05;
    (* if the worker exited (bad binary), stop waiting for a socket that will never appear
       instead of burning the full 3s of dead startup latency *)
    match (try Unix.waitpid [ Unix.WNOHANG ] worker_pid with _ -> (0, Unix.WEXITED 0)) with 0, _ -> () | _ -> worker_dead := true
  done;
  if Sys.file_exists worker_socket then Unix.putenv Dev_proto.env_esbuild_worker worker_socket
  else Ui.notice ui Ui.Info "esbuild worker did not start — falling back to cold builds";
  mark "esbuild-worker";

  (* Unit feedback in the dev loop. Discover colocated inline-test runners plus conventional
     *_test dune test executables under the watched app root, build them as watch targets, then
     run only the executables whose mtimes advanced after each green settle. *)
  let watch_roots =
    let root_of_target target =
      if String.length target > 1 && target.[0] = '@' then
        let body = String.sub target 1 (String.length target - 1) in
        match String.split_on_char '/' body with
        | [] | [ "" ] -> None
        | [ one ] -> Some one
        | parts -> Some (String.concat "/" (List.rev (List.tl (List.rev parts))))
      else Some (Filename.dirname target)
    in
    targets |> List.filter_map root_of_target |> List.filter (fun s -> s <> "." && s <> "")
  in
  let dev_tests = Dev_tests.create ~root:(Sys.getcwd ()) ~watch_roots () in
  let initial_test_targets = Dev_tests.targets dev_tests in
  Dev_tests.prime dev_tests;
  let tests_wired = ref (initial_test_targets <> []) in

  (* The WARM TEST WORKER: a resident bytecode process that Dynlinks the freshly compiled app objects
     per edit instead of cold-launching a relinked native test exe. Stublibs.ensure ran above, so the
     worker inherits CAML_LD_LIBRARY_PATH for its C stubs. If the worker binary isn't present or won't
     start, [spawn] is None and the dev loop silently stays on the cold path (Dev_tests.run_one). *)
  let test_worker = ref None in
  let start_test_worker () =
    match Test_worker.spawn ~root:(Sys.getcwd ()) () with
    | Some w -> test_worker := Some w; Dev_tests.set_worker dev_tests (Some w)
    | None -> test_worker := None; Dev_tests.set_worker dev_tests None
  in
  let stop_test_worker () =
    (match !test_worker with Some w -> Test_worker.shutdown w | None -> ());
    test_worker := None;
    Dev_tests.set_worker dev_tests None
  in
  (* a framework edit (a trigger OUTSIDE the watched app roots) means the worker's preloaded image is
     stale; restart it so subsequent runs are warm again (a stale worker would otherwise fall back via
     a Dynlink digest mismatch — correct, just cold). Also re-derive chains (the lib graph may shift). *)
  let restart_test_worker () = stop_test_worker (); Dev_tests.invalidate_chains dev_tests; start_test_worker () in
  (* Build the worker in the dev profile NOW — lock-free, before the watch below takes the build lock —
     so its preloaded framework matches, CRC-wise, the app .cma's the watch builds in the SAME profile.
     A profile skew is exactly what makes the worker's Dynlink reject with "interface mismatch on …".
     Best-effort + repo-only: downstream uses the prebuilt fennec-cli worker, and a mismatch there just
     falls back to cold via the worker's load-failed sentinel. (Same default as Dune_watch's profile.) *)
  let dev_profile = Build_dir.profile () in
  (try ignore (Sys.command (Printf.sprintf "dune build --profile %s cli/dev/worker/fennec_test_worker.bc >/dev/null 2>&1" dev_profile)) with _ -> ());
  mark "worker-build";
  start_test_worker ();
  mark "worker-spawn";
  (* with a worker active, the watch must also build the bytecode test targets (.bc → the test .cmo +
     app .cma's) so a settle has them ready for the worker to load *)
  let warm_targets () = if !test_worker <> None then Dev_tests.byte_targets dev_tests else [] in

  (* prime `dune describe` NOW — the last lock-free moment. Once the watch (next line) holds the build
     lock, `dune describe` errors ("build directory is locked"), so the warm worker could never derive
     a test's app chain and every test would fall back to cold. Best-effort; safe if it fails. *)
  Dev_tests.prime_describe dev_tests;
  mark "describe";
  let dw = ref (Dune_watch.start (targets @ initial_test_targets @ warm_targets ())) in
  mark "watch-start";
  let control_path = tmp_socket "fennec-lr" in
  (* the dev port base: --port if given, else 4000 (the server's own default). Set FENNEC_PORT
     explicitly so the supervisor and server agree, and so the banner can show the gateway URL. *)
  let dev_base = Option.value port ~default:4000 in
  let mongo = Mongo_rs.ensure_dev ~root:(Sys.getcwd ()) ~base_port:dev_base () in
  mark "mongo-backend";
  let agent =
    match agent_dir with
    | None -> None
    | Some dir -> Some (Agent_event.start ~dir ~port:dev_base ~root:(Sys.getcwd ()) ())
  in
  let emit_verdict verdict =
    match agent with None -> () | Some a -> Agent_event.emit_verdict a verdict
  in
  let agent_ready_sent = ref false in
  let pending_agent_success = ref None in
  let gateway_url = Printf.sprintf "http://localhost:%d" dev_base in
  (* FENNEC_DEV_PARENT lets the server watch THIS supervisor and self-exit if we die (even on
     SIGKILL), so it can never be left holding the dev port. FENNEC_DEV_UI asks the server to
     report its named dev URLs (a [fennec:urls] line) so the supervisor owns the clickable URLs. *)
  let dev_env =
    [| Dev_proto.env_mode ^ "=development";
       Dev_proto.env_livereload ^ "=" ^ control_path;
       Dev_proto.env_dev_parent ^ "=" ^ string_of_int (Unix.getpid ());
       Dev_proto.env_dev_ui ^ "=1";
       Dev_proto.env_port ^ "=" ^ string_of_int dev_base |]
  in
  let server = ref Down in
  (* [last_build_ms] is the MOST RECENT build's duration (shown in the ready banner) — a
     process-level fact, kept separate from [Up] because a crash-restart still needs it when no
     build just ran. The per-instance facts (busy_port, last_exe) live in [Up]. *)
  let last_build_ms = ref None in
  let assets = Assets.create ~dir:(Filename.concat (Filename.dirname exe) assets) in
  let limiter = Crash_limiter.create () in
  let dune_exits = ref 0 in
  (* reset to 0 on every good build *)
  let pidfile = Pidfile.path_for ~root:(Sys.getcwd ()) in
  (* record OUR pid too, not just the children: a previous `fennec dev` that wasn't shut down
     cleanly would otherwise keep supervising (respawning a server that fights for the port).
     The next run kills it via this pid, and its server then self-exits (getppid), freeing the
     port — so two `fennec dev`s can't coexist and serve a stale build. *)
  let record_pids () =
    let mongo_pid = match mongo with Some t -> Option.to_list (Mongo_rs.pid t) | None -> [] in
    let server_pid = match !server with Up up -> [ Server_proc.pid up.proc ] | Down -> [] in
    let test_worker_pid = match !test_worker with Some w -> [ Test_worker.pid w ] | None -> [] in
    Pidfile.record pidfile
      ([ Unix.getpid (); Dune_watch.pid !dw; worker_pid ] @ test_worker_pid @ mongo_pid @ server_pid)
  in
  let serving () = match !server with Up _ -> true | Down -> false in

  (* ═══════════ reacting to the server child's output ═══════════ *)

  (* the EFFECT of each classified server line ({!Server_proc} does the pure parsing): the dev-URL
     report drives the ready banner; a port-busy line is remembered ON THE INSTANCE so the crash
     handler can reclaim/name the holder; chatter is dropped; anything else is the user's app log. *)
  let on_line : Server_proc.parsed -> unit = function
    | Server_proc.Urls urls ->
      mark "READY (server bound)";
      (match !server with Up up -> up.busy_port <- None (* it bound — any earlier "port busy" is stale *) | Down -> ());
      Ui.ready ui ~ms:!last_build_ms ~urls ~gateway:gateway_url ~backend:(Mongo_rs.summary ());
      if not !agent_ready_sent then begin
        agent_ready_sent := true;
        emit_verdict (Verdict.Ready { url = gateway_url; dir = (match targets with t :: _ -> Filename.dirname t | [] -> ".") })
      end
    | Server_proc.Port_busy p -> ( match !server with Up up -> up.busy_port <- Some p | Down -> ())
    | Server_proc.Http access ->
      Ui.http ui access;
      (* failures ALSO go to the agent's separate runtime-error stream (never events.jsonl) *)
      if Paw.Access.is_error access then (match agent with Some a -> (try Agent_event.emit_error a access with _ -> ()) | None -> ())
    | Server_proc.Chatter -> ()
    | Server_proc.App_log line -> Ui.app ui line
  in
  let drain_server () = match !server with Up up -> Server_proc.drain up.proc ~on_line | Down -> () in

  (* a held dev port is resolved via {!Port}: reclaim a leftover of OUR server (SIGKILL), or name a
     foreign holder. The "is it ours" gate is pure + anchored + unit-tested there (it's a kill). *)
  let reclaim_port port = Port.reclaim ~exe port in
  let foreign_holder port = Port.foreign_holder ~exe port in

  (* ═══════════ server lifecycle: (re)start, then graceful shutdown ═══════════ *)

  let start_or_restart label =
    (* don't disturb the running server while the artifact is mid-write; the next settle restarts *)
    if not (Artifact.bytecode_ready exe) then ()
    else (
      (match !server with Up up -> Server_proc.stop up.proc | Down -> ());
      server := Down;
      (* re-verify after teardown — a new build can begin during [stop] and start rewriting the
         artifact; if so, skip the exec and let the next settle restart on a whole image *)
      if Artifact.bytecode_ready exe then
        match Server_proc.start ~exe ~env:dev_env with
        | Some proc ->
          mark "server-spawned";
          server := Up { proc; busy_port = None; last_exe = mtime exe };
          Assets.seed assets;
          (* re-record so the pidfile lists the NEW server pid, not the dead old one we just killed
             — otherwise the next run would SIGKILL whatever now owns that recycled pid *)
          record_pids ();
          label ()
        | None -> Ui.notice ui Ui.Error (Printf.sprintf "could not start the server (%s missing?)" exe); record_pids ())
  in

  let shutting_down = ref false in
  let shutdown _ =
    (* idempotent: a second signal (Ctrl-C mashed, or SIGTERM right after SIGINT) must not re-enter
       the teardown — just leave now. Keeps the handler from racing its own kill/reap. *)
    if !shutting_down then exit 0;
    shutting_down := true;
    (match !server with Up up -> Server_proc.stop up.proc | Down -> ()); (* frees the port before we go *)
    Ui.stopped ui;
    emit_verdict Verdict.Stopped;
    (match mongo with Some t -> Mongo_rs.stop t | None -> ());
    stop_test_worker ();
    (* WAIT for dune --watch to actually exit, don't just signal it: dune holds the build-dir lock and
       only releases it (and leaves a CONSISTENT build dir) once it finishes its current action and
       shuts down. Exiting while it is still winding down would hand the next `fennec dev` a held lock
       and a half-written build dir. [Dune_watch.stop] is SIGTERM → wait → SIGKILL. *)
    Dune_watch.stop !dw;
    kill worker_pid;
    (try Sys.remove control_path with _ -> ());
    (try Sys.remove worker_socket with _ -> ());
    (try Sys.remove pidfile with _ -> ());
    exit 0
  in
  Sys.set_signal Sys.sigint (Sys.Signal_handle shutdown);
  Sys.set_signal Sys.sigterm (Sys.Signal_handle shutdown);
  record_pids ();

  (* a trigger touches the FRAMEWORK (not the watched app) when its path lies outside every app
     watch_root — then the worker's preloaded image is stale and must be restarted. (A trigger string
     looks like "<path> changed"; we test path membership in a watch_root.) *)
  let trigger_under_watch_root trg =
    let path = match String.index_opt trg ' ' with Some i -> String.sub trg 0 i | None -> trg in
    List.exists (fun root -> path = root || (String.length path > String.length root && String.sub path 0 (String.length root + 1) = root ^ "/")) watch_roots
  in
  let framework_changed triggers =
    (* only meaningful while a worker is up; and ignore the test targets themselves *)
    !test_worker <> None && triggers <> [] && not (List.exists trigger_under_watch_root triggers)
  in

  (* ═══════════ build outcomes: a green settle (restart / reload / css / tests) or a failure ═══════════ *)

  let on_build_ok triggers dur =
    mark "build-settled";
    Crash_limiter.reset limiter;
    dune_exits := 0;
    last_build_ms := dur;
    (* a framework edit invalidates the worker's once-linked image: restart it so the next test run is
       warm again (a stale worker would otherwise just fall back via a Dynlink digest mismatch). *)
    if framework_changed triggers then restart_test_worker ();
    let agent_success = ref None in
    let just_wired_tests = ref false in
    let remember_agent served =
      agent_success :=
        Some
          (Verdict.Build_ok
             { trigger = triggers;
               served;
               build_ms = dur;
               tests = Verdict.Tests_not_changed;
               affected = Affected.classify ~backend:(served = Verdict.Backend_restart) triggers })
    in
    (* dune already rolling another build (a still-running edit burst): its artifact is being
       rewritten. Do nothing — the next settle restarts once, on a stable image. Coalesces the
       storm AND dodges a half-written load, with no wait: it's a state check, not a debounce. *)
    if Dune_watch.is_building !dw then ()
    else
      match !server with
      | Down ->
        (* first boot (or restart after a give-up): the URL banner ([Ui.ready]) comes from the
           server's port report, not here *)
        if Sys.file_exists exe then start_or_restart (fun () -> ()) else Ui.notice ui Ui.Info "built — waiting for the server binary…"
      | Up up ->
        if mtime exe > up.last_exe then
          start_or_restart (fun () ->
            Ui.rebuilt ui ~trigger:triggers ~ms:dur;
            remember_agent Verdict.Backend_restart)
        else (
          match Assets.poll assets with
          | Assets.Reload ->
            ping control_path "reload";
            Ui.reloaded ui ~trigger:triggers ~ms:dur;
            remember_agent Verdict.Full_reload
          | Assets.Css_only ->
            ping control_path "css";
            Ui.restyled ui ~trigger:triggers ~ms:dur;
            remember_agent Verdict.Css_only
          (* nothing the server cares about changed — but if this green build FIXED a prior error
             (a revert to identical bytes), clear the stuck panel; otherwise stay silent *)
          | Assets.Nothing ->
            Ui.resolved ui ~ms:dur;
            remember_agent Verdict.No_served_change);
      (* after the first successful settle, wire inline test runner targets into the watch so
         dune rebuilds them on every change. Only restart the watcher once (idempotent). *)
      if not !tests_wired then begin
        let test_targets = Dev_tests.targets dev_tests in
        if test_targets <> [] then begin
          tests_wired := true;
          just_wired_tests := true;
          (* restart the watcher with the expanded target list — dune now builds the runner
             exes alongside the server, without running them (+ the .bc's the worker loads) *)
          Dune_watch.stop !dw;
          dw := Dune_watch.start (targets @ test_targets @ warm_targets ());
          record_pids ()
        end
      end;
      (* run inline test runners whose exe mtime advanced since the last settle *)
      let tests =
        match Dev_tests.run_changed dev_tests with
        | None -> None
        | Some s ->
          Ui.tested ui ~passed:s.Dev_tests.total_passed ~failed:s.Dev_tests.total_failed
            ~libs:(List.length s.Dev_tests.results) ~ms:s.Dev_tests.ms;
          List.iter (fun (r : Dev_tests.result) ->
            if r.Dev_tests.failed > 0 then
              Ui.notice ui Ui.Warn (Printf.sprintf "test failures in %s:\n%s" r.Dev_tests.lib (String.trim r.Dev_tests.output)))
            s.Dev_tests.results;
          Some s
      in
      if !just_wired_tests then pending_agent_success := !agent_success
      else
      let agent_success =
        match (!pending_agent_success, tests) with
        | Some pending, Some _ ->
          pending_agent_success := None;
          Some pending
        | Some pending, None ->
          (* Do not strand a hook if the follow-up settle after wiring tests did not produce
             a changed runner. It is still the completed green verdict for the edit. *)
          pending_agent_success := None;
          Some pending
        | None, _ -> !agent_success
      in
      (match agent_success with
       | None -> ()
       | Some (Verdict.Build_ok b) ->
         emit_verdict (Verdict.Build_ok { b with tests = Verdict.tests_of_summary tests })
       | Some verdict -> emit_verdict verdict)
  in

  let on_build_failed _n triggers messages =
    pending_agent_success := None;
    Ui.failed ui ~raw:messages ~trigger:triggers ~serving:(serving ());
    emit_verdict
      (Verdict.Build_failed
         { trigger = triggers;
           diagnostics = Diagnostics.parse messages;
           raw = messages;
           last_good_serving = serving ();
           affected = Affected.classify triggers })
  in

  (* ═══════════ failure recovery: watcher exit, code crash, a held dev port ═══════════ *)

  let on_dune_exit () =
    incr dune_exits;
    (* a healthy dune --watch never exits on its own; if it keeps dying, the build environment is
       broken and respawning every 0.5s would just spin and spam. Give up cleanly after a few. *)
    if !dune_exits > 5 then (
      Ui.notice ui Ui.Error "dune --watch keeps exiting — fix the build environment, then restart `fennec dev`";
      emit_verdict Verdict.Watcher_exit;
      shutdown Sys.sigterm)
    else (
      Ui.notice ui Ui.Warn "dune watcher exited — restarting";
      emit_verdict Verdict.Watcher_restart;
      Unix.sleepf 0.5;
      dw := Dune_watch.start (targets @ (if !tests_wired then Dev_tests.targets dev_tests @ warm_targets () else []));
      record_pids ())
  in

  (* a code crash (not a port conflict): rate-limit restarts so a crash-loop doesn't spin *)
  let on_code_crash () =
    match Crash_limiter.record limiter ~now:(now ()) () with
    | Crash_limiter.Give_up ->
      Ui.notice ui Ui.Error "server kept crashing — fix the error and save to retry";
      emit_verdict (Verdict.Server_crash "server kept crashing — fix the error and save to retry")
    | Crash_limiter.Retry backoff ->
      Unix.sleepf backoff;
      start_or_restart (fun () ->
        Ui.notice ui Ui.Warn "server exited — restarted";
        emit_verdict (Verdict.Server_restart "server exited — restarted"))
  in
  let on_server_crash up status =
    let port_busy = status = Unix.WEXITED Dev_proto.port_in_use_exit in
    (* the crashed child was already reaped by [Server_proc.reap] in [check_server] (it returned this
       [status]); just close its pipe and drop to [Down] — no second waitpid on an already-reaped
       pid. We read [up.busy_port] from the crashed instance we were handed, so it can't be stale. *)
    Server_proc.close up.proc;
    server := Down;
    if not port_busy then on_code_crash ()
    else
      (* a held dev port: resolve it DECISIVELY on the FIRST failure. (A rate-limited retry loop
         is wrong here — it's slow, and under load the crashes can span the limiter's window so it
         never gives up, leaving the dev with a silent forever-retry instead of an answer.) *)
      match up.busy_port with
      | None -> on_code_crash () (* never parsed the port — treat as a generic crash *)
      | Some p ->
        if reclaim_port p then (
          (* it was a LEFTOVER of our own server — killed it, take the port now *)
          Unix.sleepf 0.15;
          start_or_restart (fun () -> Ui.notice ui Ui.Warn (Printf.sprintf "cleared a leftover dev server on :%d — restarted" p)))
        else (
          match foreign_holder p with
          | Some (pid, cmd) ->
            (* held by something that isn't ours: retrying can't help — name it + a one-command fix *)
            let short = if String.length cmd > 60 then String.sub cmd 0 57 ^ "…" else cmd in
            Ui.notice ui Ui.Error (Printf.sprintf "port %d is held by another process — pid %d: %s" p pid short);
            Ui.notice ui Ui.Error (Printf.sprintf "free it with:  kill %d   (then save any file to retry)" pid)
          | None ->
            (* nobody's holding it this instant (a transient blip): a rate-limited retry is right *)
            (match Crash_limiter.record limiter ~now:(now ()) ~flat:true () with
            | Crash_limiter.Give_up -> Ui.notice ui Ui.Error "the dev port is in use — free it and save any file to retry"
            | Crash_limiter.Retry backoff -> Unix.sleepf backoff; start_or_restart (fun () -> Ui.notice ui Ui.Warn "port freed — restarted")))
  in

  (* ═══════════ the loop: drain → check the child → poll the watcher → handle (total) ═══════════ *)

  let handle = function
    | Dune_watch.Settled_build { outcome = Dune_watch.Ok; triggers; duration_ms; _ } -> on_build_ok triggers duration_ms
    | Dune_watch.Settled_build { outcome = Dune_watch.Errors n; triggers; messages; _ } -> on_build_failed n triggers messages
    | Dune_watch.Exited -> on_dune_exit ()
  in
  (* collapse to the NEWEST buffered event (zero wait): a burst yields many settles; act on the
     latest only, restarting once on the final state instead of racing dune's in-flight builds.
     STOP at Exited — once the watcher is dead, poll returns Exited on every call (eof is sticky),
     so draining without this guard spins forever at 100% CPU. *)
  let rec newest ev = match ev with
    | Dune_watch.Exited -> ev (* nothing newer than death *)
    | _ -> match (try Dune_watch.poll !dw ~timeout:0. with _ -> None) with Some e -> newest e | None -> ev in
  (* did the server exit on its own? reap it (WNOHANG) and report a crash on the instance. *)
  let check_server () =
    match !server with
    | Down -> ()
    | Up up -> ( match Server_proc.reap up.proc with Some status -> on_server_crash up status | None -> ())
  in
  let step () =
    (* drain the server's output BEFORE checking for its exit, so an EADDRINUSE message ("port N
       in use") is parsed into [busy_port] before [on_server_crash] runs and wants it. *)
    drain_server (); (* fold in the server's port report + relay its app logs *)
    (* check the server EVERY iteration, not only when the dune poll times out — otherwise a
       server that crashed on boot during an edit burst (steady stream of settles) wouldn't be
       noticed until the burst quieted. *)
    check_server ();
    match Dune_watch.poll !dw ~timeout:0.2 with Some ev -> handle (newest ev) | None -> ()
  in
  let rec loop () =
    (try step () with e -> ef "fennec dev: internal error (continuing): %s\n%!" (Printexc.to_string e); Unix.sleepf 0.1);
    loop ()
  in
  loop ()
