(* See supervisor.mli. The loop reacts to one {!Dune_watch} event at a time:

     - a successful settle whose backend artifact changed -> restart the server;
     - a successful settle that only touched the web root  -> CSS hot-swap / reload ping;
     - a failed settle -> keep the last good server, print the diagnostics;
     - dune itself exiting -> respawn it;
     - (when idle) the server dying on its own -> rate-limited restart.

   It is TOTAL: every iteration is exception-wrapped, so no syscall, dead child, or parser
   surprise can take the process down. *)

module Dev_proto = Fennec_core.Dev_proto (* the shared CLI<->server wire (env names, stderr line formats, exit code) *)

let ef = Printf.eprintf (* last-resort raw stderr (preflight + the total-loop guard) *)
let now () = Unix.gettimeofday ()
let mtime path = try (Unix.stat path).Unix.st_mtime with _ -> 0.0
let starts_with s p = let lp = String.length p in String.length s >= lp && String.sub s 0 lp = p

(* ---- the OCaml stublibs dir, so a directly-spawned bytecode server can dlopen its C stubs
   regardless of the parent shell (what `dune exec` / `opam env` would provide) ---- *)
let stublibs_dir () =
  match Sys.getenv_opt "OPAM_SWITCH_PREFIX" with
  | Some p when p <> "" -> Some (Filename.concat p "lib/stublibs")
  | _ -> (
    match (try Some (String.trim (input_line (Unix.open_process_in "opam var lib 2>/dev/null"))) with _ -> None) with
    | Some lib when lib <> "" -> Some (Filename.concat lib "stublibs")
    | _ -> None)

let ensure_stublibs () =
  match stublibs_dir () with
  | None -> ()
  | Some dir ->
    let cur = try Sys.getenv "CAML_LD_LIBRARY_PATH" with Not_found -> "" in
    if not (List.mem dir (String.split_on_char ':' cur)) then
      Unix.putenv "CAML_LD_LIBRARY_PATH" (if cur = "" then dir else dir ^ ":" ^ cur)

(* ---- child processes ---- *)
(* spawn the server with stdout+stderr merged into ONE pipe we read, so the supervisor is the sole
   writer to the terminal (no interleaving) and can fold in the server's port report + relay its
   app logs through the UI. The child's fd 1/2 are dup'd from the write end before exec, so the
   server's output works; the parent keeps the read end. Returns (pid, read-end). *)
let start_server exe extra_env =
  let rd, wr = Unix.pipe () in
  let pid = Unix.create_process_env exe [| exe |] (Array.append (Unix.environment ()) extra_env) Unix.stdin wr wr in
  Unix.close wr;
  (pid, rd)

let kill pid = try Unix.kill pid Sys.sigterm with _ -> ()

(* SIGTERM, then SIGKILL if it lingers, then reap — so a freed port is actually free *)
let kill_reap pid =
  (try Unix.kill pid Sys.sigterm with _ -> ());
  let dead = ref false and i = ref 0 in
  while (not !dead) && !i < 6 do
    (match Unix.waitpid [ Unix.WNOHANG ] pid with
    | 0, _ -> Unix.sleepf 0.05 (* still alive: wait a beat *)
    | _ -> dead := true
    | exception Unix.Unix_error (Unix.EINTR, _, _) -> () (* a signal interrupted us: just retry *)
    | exception _ -> dead := true (* ECHILD &c: already reaped/gone *));
    incr i
  done;
  if not !dead then (try Unix.kill pid Sys.sigkill with _ -> ());
  (try ignore (Unix.waitpid [] pid) with _ -> ())

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

let run ~targets ~exe ~assets =
  if not (Sys.file_exists "dune-project") then (ef "fennec dev: run from a dune project root (no dune-project here)\n"; exit 1);
  let ui = Ui.create () in
  Ui.start ui ~dir:(match targets with t :: _ -> Filename.dirname t | [] -> ".");
  ensure_stublibs ();

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

  let dw = ref (Dune_watch.start targets) in
  let control_path = tmp_socket "fennec-lr" in
  (* FENNEC_DEV_PARENT lets the server watch THIS supervisor and self-exit if we die (even on
     SIGKILL), so it can never be left holding the dev port. FENNEC_DEV_UI asks the server to
     report its dev URLs (a [fennec:urls] line) so the supervisor owns the clickable URL. *)
  let dev_env =
    [| Dev_proto.env_mode ^ "=development";
       Dev_proto.env_livereload ^ "=" ^ control_path;
       Dev_proto.env_dev_parent ^ "=" ^ string_of_int (Unix.getpid ());
       Dev_proto.env_dev_ui ^ "=1" |]
  in
  let server_pid = ref None in
  let server_out = ref None in (* read-end of the server's merged stdout+stderr *)
  let server_carry = Buffer.create 256 in (* partial trailing line from the server *)
  let last_build_ms = ref None in (* duration of the build that (re)started the server *)
  let busy_port = ref None in (* the dev port the server reported as already-in-use (EADDRINUSE) *)
  let last_exe = ref 0.0 in
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
    Pidfile.record pidfile (Unix.getpid () :: Dune_watch.pid !dw :: worker_pid :: (match !server_pid with Some p -> [ p ] | None -> []))
  in
  let close_server_out () = match !server_out with Some fd -> (try Unix.close fd with _ -> ()); server_out := None | None -> () in

  (* classify a line the server printed: its dev-URL report drives the UI banner, its own framework
     chatter is suppressed (the UI says it better), everything else is the user's app log. The wire
     formats are parsed via {!Dev_proto} — the single shared definition both sides reference. *)
  let route_server_line raw =
    let line = String.trim raw in
    if line = "" then ()
    else
      match Dev_proto.parse_urls_line line with
      | Some urls ->
        busy_port := None; (* it bound — any earlier "port busy" is stale *)
        Ui.ready ui ~ms:!last_build_ms ~urls
      | None -> (
        match Dev_proto.parse_port_busy line with
        | Some p ->
          (* remember WHICH port so the crash handler can reclaim it (if a leftover of ours holds
             it) or name the culprit (if it's foreign) *)
          busy_port := Some p
        | None -> if starts_with line Dev_proto.chatter_prefix then () (* framework chatter *) else Ui.app ui line)
  in
  (* drain whatever the server has written (non-blocking), splitting it into lines *)
  let drain_server () =
    match !server_out with
    | None -> ()
    | Some fd ->
      let rec go () =
        match (try Unix.select [ fd ] [] [] 0.0 with _ -> ([], [], [])) with
        | [], _, _ -> ()
        | _ -> (
          let buf = Bytes.create 4096 in
          match (try Unix.read fd buf 0 (Bytes.length buf) with _ -> 0) with
          | 0 -> () (* EOF — the server's gone; check_server reaps the pid *)
          | k ->
            Buffer.add_subbytes server_carry buf 0 k;
            let s = Buffer.contents server_carry in
            Buffer.clear server_carry;
            let n = String.length s and start = ref 0 in
            for i = 0 to n - 1 do
              if s.[i] = '\n' then (route_server_line (String.sub s !start (i - !start)); start := i + 1)
            done;
            if !start < n then Buffer.add_string server_carry (String.sub s !start (n - !start));
            go ())
      in
      go ()
  in

  (* who is LISTENING on [port] now, as (pid, full command) — via lsof + ps; [] if lsof is absent *)
  let port_listeners port =
    match (try Some (Unix.open_process_in (Printf.sprintf "lsof -nP -iTCP:%d -sTCP:LISTEN -t 2>/dev/null" port)) with _ -> None) with
    | None -> []
    | Some ic ->
      let pids = ref [] in
      (try while true do (match int_of_string_opt (String.trim (input_line ic)) with Some p when p > 1 -> pids := p :: !pids | _ -> ()) done with End_of_file -> ());
      ignore (Unix.close_process_in ic);
      List.map
        (fun p ->
          let cmd = match (try Some (input_line (Unix.open_process_in (Printf.sprintf "ps -p %d -o args= 2>/dev/null" p))) with _ -> None) with Some s -> String.trim s | None -> "" in
          (p, cmd))
        !pids
  in
  (* a holder is OURS iff its command runs our server binary. We match the build-relative tail
     (e.g. "_build/default/examples/site/server.bc") rather than the absolute path, so a leftover
     started either way (absolute by a prior supervisor, or relative) is recognised — yet it's
     still our specific artifact, never an unrelated process that merely sits on the port. *)
  let exe_tail = match Dune_watch.find_sub exe "_build/" with Some i -> String.sub exe i (String.length exe - i) | None -> exe in
  let ours (_, cmd) = exe_tail <> "" && Dune_watch.find_sub cmd exe_tail <> None in
  (* free [port] by SIGKILLing any leftover of OUR server holding it; true if we killed something *)
  let reclaim_port port =
    let mine = List.filter ours (port_listeners port) in
    List.iter (fun (pid, _) -> try Unix.kill pid Sys.sigkill with _ -> ()) mine;
    if mine <> [] then Unix.sleepf 0.2; (* let the port actually free before the retry binds *)
    mine <> []
  in
  let foreign_holder port = List.find_opt (fun h -> not (ours h)) (port_listeners port) in

  let start_or_restart label =
    (* don't disturb the running server while the artifact is mid-write; the next settle restarts *)
    if not (Artifact.bytecode_ready exe) then ()
    else (
      (match !server_pid with Some pid -> kill_reap pid | None -> ());
      close_server_out ();
      server_pid := None;
      Buffer.clear server_carry;
      (* re-verify after teardown — a new build can begin during kill_reap and start rewriting the
         artifact; if so, skip the exec and let the next settle restart on a whole image *)
      if Artifact.bytecode_ready exe then
        match (try Some (start_server exe dev_env) with _ -> None) with
        | Some (pid, fd) -> server_pid := Some pid; server_out := Some fd; last_exe := mtime exe; Assets.seed assets; record_pids (); label ()
        (* re-record even on failure so the pidfile no longer lists the dead old server pid (we
           just killed it) — otherwise the next run would SIGKILL whatever now owns that pid *)
        | None -> Ui.notice ui Ui.Error (Printf.sprintf "could not start the server (%s missing?)" exe); record_pids ())
  in

  let shutting_down = ref false in
  let shutdown _ =
    (* idempotent: a second signal (Ctrl-C mashed, or SIGTERM right after SIGINT) must not re-enter
       the teardown — just leave now. Keeps the handler from racing its own kill/reap. *)
    if !shutting_down then exit 0;
    shutting_down := true;
    (match !server_pid with Some pid -> kill_reap pid | None -> ()); (* frees the port before we go *)
    close_server_out ();
    Ui.stopped ui;
    kill (Dune_watch.pid !dw);
    kill worker_pid;
    (try Sys.remove control_path with _ -> ());
    (try Sys.remove worker_socket with _ -> ());
    (try Sys.remove pidfile with _ -> ());
    exit 0
  in
  Sys.set_signal Sys.sigint (Sys.Signal_handle shutdown);
  Sys.set_signal Sys.sigterm (Sys.Signal_handle shutdown);
  record_pids ();

  let on_build_ok triggers dur =
    Crash_limiter.reset limiter;
    dune_exits := 0;
    last_build_ms := dur;
    (* dune already rolling another build (a still-running edit burst): its artifact is being
       rewritten. Do nothing — the next settle restarts once, on a stable image. Coalesces the
       storm AND dodges a half-written load, with no wait: it's a state check, not a debounce. *)
    if Dune_watch.is_building !dw then ()
    else
      match !server_pid with
      | None ->
        (* first boot: the URL banner ([Ui.ready]) comes from the server's port report, not here *)
        if Sys.file_exists exe then start_or_restart (fun () -> ()) else Ui.notice ui Ui.Info "built — waiting for the server binary…"
      | Some _ ->
        if mtime exe > !last_exe then start_or_restart (fun () -> Ui.rebuilt ui ~trigger:triggers ~ms:dur)
        else (
          match Assets.poll assets with
          | Assets.Reload -> ping control_path "reload"; Ui.reloaded ui ~trigger:triggers ~ms:dur
          | Assets.Css_only -> ping control_path "css"; Ui.restyled ui ~trigger:triggers ~ms:dur
          (* nothing the server cares about changed — but if this green build FIXED a prior error
             (a revert to identical bytes), clear the stuck panel; otherwise stay silent *)
          | Assets.Nothing -> Ui.resolved ui ~ms:dur)
  in

  let on_build_failed _n triggers messages = Ui.failed ui ~raw:messages ~trigger:triggers ~serving:(!server_pid <> None) in

  let on_dune_exit () =
    incr dune_exits;
    (* a healthy dune --watch never exits on its own; if it keeps dying, the build environment is
       broken and respawning every 0.5s would just spin and spam. Give up cleanly after a few. *)
    if !dune_exits > 5 then (
      Ui.notice ui Ui.Error "dune --watch keeps exiting — fix the build environment, then restart `fennec dev`";
      shutdown Sys.sigterm)
    else (
      Ui.notice ui Ui.Warn "dune watcher exited — restarting";
      Unix.sleepf 0.5;
      dw := Dune_watch.start targets;
      record_pids ())
  in

  (* a code crash (not a port conflict): rate-limit restarts so a crash-loop doesn't spin *)
  let on_code_crash () =
    match Crash_limiter.record limiter ~now:(now ()) () with
    | Crash_limiter.Give_up -> Ui.notice ui Ui.Error "server kept crashing — fix the error and save to retry"
    | Crash_limiter.Retry backoff -> Unix.sleepf backoff; start_or_restart (fun () -> Ui.notice ui Ui.Warn "server exited — restarted")
  in
  let on_server_crash status =
    let port_busy = status = Unix.WEXITED Dev_proto.port_in_use_exit in
    (* the crashed child was already reaped by [check_server]'s WNOHANG waitpid (it returned this
       [status]); just drop our handle — no second blocking waitpid on an already-reaped pid *)
    close_server_out ();
    server_pid := None;
    if not port_busy then on_code_crash ()
    else
      (* a held dev port: resolve it DECISIVELY on the FIRST failure. (A rate-limited retry loop
         is wrong here — it's slow, and under load the crashes can span the limiter's window so it
         never gives up, leaving the dev with a silent forever-retry instead of an answer.) *)
      match !busy_port with
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

  let handle = function
    | Dune_watch.Settled_build { outcome = Dune_watch.Ok; triggers; duration_ms; _ } -> on_build_ok triggers duration_ms
    | Dune_watch.Settled_build { outcome = Dune_watch.Errors n; triggers; messages; _ } -> on_build_failed n triggers messages
    | Dune_watch.Exited -> on_dune_exit ()
  in
  (* collapse to the NEWEST buffered event (zero wait): a burst yields many settles; act on the
     latest only, restarting once on the final state instead of racing dune's in-flight builds *)
  let rec newest ev = match (try Dune_watch.poll !dw ~timeout:0. with _ -> None) with Some e -> newest e | None -> ev in
  (* did the server exit on its own? reap it (WNOHANG) and report a crash. *)
  let check_server () =
    match !server_pid with
    | None -> ()
    | Some pid -> (match (try Unix.waitpid [ Unix.WNOHANG ] pid with _ -> (0, Unix.WEXITED 0)) with 0, _ -> () | _, status -> on_server_crash status)
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
