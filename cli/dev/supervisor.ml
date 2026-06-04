(* See supervisor.mli. The loop reacts to one {!Dune_watch} event at a time:

     - a successful settle whose backend artifact changed -> restart the server;
     - a successful settle that only touched the web root  -> CSS hot-swap / reload ping;
     - a failed settle -> keep the last good server, print the diagnostics;
     - dune itself exiting -> respawn it;
     - (when idle) the server dying on its own -> rate-limited restart.

   It is TOTAL: every iteration is exception-wrapped, so no syscall, dead child, or parser
   surprise can take the process down. *)

let pf = Printf.printf
let ef = Printf.eprintf
let now () = Unix.gettimeofday ()
let mtime path = try (Unix.stat path).Unix.st_mtime with _ -> 0.0

(* ---- status output (colour only on a terminal, NO_COLOR-aware) ---- *)
let use_color =
  lazy
    (let tty = try Unix.isatty Unix.stderr with _ -> false in
     let no_color = match Sys.getenv_opt "NO_COLOR" with Some s -> s <> "" | None -> false in
     tty && not no_color)

let paint code s = if Lazy.force use_color then "\027[" ^ code ^ "m" ^ s ^ "\027[0m" else s
let dim s = paint "2" s
let fmt_trigger = function [] -> "filesystem change" | [ x ] -> x | x :: rest -> Printf.sprintf "%s (+%d more)" x (List.length rest)
let fmt_ms = function Some ms -> Printf.sprintf " · %.0fms" ms | None -> ""

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
let start_server exe extra_env =
  Unix.create_process_env exe [| exe |] (Array.append (Unix.environment ()) extra_env) Unix.stdin Unix.stdout Unix.stderr

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
  pf "fennec dev: watching %s\n%!" (String.concat " " targets);
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
  if Sys.file_exists worker_socket then Unix.putenv "FENNEC_ESBUILD_WORKER" worker_socket
  else pf "fennec dev: esbuild worker did not start; falling back to cold builds\n%!";

  let dw = ref (Dune_watch.start targets) in
  let control_path = tmp_socket "fennec-lr" in
  (* FENNEC_DEV_PARENT lets the server watch THIS supervisor and self-exit if we die (even on
     SIGKILL), so it can never be left holding the dev port *)
  let dev_env = [| "FENNEC_ENV=development"; "FENNEC_LIVERELOAD=" ^ control_path; "FENNEC_DEV_PARENT=" ^ string_of_int (Unix.getpid ()) |] in
  let server_pid = ref None in
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

  let start_or_restart label =
    (* don't disturb the running server while the artifact is mid-write; the next settle restarts *)
    if not (Artifact.bytecode_ready exe) then ()
    else (
      (match !server_pid with Some pid -> kill_reap pid | None -> ());
      server_pid := None;
      (* re-verify after teardown — a new build can begin during kill_reap and start rewriting the
         artifact; if so, skip the exec and let the next settle restart on a whole image *)
      if Artifact.bytecode_ready exe then
        match (try Some (start_server exe dev_env) with _ -> None) with
        | Some pid -> server_pid := Some pid; last_exe := mtime exe; Assets.seed assets; record_pids (); label ()
        (* re-record even on failure so the pidfile no longer lists the dead old server pid (we
           just killed it) — otherwise the next run would SIGKILL whatever now owns that pid *)
        | None -> ef "fennec dev: could not start the server (%s missing?)\n%!" exe; record_pids ())
  in

  let shutting_down = ref false in
  let shutdown _ =
    (* idempotent: a second signal (Ctrl-C mashed, or SIGTERM right after SIGINT) must not re-enter
       the teardown — just leave now. Keeps the handler from racing its own kill/reap. *)
    if !shutting_down then exit 0;
    shutting_down := true;
    pf "\nfennec dev: shutting down\n%!";
    (match !server_pid with Some pid -> kill_reap pid | None -> ()); (* frees the port before we go *)
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
    (* dune already rolling another build (a still-running edit burst): its artifact is being
       rewritten. Do nothing — the next settle restarts once, on a stable image. Coalesces the
       storm AND dodges a half-written load, with no wait: it's a state check, not a debounce. *)
    if Dune_watch.is_building !dw then ()
    else
      match !server_pid with
      | None ->
        if Sys.file_exists exe then start_or_restart (fun () -> pf "%s ready%s — http://localhost (dev)\n%!" (paint "32" "[fennec] ●") (fmt_ms dur))
        else pf "%s built, waiting for the server exe…\n%!" (paint "33" "[fennec] ○")
      | Some _ ->
        if mtime exe > !last_exe then
          start_or_restart (fun () -> pf "%s rebuilt · %s%s · server restarted\n%!" (paint "32" "[fennec] ●") (fmt_trigger triggers) (fmt_ms dur))
        else (
          match Assets.poll assets with
          | Assets.Reload -> ping control_path "reload"; pf "%s reload · %s%s\n%!" (paint "36" "[fennec] ↻") (fmt_trigger triggers) (fmt_ms dur)
          | Assets.Css_only -> ping control_path "css"; pf "%s css · %s%s\n%!" (paint "36" "[fennec] ↻") (fmt_trigger triggers) (fmt_ms dur)
          | Assets.Nothing -> ())
  in

  let on_build_failed n triggers messages =
    pf "%s build failed · %d error%s · %s\n%!" (paint "31" "[fennec] ✗") n (if n = 1 then "" else "s") (fmt_trigger triggers);
    if String.trim messages <> "" then ef "%s\n%!" (dim messages);
    match !server_pid with Some _ -> ef "%s\n%!" (dim "          (keeping the last working server)") | None -> ()
  in

  let on_dune_exit () =
    incr dune_exits;
    (* a healthy dune --watch never exits on its own; if it keeps dying, the build environment is
       broken and respawning every 0.5s would just spin and spam. Give up cleanly after a few. *)
    if !dune_exits > 5 then (
      ef "%s dune --watch keeps exiting — fix the build environment, then restart `fennec dev`.\n%!" (paint "31" "[fennec] ✗");
      shutdown Sys.sigterm)
    else (
      ef "%s dune watcher exited — restarting it\n%!" (paint "33" "[fennec] ⚠");
      Unix.sleepf 0.5;
      dw := Dune_watch.start targets;
      record_pids ())
  in

  let on_server_crash status =
    let port_busy = status = Unix.WEXITED 98 in
    (* the crashed child was already reaped by [check_server]'s WNOHANG waitpid (it returned this
       [status]); just drop our handle — no second blocking waitpid on an already-reaped pid *)
    server_pid := None;
    match Crash_limiter.record limiter ~now:(now ()) ~flat:port_busy () with
    | Crash_limiter.Give_up ->
      if port_busy then ef "%s port in use after several tries — another dev server running? free it and save to retry.\n%!" (paint "31" "[fennec] ✗")
      else ef "%s server kept crashing — fix the error and save to retry.\n%!" (paint "31" "[fennec] ✗")
    | Crash_limiter.Retry backoff ->
      Unix.sleepf backoff;
      start_or_restart (fun () ->
          if port_busy then pf "%s port freed — server restarted\n%!" (paint "33" "[fennec] ↻")
          else pf "%s server exited — restarted\n%!" (paint "33" "[fennec] ↻"))
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
    (* check the server EVERY iteration, not only when the dune poll times out — otherwise a
       server that crashed on boot during an edit burst (steady stream of settles) wouldn't be
       noticed until the burst quieted. *)
    check_server ();
    match Dune_watch.poll !dw ~timeout:0.5 with Some ev -> handle (newest ev) | None -> ()
  in
  let rec loop () =
    (try step () with e -> ef "fennec dev: internal error (continuing): %s\n%!" (Printexc.to_string e); Unix.sleepf 0.1);
    loop ()
  in
  loop ()
