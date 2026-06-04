(* `fennec dev` — the development orchestrator.

   Per examples/CLI-INTEROP.md, this is STRICTLY operational sugar over what dune
   and the framework already do. It owns no build logic — dune is the sole source
   watcher and builder. The CLI runs three things and supervises them:

     1. `dune build --watch <target>` — dune watches SOURCE and rebuilds everything
        incrementally (OCaml server + assets, since assets are dune rules that call
        `fennec build`).

     2. the server exe — spawned as a child. The CLI watches the build OUTPUT for
        filesystem events (the native `notify` watcher, recursive over the exe's
        build dir); when a backend change rebuilds the exe we restart the child.
        Its livereload socket drops and the browser reconnects -> reload.

     3. the served web root — the SAME output watcher also covers the assembled
        bundles. A frontend-only edit (CSS/JS) leaves the exe untouched, so instead
        of restarting we ping the server's dev control socket, which relays a CSS
        hot-swap or full reload. We gate on a content HASH, since dune rewrites the
        whole web root (fresh mtimes) on every build — only a real change reloads,
        and a CSS-only edit never reloads the JS.

   So ALL filesystem watching of build outputs lives here, evented, in one place;
   the framework watches nothing. Delete this command and the project is still a
   plain dune project that builds and runs (`dune build --watch` + `dune exec`);
   you lose only the automated restart and CSS hot-swap, exactly as CLI-INTEROP
   records. *)

let pf = Printf.printf
let ef = Printf.eprintf

(* poll an mtime; Stdlib only, cross-platform *)
let mtime path = try (Unix.stat path).Unix.st_mtime with _ -> 0.0

(* spawn `dune build --watch <targets…>`; returns the pid *)
let start_watch targets =
  let args = Array.of_list ("dune" :: "build" :: "--watch" :: targets) in
  Unix.create_process "dune" args Unix.stdin Unix.stderr Unix.stderr

(* The OCaml stublibs dir, so a directly-spawned BYTECODE server can dlopen its C stubs
   (dllcstruct_stubs.so, …) — the path `dune exec` / an active `opam env` would provide. We
   set it ourselves so `fennec dev` works even from a shell that didn't export
   CAML_LD_LIBRARY_PATH, without giving up bytecode's fast rebuilds. *)
(* is [dir] already one of the ':'-separated entries in [paths]? *)
let contains_path paths dir = List.mem dir (String.split_on_char ':' paths)

let stublibs_dir () =
  match Sys.getenv_opt "OPAM_SWITCH_PREFIX" with
  | Some p when p <> "" -> Some (Filename.concat p "lib/stublibs")
  | _ -> (
    match (try Some (String.trim (input_line (Unix.open_process_in "opam var lib 2>/dev/null"))) with _ -> None) with
    | Some lib when lib <> "" -> Some (Filename.concat lib "stublibs")
    | _ -> None)

(* prepend the stublibs dir to CAML_LD_LIBRARY_PATH in this process, so spawned children
   (which inherit our environment) can load bytecode C stubs. Idempotent enough for one call. *)
let ensure_stublibs_path () =
  match stublibs_dir () with
  | None -> ()
  | Some dir ->
    let cur = try Sys.getenv "CAML_LD_LIBRARY_PATH" with Not_found -> "" in
    if not (contains_path cur dir) then
      Unix.putenv "CAML_LD_LIBRARY_PATH" (if cur = "" then dir else dir ^ ":" ^ cur)

(* (re)spawn the server exe; returns the pid *)
let start_server exe extra_env =
  let env = Array.append (Unix.environment ()) extra_env in
  Unix.create_process_env exe [| exe |] env Unix.stdin Unix.stdout Unix.stderr

let kill_quietly pid = try Unix.kill pid Sys.sigterm with _ -> ()

(* A unix-socket path in the temp dir, unique to this CLI run. Kept short — unix
   socket paths cap around 100 bytes — falling back to /tmp if $TMPDIR is long. *)
let tmp_socket prefix =
  let name = Printf.sprintf "%s-%d.sock" prefix (Unix.getpid ()) in
  let p = Filename.concat (Filename.get_temp_dir_name ()) name in
  if String.length p <= 100 then p else Filename.concat "/tmp" name

(* the dev control socket the framework listens on (FENNEC_LIVERELOAD) *)
let control_socket_path () = tmp_socket "fennec-lr"

(* spawn the persistent esbuild worker (`fennec __esbuild-worker <socket>`), reusing
   this same fennec binary; returns its pid *)
let start_esbuild_worker socket =
  let exe = Sys.executable_name in
  Unix.create_process exe [| exe; "__esbuild-worker"; socket |] Unix.stdin Unix.stderr Unix.stderr

let run targets exe assets =
  if not (Sys.file_exists "dune-project") then begin
    ef "fennec dev: run from a dune project root (no dune-project here)\n";
    exit 1
  end;
  pf "fennec dev: watching %s, serving %s\n%!" (String.concat " " targets) exe;
  (* make the bytecode server's C stubs loadable regardless of the parent shell's env *)
  ensure_stublibs_path ();

  (* 0. the warm esbuild worker — started BEFORE dune so even the first build
     delegates to it. We export its socket; dune passes the env through to the
     `fennec build` rule actions, which connect to it for a fast incremental
     rebuild. If it doesn't come up, builds simply fall back to the cold path. *)
  let worker_socket = tmp_socket "fennec-esb" in
  let worker_pid = start_esbuild_worker worker_socket in
  let ww = ref 0.0 in
  while (not (Sys.file_exists worker_socket)) && !ww < 3.0 do
    Unix.sleepf 0.05;
    ww := !ww +. 0.05
  done;
  if Sys.file_exists worker_socket then Unix.putenv "FENNEC_ESBUILD_WORKER" worker_socket
  else pf "fennec dev: esbuild worker did not start; falling back to cold builds\n%!";

  (* 1. dune --watch: the one source watcher + builder *)
  let watch_pid = start_watch targets in

  (* wait for the first build to produce the exe *)
  let waited = ref 0.0 in
  while (not (Sys.file_exists exe)) && !waited < 30.0 do
    Unix.sleepf 0.2;
    waited := !waited +. 0.2
  done;
  if not (Sys.file_exists exe) then begin
    ef "fennec dev: %s was not built within 30s — is the target correct?\n" exe;
    kill_quietly watch_pid;
    exit 1
  end;

  (* 2. supervise the server: restart it whenever the exe artifact changes, and
     restart a crashed server — but with a crash-rate limiter so a server that
     fails instantly on boot doesn't hot-loop forever. *)
  let control_path = control_socket_path () in
  let dev_env = [| "FENNEC_ENV=development"; "FENNEC_LIVERELOAD=" ^ control_path |] in
  let server_pid = ref (start_server exe dev_env) in
  let last_exe = ref (mtime exe) in

  (* Frontend livereload. The output watcher (below) covers the served web root; on
     a real CONTENT change we ping the dev control socket. We hash files, not
     mtimes: dune rewrites the whole web root on every build, so unchanged files get
     fresh mtimes — only a true change should reload, and a CSS-only edit must not
     reload the JS. *)
  let assets_dir = Filename.concat (Filename.dirname exe) assets in
  let asset_hashes : (string, Digest.t) Hashtbl.t = Hashtbl.create 32 in
  let rec scan dir acc =
    match Sys.readdir dir with
    | exception _ -> acc
    | entries ->
      Array.fold_left
        (fun acc f ->
          let p = Filename.concat dir f in
          if (try Sys.is_directory p with _ -> false) then scan p acc
          else match Filename.extension f with ".css" | ".js" | ".mjs" -> p :: acc | _ -> acc)
        acc entries
  in
  (* refresh the hash table; return (any CSS changed, any non-CSS changed) *)
  let scan_assets () =
    let css = ref false and other = ref false in
    List.iter
      (fun p ->
        match (try Some (Digest.file p) with _ -> None) with
        | None -> ()
        | Some h -> (
          match Hashtbl.find_opt asset_hashes p with
          | Some old when old = h -> ()
          | _ ->
            Hashtbl.replace asset_hashes p h;
            if Filename.extension p = ".css" then css := true else other := true))
      (scan assets_dir []);
    (!css, !other)
  in
  (* push one frame to the server's dev control socket (best-effort; if the server
     isn't listening yet the connect just fails and the next build pings again) *)
  let ping (frame : string) =
    try
      let fd = Unix.socket Unix.PF_UNIX Unix.SOCK_STREAM 0 in
      Fun.protect
        ~finally:(fun () -> try Unix.close fd with _ -> ())
        (fun () ->
          Unix.connect fd (Unix.ADDR_UNIX control_path);
          let msg = frame ^ "\n" in
          ignore (Unix.write_substring fd msg 0 (String.length msg)))
    with _ -> ()
  in
  (* on a served-asset content change: a JS change wins (full reload), else CSS
     hot-swaps; nothing changed -> no frame *)
  let signal_assets () =
    let css, other = scan_assets () in
    if other then ping "reload" else if css then ping "css"
  in
  (* the web root is built alongside the exe (@dev) but may land a moment later;
     wait briefly so the seed + the recursive watch below see the real tree *)
  let w2 = ref 0.0 in
  while (not (Sys.file_exists assets_dir)) && !w2 < 5.0 do
    Unix.sleepf 0.1;
    w2 := !w2 +. 0.1
  done;
  ignore (scan_assets ()); (* seed the table with the current build (not a change) *)

  (* EVENTED detection via the CLI's cross-platform native watcher (FSEvents /
     inotify / kqueue — the `notify` crate). TWO narrow watches feed one event
     stream, deliberately NOT a recursive watch of the whole build dir — that would
     put an inotify watch on every node_modules / build subdir on Linux and flood
     events. Instead: the exe's dir NON-recursively (catches the relinked exe, a
     direct child), and the served web root RECURSIVELY (a small subtree whose dirs
     keep their inodes across rebuilds, so the watch survives). [await] returns the
     instant dune finishes writing; no polling. If the platform can't deliver
     events the watcher is unavailable and we fall back to a short sleep. *)
  let watcher = Fennec_buildkit.Watch.start (Filename.dirname exe) in
  let evented = Fennec_buildkit.Watch.available watcher in
  if evented then ignore (Fennec_buildkit.Watch.add watcher ~recursive:true assets_dir);
  pf "fennec dev: server up (pid %d) — rebuilds via %s\n%!" !server_pid
    (if evented then "fs events" else "poll");

  let shutdown _ =
    pf "\nfennec dev: shutting down\n%!";
    kill_quietly !server_pid;
    kill_quietly watch_pid;
    kill_quietly worker_pid;
    if evented then Fennec_buildkit.Watch.free watcher;
    (try Sys.remove control_path with _ -> ());
    (try Sys.remove worker_socket with _ -> ());
    exit 0
  in
  Sys.set_signal Sys.sigint (Sys.Signal_handle shutdown);
  Sys.set_signal Sys.sigterm (Sys.Signal_handle shutdown);

  (* Block until a filesystem event (instant on a finished rebuild) or a bounded
     timeout — the timeout only bounds how soon a CRASHED server is noticed; a real
     rebuild wakes us immediately via the event. No busy polling either way.
     Returns true if something happened (an event, or the poll-fallback tick), so
     the caller only rescans the served assets when there was a reason to. *)
  let await () =
    if evented then Fennec_buildkit.Watch.wait watcher ~timeout_ms:1000
    else (Unix.sleepf 0.2; true)
  in

  (* crash bookkeeping: count crashes within a sliding window (real time); back off
     and, past a threshold, stop restarting until the NEXT rebuild (which clears the
     streak, since a code change is the user's fix). *)
  let now () = Unix.gettimeofday () in
  let crash_times = ref [] in (* recent crash timestamps, newest first *)
  let window = 10.0 (* seconds *) and max_crashes = 5 in
  let prune () = crash_times := List.filter (fun t -> now () -. t <= window) !crash_times in
  let restart reason =
    server_pid := start_server exe dev_env;
    pf "fennec dev: %s -> restarted server (pid %d)\n%!" reason !server_pid
  in
  let rec loop ~suspended =
    let event = await () in
    if event then signal_assets (); (* frontend hot-swap: ping on a real asset change *)
    let m = mtime exe in
    let rebuilt = m > !last_exe in
    if rebuilt then begin
      last_exe := m;
      crash_times := []; (* a rebuild is the fix: clear the crash streak *)
      kill_quietly !server_pid;
      (try ignore (Unix.waitpid [] !server_pid) with _ -> ());
      restart "backend rebuilt";
      loop ~suspended:false
    end
    else if suspended then loop ~suspended:true (* wait for the next rebuild *)
    else begin
      let crashed =
        match Unix.waitpid [ Unix.WNOHANG ] !server_pid with
        | 0, _ -> false
        | _, _ -> true
        | exception _ -> false
      in
      if not crashed then loop ~suspended:false
      else begin
        crash_times := now () :: !crash_times;
        prune ();
        let n = List.length !crash_times in
        if n >= max_crashes then begin
          ef
            "fennec dev: server crashed %d times in %.0fs — giving up restarts. \
             Fix the error and save to retry.\n%!"
            n window;
          loop ~suspended:true
        end
        else begin
          (* exponential backoff: 0.2s, 0.4s, 0.8s, … capped at ~3s *)
          let delay = Float.min 3.0 (0.2 *. (2. ** float_of_int (n - 1))) in
          Unix.sleepf delay;
          restart (Printf.sprintf "server exited (crash %d/%d)" n max_crashes);
          loop ~suspended:false
        end
      end
    end
  in
  loop ~suspended:false
