(* `fennec dev` — the development orchestrator.

   Per examples/CLI-INTEROP.md, this is STRICTLY operational sugar over things
   dune and the framework already do. It owns no build logic and no file
   watching of source: it runs two long-lived things and supervises them.

     1. `dune build --watch <target>` — dune is the sole source watcher and the
        sole builder (OCaml server + assets, since assets are dune rules that call
        `fennec build`). This rebuilds everything incrementally on edit.

     2. the server exe — spawned as a child. When dune rebuilds it (backend
        change), the exe artifact's mtime changes; we restart the child. The
        browser's livereload socket drops and reconnects -> reload. Frontend-only
        edits don't restart the server; the framework pushes a hot-swap itself.

   Delete this command and the project still works: `dune build --watch` in one
   terminal + `dune exec <exe>` in another gives livereload via the same
   mechanisms. This is only convenience. *)

let pf = Printf.printf
let ef = Printf.eprintf

(* poll an mtime; Stdlib only, cross-platform *)
let mtime path = try (Unix.stat path).Unix.st_mtime with _ -> 0.0

(* spawn `dune build --watch <target>`; returns the pid *)
let start_watch target =
  let args = [| "dune"; "build"; "--watch"; target |] in
  Unix.create_process "dune" args Unix.stdin Unix.stderr Unix.stderr

(* (re)spawn the server exe; returns the pid *)
let start_server exe extra_env =
  let env = Array.append (Unix.environment ()) extra_env in
  Unix.create_process_env exe [| exe |] env Unix.stdin Unix.stdout Unix.stderr

let kill_quietly pid = try Unix.kill pid Sys.sigterm with _ -> ()

let run target exe =
  if not (Sys.file_exists "dune-project") then begin
    ef "fennec dev: run from a dune project root (no dune-project here)\n";
    exit 1
  end;
  pf "fennec dev: watching %s, serving %s\n%!" target exe;

  (* 1. dune --watch: the one source watcher + builder *)
  let watch_pid = start_watch target in

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

  (* 2. supervise the server: restart it whenever the exe artifact changes *)
  let dev_env = [| "FENNEC_ENV=development" |] in
  let server_pid = ref (start_server exe dev_env) in
  let last_exe = ref (mtime exe) in
  pf "fennec dev: server up (pid %d)\n%!" !server_pid;

  let shutdown _ =
    pf "\nfennec dev: shutting down\n%!";
    kill_quietly !server_pid;
    kill_quietly watch_pid;
    exit 0
  in
  Sys.set_signal Sys.sigint (Sys.Signal_handle shutdown);
  Sys.set_signal Sys.sigterm (Sys.Signal_handle shutdown);

  (* also reap an unexpectedly-dead server (e.g. a crash) and restart it *)
  let rec loop () =
    Unix.sleepf 0.3;
    let m = mtime exe in
    let rebuilt = m > !last_exe in
    let crashed =
      match Unix.waitpid [ Unix.WNOHANG ] !server_pid with
      | 0, _ -> false
      | _, _ -> true
      | exception _ -> false
    in
    if rebuilt then begin
      last_exe := m;
      (* the exe changed: restart so the new backend is live *)
      kill_quietly !server_pid;
      (try ignore (Unix.waitpid [] !server_pid) with _ -> ());
      server_pid := start_server exe dev_env;
      pf "fennec dev: backend rebuilt -> restarted server (pid %d)\n%!" !server_pid
    end
    else if crashed then begin
      server_pid := start_server exe dev_env;
      pf "fennec dev: server exited -> restarted (pid %d)\n%!" !server_pid
    end;
    loop ()
  in
  loop ()
