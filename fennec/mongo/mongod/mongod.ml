(* mongod lifecycle management — launch and supervise a real MongoDB server for dev/test, in pure
   OCaml over Unix. Minimongo (:memory:) remains the default for dev/test; this is the path to a
   real mongod when one is wanted (and the harness the libmongoc driver is exercised against).

   A launched instance gets its own data directory and TCP port, is waited on until it actually
   accepts connections (not merely spawned), and is stopped gracefully (SIGTERM, then SIGKILL after
   a grace period). Ephemeral instances clean their data dir on stop. *)

exception Not_installed of string
exception Launch_failed of string

type t = {
  port : int;
  dbpath : string;
  pid : int;
  ephemeral : bool;
  logpath : string;
  mutable stopped : bool; (* idempotency gate: only the first stop does the work *)
}

(* ---- the live-instance registry: every started instance is reaped on process exit ------------ *)
(* No dangling mongods. A thread-safe registry (the test orchestrator runs suites on worker threads,
   each possibly starting its own instance) plus a single at_exit that stops them all. at_exit runs
   on normal exit AND on an uncaught exception that unwinds to exit; for signals, the CLI tracks the
   pid in its reaper (SIGINT/SIGTERM → kill + exit → this at_exit cleans the data dirs). Only a
   SIGKILL of the launcher can leak an instance — and since instances use a free port + a private
   temp dir, a leaked one never collides with the next run. *)
let registry_mutex = Mutex.create ()
let live : t list ref = ref []
let at_exit_installed = ref false

let with_lock f =
  Mutex.lock registry_mutex;
  Fun.protect ~finally:(fun () -> Mutex.unlock registry_mutex) f

let register t = with_lock (fun () -> live := t :: !live)

(* ---- locating the binary ------------------------------------------------- *)

let is_executable p = try Unix.access p [ Unix.X_OK ]; (Unix.stat p).Unix.st_kind = Unix.S_REG with _ -> false

let find ?(extra = []) () =
  let from_path =
    match Sys.getenv_opt "PATH" with
    | Some path -> List.map (fun d -> Filename.concat d "mongod") (String.split_on_char ':' path)
    | None -> []
  in
  let common = [ "/opt/homebrew/bin/mongod"; "/usr/local/bin/mongod"; "/usr/bin/mongod"; "/snap/bin/mongod" ] in
  List.find_opt is_executable (extra @ from_path @ common)

let install_hint () =
  "mongod was not found. Minimongo (:memory:) is the default for dev/test and needs no mongod; \
   install MongoDB Community Server only if you want a real server:\n\
  \  macOS:  brew tap mongodb/brew && brew install mongodb-community\n\
  \  Linux:  https://www.mongodb.com/docs/manual/administration/install-on-linux/\n\
   Then ensure `mongod` is on PATH (or pass its path explicitly)."

(* ---- small Unix helpers -------------------------------------------------- *)

(* ask the OS for a free loopback TCP port by binding to port 0 *)
let free_port () =
  let s = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
  Fun.protect
    ~finally:(fun () -> try Unix.close s with _ -> ())
    (fun () ->
      Unix.setsockopt s Unix.SO_REUSEADDR true;
      Unix.bind s (Unix.ADDR_INET (Unix.inet_addr_loopback, 0));
      match Unix.getsockname s with Unix.ADDR_INET (_, p) -> p | _ -> 0)

let can_connect port =
  let s = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
  match Unix.connect s (Unix.ADDR_INET (Unix.inet_addr_loopback, port)) with
  | () -> (try Unix.close s with _ -> ()); true
  | exception _ -> (try Unix.close s with _ -> ()); false

let sleep secs = try ignore (Unix.select [] [] [] secs) with _ -> ()

(* still running? (0) ; or reaped with a status *)
let exited pid = match Unix.waitpid [ Unix.WNOHANG ] pid with 0, _ -> None | _, status -> Some status | exception _ -> Some (Unix.WEXITED 0)

let make_temp_dir () =
  let f = Filename.temp_file "fennec-mongod-" "" in
  Sys.remove f;
  Unix.mkdir f 0o755;
  f

let rm_rf dir = if Sys.file_exists dir then ignore (Sys.command (Printf.sprintf "rm -rf %s" (Filename.quote dir)))

let tail_log path =
  try
    let ic = open_in path in
    Fun.protect
      ~finally:(fun () -> try close_in ic with _ -> ())
      (fun () ->
        let lines = ref [] in
        (try while true do lines := input_line ic :: !lines done with End_of_file -> ());
        String.concat "\n" (List.rev (List.filteri (fun i _ -> i < 20) !lines)))
  with _ -> "(no log)"

(* ---- lifecycle ----------------------------------------------------------- *)

let port t = t.port
let dbpath t = t.dbpath
let pid t = t.pid
let logpath t = t.logpath
let uri t = Printf.sprintf "mongodb://127.0.0.1:%d" t.port

let wait_ready ~timeout port pid logpath =
  let deadline = Unix.gettimeofday () +. timeout in
  let rec loop () =
    match exited pid with
    | Some _ -> raise (Launch_failed (Printf.sprintf "mongod exited before accepting connections:\n%s" (tail_log logpath)))
    | None ->
        if can_connect port then ()
        else if Unix.gettimeofday () > deadline then (
          (try Unix.kill pid Sys.sigkill with _ -> ());
          (try ignore (Unix.waitpid [] pid) with _ -> ());
          raise (Launch_failed (Printf.sprintf "mongod did not accept connections within %.0fs:\n%s" timeout (tail_log logpath))))
        else (sleep 0.1; loop ())
  in
  loop ()

(* idempotent: only the first stop does the work + unregisters (at_exit and an explicit stop can
   both reach here, possibly on different threads) *)
let stop t =
  let proceed =
    with_lock (fun () ->
        if t.stopped then false else (t.stopped <- true; live := List.filter (fun x -> x != t) !live; true))
  in
  if proceed then begin
    (try Unix.kill t.pid Sys.sigterm with _ -> ());
    let deadline = Unix.gettimeofday () +. 10. in
    let rec wait () =
      match exited t.pid with
      | Some _ -> ()
      | None ->
          if Unix.gettimeofday () > deadline then (
            (try Unix.kill t.pid Sys.sigkill with _ -> ());
            try ignore (Unix.waitpid [] t.pid) with _ -> ())
          else (sleep 0.1; wait ())
    in
    wait ();
    if t.ephemeral then rm_rf t.dbpath
  end

(* reap every still-live instance — registered once as an at_exit, so a normal exit or an uncaught
   exception that unwinds to exit never leaves a mongod behind *)
let stop_all () = List.iter (fun t -> try stop t with _ -> ()) (with_lock (fun () -> !live))

let ensure_at_exit () =
  let fresh = with_lock (fun () -> if !at_exit_installed then false else (at_exit_installed := true; true)) in
  if fresh then at_exit stop_all

let start ?mongod ?port ?dbpath ?(timeout = 30.) () =
  let bin = match mongod with Some b -> b | None -> ( match find () with Some b -> b | None -> raise (Not_installed (install_hint ()))) in
  let port = match port with Some p -> p | None -> free_port () in
  let ephemeral, dbpath = match dbpath with Some d -> (false, d) | None -> (true, make_temp_dir ()) in
  (try Unix.mkdir dbpath 0o755 with Unix.Unix_error (Unix.EEXIST, _, _) -> () | e -> raise e);
  let logpath = Filename.concat dbpath "mongod.log" in
  let devnull = Unix.openfile "/dev/null" [ Unix.O_RDONLY ] 0 in
  let log_fd = Unix.openfile logpath [ Unix.O_WRONLY; Unix.O_CREAT; Unix.O_TRUNC ] 0o644 in
  let args =
    [| bin; "--dbpath"; dbpath; "--port"; string_of_int port; "--bind_ip"; "127.0.0.1"; "--nounixsocket" |]
  in
  let pid =
    try Unix.create_process bin args devnull log_fd log_fd
    with e ->
      (try Unix.close devnull with _ -> ());
      (try Unix.close log_fd with _ -> ());
      if ephemeral then rm_rf dbpath;
      raise (Launch_failed (Printexc.to_string e))
  in
  (try Unix.close devnull with _ -> ());
  (try Unix.close log_fd with _ -> ());
  let t = { port; dbpath; pid; ephemeral; logpath; stopped = false } in
  register t (* registered BEFORE the readiness wait, so even a wait that hangs-then-fails is reaped *);
  ensure_at_exit ();
  (try wait_ready ~timeout port pid logpath with e -> stop t (* kill + clean + unregister *); raise e);
  t

let with_ephemeral ?mongod ?timeout f =
  let t = start ?mongod ?timeout () in
  Fun.protect ~finally:(fun () -> stop t) (fun () -> f t)
