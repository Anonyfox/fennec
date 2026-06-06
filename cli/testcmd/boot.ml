(* Boot one app instance for a suite: spawn the server bytecode with the suite's isolated env,
   wait for its port, and tear it down cleanly. Output goes to a per-instance LOG FILE (not a
   pipe — so a chatty server can never fill a pipe and block, and the log is there for failure
   diagnostics). The bytecode finds its C stubs via the inherited environment (opam's
   CAML_LD_LIBRARY_PATH), and its webroot next to its own exe — so cwd is irrelevant. *)

type t = { pid : int; log_path : string }

let env_array (extra : (string * string) list) =
  Array.append (Unix.environment ()) (Array.of_list (List.map (fun (k, v) -> k ^ "=" ^ v) extra))

let spawn ~exe ~(env : (string * string) list) : t =
  let log_path = Filename.temp_file "fennec_test_" ".log" in
  let fd = Unix.openfile log_path [ Unix.O_WRONLY; Unix.O_CREAT; Unix.O_TRUNC ] 0o644 in
  let pid = Unix.create_process_env exe [| exe |] (env_array env) Unix.stdin fd fd in
  Unix.close fd;
  { pid; log_path }

let alive pid = match Unix.waitpid [ Unix.WNOHANG ] pid with 0, _ -> true | _ -> false | exception _ -> false

let port_open port =
  match Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 with
  | fd -> Fun.protect ~finally:(fun () -> try Unix.close fd with _ -> ())
            (fun () -> try Unix.connect fd (Unix.ADDR_INET (Unix.inet_addr_loopback, port)); true with _ -> false)
  | exception _ -> false

(* block until the instance accepts on [port], or fail (server exited / timed out) *)
let wait_ready t ~port ~timeout : (unit, string) result =
  let deadline = Unix.gettimeofday () +. timeout in
  let rec loop () =
    if port_open port then Ok ()
    else if not (alive t.pid) then Error "the server exited before binding its port"
    else if Unix.gettimeofday () > deadline then Error (Printf.sprintf "the server did not become ready within %.0fs" timeout)
    else (Unix.sleepf 0.05; loop ())
  in
  loop ()

(* SIGTERM, then SIGKILL if it lingers, then reap — guarantees the port is freed. *)
let stop t =
  (try Unix.kill t.pid Sys.sigterm with _ -> ());
  let dead = ref false and i = ref 0 in
  while (not !dead) && !i < 20 do
    (match Unix.waitpid [ Unix.WNOHANG ] t.pid with
     | 0, _ -> Unix.sleepf 0.05
     | _ -> dead := true
     | exception _ -> dead := true);
    incr i
  done;
  if not !dead then ((try Unix.kill t.pid Sys.sigkill with _ -> ()); (try ignore (Unix.waitpid [] t.pid) with _ -> ()))

let read_log t = try In_channel.with_open_bin t.log_path In_channel.input_all with _ -> ""
let cleanup t = try Sys.remove t.log_path with _ -> ()
