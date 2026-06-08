(* mongod lifecycle, against a REAL server when installed. Skips (passes) if no mongod is found, so
   CI without MongoDB stays green; where mongod is present it launches an ephemeral instance,
   connects to its port, and confirms the data dir is removed on stop. *)

module M = Fennec_mongo_mongod.Mongod

let connects port =
  let s = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
  match Unix.connect s (Unix.ADDR_INET (Unix.inet_addr_loopback, port)) with
  | () -> (try Unix.close s with _ -> ()); true
  | exception _ -> (try Unix.close s with _ -> ()); false

let%test "find returns an executable when mongod is installed (else None, and we skip)" =
  match M.find () with None -> true | Some p -> Filename.basename p = "mongod" && Sys.file_exists p

let%test "ephemeral mongod: starts, accepts connections, stops and cleans its data dir" =
  match M.find () with
  | None -> true (* no mongod here — skip *)
  | Some _ ->
      let saved = ref "" in
      let inside =
        M.with_ephemeral (fun t ->
            saved := M.dbpath t;
            connects (M.port t)
            && M.uri t = Printf.sprintf "mongodb://127.0.0.1:%d" (M.port t)
            && Sys.file_exists (M.dbpath t)
            && M.pid t > 0)
      in
      (* on stop the ephemeral data dir is removed, and the port no longer accepts *)
      inside && (not (Sys.file_exists !saved))

(* NO DANGLING PROCESSES: a helper starts a mongod and exits WITHOUT stopping it (normally, and via
   an uncaught exception); the lifecycle's at_exit reaper must have killed it by the time the helper
   process has fully exited. We read the mongod pid the helper prints, then assert it is gone. *)
let alive pid = try Unix.kill pid 0; true with _ -> false

let run_helper args =
  let helper = Filename.concat (Filename.dirname Sys.executable_name) "reap_child.exe" in
  let cmd = Filename.quote helper ^ if args = "" then "" else " " ^ args in
  let ic = Unix.open_process_in cmd in
  let line = try input_line ic with End_of_file -> "" in
  ignore (Unix.close_process_in ic) (* blocks until the helper has fully exited (at_exit done) *);
  match int_of_string_opt (String.trim line) with Some pid -> Some pid | None -> None

let%test "no dangling: a process that starts a mongod and exits leaves no mongod (at_exit reaps it)" =
  match M.find () with
  | None -> true
  | Some _ ->
      let reaped args =
        match run_helper args with
        | None -> false
        | Some pid ->
            Unix.sleepf 0.2 (* small margin after the helper's reaping completes *);
            not (alive pid)
      in
      reaped "" (* normal exit *) && reaped "raise" (* uncaught exception *)

let () = exit (Fennec_hunt_unit.run ())
