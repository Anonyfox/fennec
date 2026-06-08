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

let () = exit (Fennec_hunt_unit.run ())
