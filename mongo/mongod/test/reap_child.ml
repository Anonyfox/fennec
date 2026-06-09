(* Helper for the no-dangling test: start a mongod, print its pid, then exit — normally, or by
   raising if argv.(1) = "raise". It deliberately does NOT call stop, proving that the lifecycle's
   at_exit reaper kills the mongod on process exit (and that an uncaught exception, which still runs
   at_exit on the way out, is covered too). *)

module M = Fennec_mongo_mongod.Mongod

let () =
  match M.find () with
  | None -> exit 0
  | Some _ ->
      let t = M.start () in
      Printf.printf "%d\n%!" (M.pid t);
      if Array.length Sys.argv > 1 && Sys.argv.(1) = "raise" then failwith "boom"
