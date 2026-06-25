(* The dual-compiled web/methods/ end to end: the addTask method declared in web/methods/add_task.mlx
   registers its ~server handler for free (via the Rpc seam the facade installs), and invoking it runs
   the handler + writes. Over MONGO_URL=:memory: under an Eio switch. *)

let () = Unix.putenv "MONGO_URL" ":memory:"

module Pulse = Fennec_pulse_app (* its module-init installs the Rpc registrar *)
module R = Fennec_pulse_app.R
module B = Bson

let check msg c = if c then Printf.printf "  ok: %s\n%!" msg else (Printf.printf "  FAIL: %s\n%!" msg; exit 1)

let () =
  ignore Add_task.add_task;
  (* referencing the method forces Add_task's module-init, which registers the handler (buffered through
     the seam if it ran before the facade installed it) *)
  Eio_main.run @@ fun _env ->
  Eio.Switch.run @@ fun sw ->
  Fennec_mongo_dynamic.Dynamic.set_switch sw;

  check "addTask is registered server-side" (List.mem "addTask" (R.method_names ()));

  (* invoke it the way the DDP session does *)
  let result = R.apply ~user_id:None ~remote_ip:None ~set_user_id:(fun _ -> ()) "addTask" [ B.String "Buy milk" ] in
  check "the handler ran and returned an id" (match result with B.String s -> s <> "" | _ -> false);
  check "the handler wrote the task" (List.length (Pulse.all Task.collection) = 1);

  Printf.printf "all method tests passed\n%!"
