(* The dual-compiled web/methods/ SLOT FORM end to end: add_task is declared in web/methods/add_task.mlx
   as a plain typed function `string -> Task.t`. The fur ppx (-method) DERIVES the wire contract from the
   signature and registers the body as the server handler (via the Rpc seam the facade installs);
   invoking it runs the handler + writes. Over MONGO_URL=:memory: under an Eio switch. *)

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

  check "add_task is registered (slot form, contract derived from the signature)"
    (List.mem "add_task" (R.method_names ()));
  let t = R.apply ~user_id:None ~remote_ip:None ~set_user_id:(fun _ -> ()) "add_task" [ B.String "Buy milk" ] in
  check "the handler ran and returned the created Task" (match t with B.Document _ -> true | _ -> false);
  check "the handler wrote the task" (List.length (Pulse.all Task.collection) = 1);

  Printf.printf "all method tests passed\n%!"
