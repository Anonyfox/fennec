(* The app's typed method declarations — ONE shared value per method. The server attaches handlers
   to these (RData.handle, server.ml); the browser calls through them (Ddp_client.call_m). Because
   both sides reference the same value, a renamed method, a changed arity, or a drifted type is a
   COMPILE error across the whole app — and the codecs are the validation (a malformed call is a 400
   before the handler runs). This module is pure and shared: it compiles into the SSR binary and the
   JS bundle alike. *)

module M = Fennec_pulse_method

(* addTask: title -> the new task's _id *)
let add_task : (string, string) M.Method.t =
  M.Method.define "addTask" ~args:(M.Codec.a1 M.Codec.string) ~result:M.Codec.string
