(* The app's typed method declarations — ONE shared value per method. The server attaches handlers
   to these (RData.handle, server.ml); the browser calls through them (Ddp_client.call_m). Because
   both sides reference the same value, a renamed method, a changed arity, or a drifted type is a
   COMPILE error across the whole app — and the codecs are the validation (a malformed call is a 400
   before the handler runs). This module is pure and shared: it compiles into the SSR binary and the
   JS bundle alike. *)


(* addTask: title -> the new task's _id. The stub is the opt-in optimistic half: it predicts the
   handler's one insert against the client cache, so the row appears INSTANTLY; the server's
   [updated] then reveals truth — and because both sides mint the insert id from the call's seed,
   the optimistic row and the real one are the same row (no flicker). *)
let add_task : (string, string) Method.t =
  Method.define "addTask" ~args:(Codec.a1 Codec.string) ~result:Codec.string
    ~stub:(fun sim title -> ignore (sim.Method.insert "tasks" (Bson.doc [ ("title", Bson.str title) ])))
