(* A live websocket connection as a text-message channel — pure callbacks, no Eio, so paws and the
   realtime layer reference it freely. The native server provides the concrete [send] (serialized —
   safe to call from any fiber); a handler sets [on_text] / [on_close]. *)

type t = {
  send : string -> unit;
  mutable on_text : string -> unit;
  mutable on_close : unit -> unit;
}
