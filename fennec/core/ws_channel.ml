(* A live websocket connection as a text-message channel — pure (just callbacks),
   no Eio, so it can live in core and be referenced by paws. The native server
   provides the concrete [send]; a handler sets [on_text]/[on_close]. [send] is
   safe to call from any fiber (the server serializes it). *)

type t = {
  send : string -> unit;
  mutable on_text : string -> unit;
  mutable on_close : unit -> unit;
}
