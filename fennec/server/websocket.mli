(** WebSocket endpoint as a paw. On a request targeting [path], it answers by upgrading the
    connection and running [setup] on the live channel; otherwise it declines. The RFC 6455
    handshake/framing is handled by the server. *)

(** Build a websocket paw serving [path] with the channel callback [setup]. *)
val make : string -> (Fennec_core.Ws_channel.t -> unit) -> Fennec_paw.Paw.t
