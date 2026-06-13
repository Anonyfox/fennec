(** WebSocket endpoint as a paw. On a request targeting [path], it answers by upgrading the
    connection and running [setup] on the live channel; otherwise it declines. The RFC 6455
    handshake/framing is handled by the server. *)

(** Build a websocket paw serving [path] with the channel callback [setup]. In [setup], use
    [ch.send] to push a frame and set [ch.on_text] / [ch.on_close] to handle the peer:
    {[
      let paw =
        Websocket.make "/ws" (fun ch ->
            ch.Fennec_core.Ws_channel.on_text <- (fun msg -> ch.send ("echo: " ^ msg));
            ch.on_close <- (fun () -> ()))
    ]} *)
val make : string -> (Fennec_core.Ws_channel.t -> unit) -> Fennec_paw.Paw.t
