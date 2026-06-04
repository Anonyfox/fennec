(* WebSocket endpoint as a paw. When the request targets [path], answer by upgrading and
   running [setup] on the live channel; otherwise decline. The RFC 6455 handshake/framing is
   the server's job (it sees the pending upgrade on the conn) — so a websocket is just a paw.
   Livereload is built on this same primitive (see Livereload.paw). *)

module Conn = Fennec_paw.Conn
module Paw = Fennec_paw.Paw

let make (path : string) (setup : Fennec_core.Ws_channel.t -> unit) : Paw.t =
 fun c -> if Conn.path c = path then Conn.upgrade c setup else c
