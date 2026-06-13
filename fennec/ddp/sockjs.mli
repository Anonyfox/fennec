(** SockJS framing for the websocket transport — the thin compat layer so a stock Meteor browser
    client (which dials [/sockjs] first) can speak to us. Raw [/websocket] is the primary path; this
    only wraps/unwraps DDP JSON. Pure — native and JavaScript.

    {[
      (* open the SockJS channel, then frame DDP out and unframe DDP in *)
      ch.send Sockjs.open_frame;
      ch.send (Sockjs.wrap [ Message.encode m ]);
      List.iter (fun ddp -> handle ddp) (Sockjs.unwrap frame)
    ]} *)

(** The SockJS open frame, ["o"]. *)
val open_frame : string

(** The SockJS heartbeat frame, ["h"]. *)
val heartbeat : string

(** Wrap DDP message strings as a server→client SockJS array frame ([a["…","…"]]). *)
val wrap : string list -> string

(** Frame DDP message strings as a client→server SockJS array ([["…","…"]], no ['a'] prefix). *)
val client_frame : string list -> string

(** Whether a frame is a SockJS array-of-messages frame (server ['a[...]'] or client ['[...]']);
    control frames (['o']/['h']/['c[...]']) are handled separately. *)
val is_array_frame : string -> bool

(** [close_frame code reason] is the SockJS close frame ([c[code,"reason"]]). *)
val close_frame : int -> string -> string

(** Unwrap a client→server frame ([["…",…]] | ["…"] | [a[…]]) to its DDP JSON strings. *)
val unwrap : string -> string list

(** [info ~entropy] is the [/sockjs/info] handshake payload (entropy supplied by the caller). *)
val info : entropy:int -> string

(** Whether a request path is a SockJS websocket path (contains ["/sockjs/"]). *)
val is_sockjs_path : string -> bool
