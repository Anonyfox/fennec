(** The frontend side of {!Console_protocol}: a Unix-socket connection to the in-server eval engine.

    Deliberately [Unix]-fd based (not Eio) so the one client works in BOTH the standalone console's own
    [select] loop and the [dev --console] supervisor's synchronous poll loop. *)

type t

(** [connect path] opens the engine's unix socket. @raise Unix.Unix_error if it cannot connect. *)
val connect : string -> t

(** The underlying fd, for the caller to [select] on alongside stdin. *)
val fd : t -> Unix.file_descr

(** Send one request (a whole frame). *)
val send : t -> Console_protocol.request -> unit

(** Read whatever bytes are available (call when {!fd} is readable) and decode the complete replies
    that result. [`Closed] means the engine hung up. *)
val receive : t -> [ `Replies of Console_protocol.reply list | `Closed ]

(** Close the connection. *)
val close : t -> unit
