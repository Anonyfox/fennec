(** RFC 6455 WebSocket codec over Eio buffered IO. Reads enforce the protocol
    invariants (client frames masked, frame/message size caps, control-frame
    constraints, reserved bits); writes are server-side (unmasked). *)

type opcode = Continuation | Text | Binary | Close | Ping | Pong | Other of int

(** [rsv1] carries the permessage-deflate "compressed" bit (RFC 7692). *)
type frame = { fin : bool; rsv1 : bool; opcode : opcode; payload : string }

(** Outcome of reading a frame. *)
type read_result =
  | Frame of frame
  | Eof  (** clean end of stream *)
  | Protocol_error of string  (** caller should answer with a Close (1002/1009) *)

(** Per-frame / per-message payload ceilings (bytes). *)
val max_frame_size : int
val max_message_size : int

(** Compute the [Sec-WebSocket-Accept] value for a client key. *)
val accept_key : string -> string

(** Read one frame, enforcing all RFC 6455 read invariants. *)
val read_frame : Eio.Buf_read.t -> read_result

(** Write a server (unmasked) frame. *)
val write_frame : Eio.Buf_write.t -> frame -> unit

(** A Close frame carrying a 2-byte status code (default 1000). *)
val close_frame : ?code:int -> unit -> frame

(** Write a client-style MASKED frame — for tests that exercise {!read_frame}. *)
val write_masked_frame : ?mask:string -> Eio.Buf_write.t -> frame -> unit
