(** The console REPL session — ties the line {!Editor}, the socket {!Console_client}, and a render
    {!sink} together.

    The same session drives the standalone console ({!run}, which owns the terminal) and the
    [dev --console] prompt (the supervisor feeds it keys + replies). Rendering is pluggable through the
    {!sink}, so a frontend — a terminal, the dev UI, or a future web surface — is simply a sink. *)

(** Where a session draws. Every callback renders {e above} a single prompt line pinned at the bottom. *)
type sink = {
  banner : Console_protocol.banner -> unit;  (** show the welcome context, once *)
  output : id:int -> text:string -> is_error:bool -> unit;  (** an eval's result/error, above the prompt *)
  prompt : string -> cursor:int -> unit;  (** (re)draw the input line ([prompt ^ buffer]); cursor at byte [cursor] *)
  commit : unit -> unit;  (** freeze the on-screen prompt line into scrollback (on submit) *)
  notice : string -> unit;  (** a one-off line (completion list / info), above the prompt *)
}

type t

(** A session over [client], rendering through [sink]. [prompt] defaults to ["fennec» "]. *)
val create : client:Console_client.t -> sink:sink -> ?prompt:string -> unit -> t

(** Redraw the prompt line. *)
val render_prompt : t -> unit

(** Feed one decoded key; [`Quit] means the user asked to leave (Ctrl-D on an empty line). *)
val feed_key : t -> Key.t -> [ `Ok | `Quit ]

(** Feed engine replies (from {!Console_client.receive}). *)
val feed_replies : t -> Console_protocol.reply list -> unit

(** Run a standalone console against the engine socket at [sock_path]: own the terminal in raw mode,
    [select] on stdin + the socket, loop until Ctrl-D or the engine hangs up. Restores the terminal on
    exit. A no-op-ish fallback when stdin is not a TTY. *)
val run : sock_path:string -> unit
