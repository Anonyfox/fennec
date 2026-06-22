(** A single-line input editor with history — the keystroke state machine behind the console prompt.

    It is deliberately I/O-free: {!feed} mutates the buffer and returns what the caller should do
    ({!outcome}); the caller (the REPL session) renders {!line} and talks to the socket. The same
    editor drives the standalone console and the [dev --console] prompt, so neither owns the rendering. *)

type t

(** A fresh editor with an empty buffer and no history. *)
val create : unit -> t

(** What a fed key asks the caller to do. *)
type outcome =
  | Edited  (** the buffer/cursor changed — redraw the prompt line *)
  | Submit of string  (** Enter on a non-empty buffer — evaluate this phrase (and it joins history) *)
  | Interrupt  (** Ctrl-C — cancel a running eval, or clear the line *)
  | Eof  (** Ctrl-D on an empty buffer — leave the console *)
  | Complete of string  (** Tab — complete this identifier prefix (the word before the cursor) *)
  | Clear  (** Ctrl-L — clear the screen *)
  | Nothing  (** nothing to do *)

(** Feed one decoded key; mutate state and report the {!outcome}. *)
val feed : t -> Key.t -> outcome

(** The current buffer contents. *)
val buffer : t -> string

(** The cursor's byte offset within {!buffer} ([0 .. length]). *)
val cursor : t -> int

(** Replace the buffer and put the cursor at its end (used to apply a completion). *)
val set : t -> string -> unit

(** [replace_leaf t candidate] swaps the dotted identifier's last segment ending at the cursor for
    [candidate] — e.g. with the buffer ["…Pulse.fi"] and [candidate = "find"] the buffer becomes
    ["…Pulse.find"]. Used to apply a single completion. *)
val replace_leaf : t -> string -> unit
