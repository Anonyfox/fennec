(** A tiny, pure JSON value + parser + serializer — Stdlib only, so the whole DDP layer compiles
    identically to native and JavaScript. UTF-8 passes through verbatim on output; [\u] escapes
    (including surrogate pairs) decode on input. Sized for protocol frames, not large documents. *)

(** A JSON value. *)
type t =
  | Null
  | Bool of bool
  | Number of float
  | String of string
  | List of t list
  | Obj of (string * t) list

(** Serialize a JSON value to a string. *)
val to_string : t -> string

(** Raised by {!parse} on malformed input. *)
exception Parse_error of string

(** Parse a JSON string. @raise Parse_error on malformed input. *)
val parse : string -> t

(** [member k j] is the value of object field [k] in [j], or [None] (also [None] for a non-object). *)
val member : string -> t -> t option

(** The string of a [String] value, else [None]. *)
val to_string_opt : t -> string option

(** The elements of a [List] value, else [None]. *)
val to_list_opt : t -> t list option
