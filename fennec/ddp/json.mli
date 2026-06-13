(** A tiny, pure JSON value + parser + serializer — Stdlib only, so the whole DDP layer compiles
    identically to native and JavaScript. UTF-8 passes through verbatim on output; [\u] escapes
    (including surrogate pairs) decode on input. Sized for protocol frames, not large documents.

    {[
      (* parse at the network boundary with the non-raising form, then read a field *)
      match Json.parse_opt body with
      | Some j -> Option.bind (Json.member "msg" j) Json.to_string_opt
      | None -> None
    ]} *)

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

(** Parse a JSON string. @raise Parse_error on malformed/trailing input. Non-finite numbers
    serialize as [null] (JSON has no NaN/Infinity); nesting is bounded; malformed [\u] surrogates
    decode to U+FFFD. *)
val parse : string -> t

(** [parse_opt s] is [Some (parse s)], or [None] on malformed input — the non-raising form for the
    network boundary, where bad input is expected. *)
val parse_opt : string -> t option

(** The magnitude below which an integral number serializes as an integer (and {!Ejson} decodes to
    [Int]); at or above it, numbers stay floats. *)
val int_cutoff : float

(** [member k j] is the value of object field [k] in [j], or [None] (also [None] for a non-object). *)
val member : string -> t -> t option

(** The string of a [String] value, else [None]. *)
val to_string_opt : t -> string option

(** The elements of a [List] value, else [None]. *)
val to_list_opt : t -> t list option
