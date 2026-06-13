(** Typed selectors over field handles — each combinator compiles to the Bson selector the engine
    already executes; field names/encodings come from the declaration (a renamed field is a compile
    error). Combine with {!all} (AND, the default reading of a list) / {!any} (OR); {!raw} keeps
    the full Mongo operator surface reachable. *)

type t = (string * Bson.t) list

val eq : 'a Codec.field -> 'a -> t
val ne : 'a Codec.field -> 'a -> t
val lt : 'a Codec.field -> 'a -> t
val lte : 'a Codec.field -> 'a -> t
val gt : 'a Codec.field -> 'a -> t
val gte : 'a Codec.field -> 'a -> t
val in_ : 'a Codec.field -> 'a list -> t
val nin : 'a Codec.field -> 'a list -> t
val exists : 'a Codec.field -> bool -> t

(** Array membership: the list field contains this element. *)
val has : 'a list Codec.field -> 'a -> t

(** The list field contains ALL of these elements ($all). *)
val contains_all : 'a list Codec.field -> 'a list -> t

(** The list field has exactly [n] elements ($size). *)
val size : _ list Codec.field -> int -> t

(** [regex ?opts f re] — a string field matches the pattern ([opts] e.g. ["i"]). *)
val regex : ?opts:string -> string Codec.field -> string -> t

(** Negate a clause ($nor of one). *)
val not_ : t -> t

val all : t list -> t
val any : t list -> t
val raw : Bson.t -> t
val to_bson : t -> Bson.t
