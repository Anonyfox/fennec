(** Bson codecs — the typed edge of the method layer. A ['a t] encodes/decodes one value; an
    ['a args] maps a whole positional parameter list (DDP method params). Decoding doubles as
    {e validation}: the server turns a decode failure into a ["400"] before the handler runs — what
    Meteor needs check/ValidatedMethod for, the codec gives for free. Value-level combinators (no
    ppx, no functors), so hundreds of methods stay cheap to compile. *)

(** A bidirectional value codec. [enc] is used for outgoing values; [dec] is used for incoming
    values and may reject malformed or semantically invalid input with a user-facing reason. *)
type 'a t = {
  enc : 'a -> Bson.t;  (** Encode one typed value to the BSON wire representation. *)
  dec : Bson.t -> ('a, string) result;  (** Decode and validate one BSON value. *)
}

(** Build a codec from raw encode/decode functions. *)
val make : enc:('a -> Bson.t) -> dec:(Bson.t -> ('a, string) result) -> 'a t

(** UTF-8 text encoded as BSON [String]. *)
val string : string t

(** Accepts [Int] and integral [Float] (EJSON numbers arrive as floats off the wire). *)
val int : int t

(** Floating-point numbers; BSON [Int] values decode by widening to float. *)
val float : float t

(** Boolean values encoded as BSON [Bool]. *)
val bool : bool t

(** The untyped escape hatch: any Bson value passes through. *)
val bson : Bson.t t

(** Encodes as [Null]. *)
val unit : unit t

(** [None] ⇔ [Null]. *)
val option : 'a t -> 'a option t

(** Homogeneous lists encoded as BSON arrays. *)
val list : 'a t -> 'a list t

(** [conv dec enc base] lifts [base] to a richer type — [dec] may reject (the message reaches the
    client as the 400 reason). The way to codec a record: [conv] over {!bson} using {!field}. *)
val conv : ('a -> ('b, string) result) -> ('b -> 'a) -> 'a t -> 'b t

(** [field d name c] reads field [name] of document [d] with [c] (error mentions the field). *)
val field : Bson.t -> string -> 'a t -> ('a, string) result

(** A whole positional parameter list. *)
type 'a args = { enc_args : 'a -> Bson.t list; dec_args : Bson.t list -> ('a, string) result }

(** No positional method arguments. *)
val a0 : unit args

(** One positional method argument. *)
val a1 : 'a t -> 'a args

(** Two positional method arguments, represented as a pair. *)
val a2 : 'a t -> 'b t -> ('a * 'b) args

(** Three positional method arguments, represented as a triple. *)
val a3 : 'a t -> 'b t -> 'c t -> ('a * 'b * 'c) args
