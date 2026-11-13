(** Typed selectors over field handles — each combinator compiles to the Bson selector the engine
    already executes; field names/encodings come from the declaration (a renamed field is a compile
    error). Combine with {!all} (AND, the default reading of a list) / {!any} (OR); {!raw} keeps
    the full Mongo operator surface reachable.

    {[ open Task   (* brings the model's Fields into scope *)
       (* the [%q] DSL (recommended) — reads as a condition: *)
       let _ = Task.find ~where:[%q status = "doing" && priority >= 2 && assignee.email = "x"] ()
       (* the explicit combinator form (escape hatch): *)
       let _ = Filter.[ eq Fields.status "doing"; gte Fields.priority 2 ] ]}
*)

(** A list of selector clauses; a list of clauses means all of them must match. *)
type t = (string * Bson.t) list

(** The field equals the value ([$eq]). *)
val eq : 'a Sift.field -> 'a -> t

(** The field differs from the value ([$ne]). *)
val ne : 'a Sift.field -> 'a -> t

(** The field is less than the value ([$lt]). *)
val lt : 'a Sift.field -> 'a -> t

(** The field is less than or equal to the value ([$lte]). *)
val lte : 'a Sift.field -> 'a -> t

(** The field is greater than the value ([$gt]). *)
val gt : 'a Sift.field -> 'a -> t

(** The field is greater than or equal to the value ([$gte]). *)
val gte : 'a Sift.field -> 'a -> t

(** The field equals one of the values ([$in]). *)
val in_ : 'a Sift.field -> 'a list -> t

(** The field equals none of the values ([$nin]). *)
val nin : 'a Sift.field -> 'a list -> t

(** The field is present ([true]) or absent ([false]) ([$exists]). *)
val exists : 'a Sift.field -> bool -> t

(** Array membership: the list field contains this element. *)
val has : 'a list Sift.field -> 'a -> t

(** The list field contains ALL of these elements ($all). *)
val contains_all : 'a list Sift.field -> 'a list -> t

(** The list field has exactly [n] elements ($size). *)
val size : _ list Sift.field -> int -> t

(** [regex ?opts f re] — a string field matches the pattern ([opts] e.g. ["i"]). *)
val regex : ?opts:string -> string Sift.field -> string -> t

(** Negate a clause ($nor of one). *)
val not_ : t -> t

(** All of the clauses match ([$and]). *)
val all : t list -> t

(** At least one of the clauses matches ([$or]). *)
val any : t list -> t

(** An untyped selector document, as is (the escape hatch). *)
val raw : Bson.t -> t

(** The Mongo selector document. *)
val to_bson : t -> Bson.t
