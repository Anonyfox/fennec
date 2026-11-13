(** Typed modifiers over field handles — the $-operators with the strings taken out of app code.
    Combine with {!all} (grouped by operator); {!raw} keeps the full surface reachable.

    {[ let _ = T.update tasks ~where:[%q id = x] [%set status = "done"]   (* the [%set] DSL *)
       let _ = Update.(all [ set Fields.status "done"; inc Fields.n 1; push Fields.tags "t" ]) ]}
*)

(** An update modifier. *)
type t

(** Set the field to the value ([$set]). *)
val set : 'a Sift.field -> 'a -> t

(** Remove the field ([$unset]). *)
val unset : 'a Sift.field -> t

(** Add to an integer field ([$inc]). *)
val inc : int Sift.field -> int -> t

(** Add to a float field ([$inc]). *)
val inc_f : float Sift.field -> float -> t

(** Append an element to a list field ([$push]). *)
val push : 'a list Sift.field -> 'a -> t

(** Append an element to a list field unless it is already present ([$addToSet]). *)
val add_to_set : 'a list Sift.field -> 'a -> t

(** Remove every occurrence of an element from a list field ([$pull]). *)
val pull : 'a list Sift.field -> 'a -> t

(** Remove every occurrence of each of the elements from a list field ([$pullAll]). *)
val pull_all : 'a list Sift.field -> 'a list -> t

(** Remove the first element of a list field ([$pop] -1). *)
val pop_first : _ list Sift.field -> t

(** Remove the last element of a list field ([$pop] 1). *)
val pop_last : _ list Sift.field -> t

(** Set the field to [v] only if [v] is less than the current value ($min). *)
val min : 'a Sift.field -> 'a -> t

(** Set the field to the value only if it is greater than the current one ([$max]). *)
val max : 'a Sift.field -> 'a -> t

(** Multiply an integer field ([$mul]). *)
val mul : int Sift.field -> int -> t

(** Multiply a float field ([$mul]). *)
val mul_f : float Sift.field -> float -> t

(** Set only on an upsert-insert ($setOnInsert). *)
val set_on_insert : 'a Sift.field -> 'a -> t

(** Rename a field to another field of the same type ($rename). *)
val rename : 'a Sift.field -> to_:'a Sift.field -> t

(** Combine several modifiers into one update. *)
val all : t list -> t

(** An untyped modifier document, as is (the escape hatch). *)
val raw : Bson.t -> t

(** The Mongo update document. *)
val to_bson : t -> Bson.t
