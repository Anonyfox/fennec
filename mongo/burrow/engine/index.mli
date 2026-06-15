(* Index — a secondary-index access method over one {!Catalog.index} sub-DB. An entry is
   [encoded-key-tuple ++ record-key -> record-key]: the record-key suffix makes entries unique and
   orders ties; the value is the clustered record-store key to fetch the document.

   Completeness — the invariant the planner relies on: every document yields at least one entry, so an
   index never has a false negative. A missing / empty / composite-only indexed field is indexed as
   Null; an array field is multikey-expanded over its scalar elements; a compound key is the cartesian
   product of its fields' value-sets. Over-produced candidates are filtered by the residual matcher, so
   the only requirement (never miss a match) holds. Descending fields are byte-complemented, so a
   forward scan of the sub-DB yields that field in descending value order. *)

module Store = Burrow_store.Store

val encode_value : Bson.t -> int -> string
(** Encode one field value for a direction (1 asc | -1 desc) — the planner builds scan bounds with
    this. A composite value collapses to the Null encoding (see completeness, above). *)

val keys_for_doc : Catalog.index -> Bson.t -> string list
(** The encoded key tuples for a document (multikey / compound expansion), deduped, without the
    record-key suffix. *)

val is_multikey_doc : Catalog.index -> Bson.t -> bool
(** Whether [doc] indexes an array for any of [idx]'s fields (so [idx] must be flagged multikey). *)

val add : [ `W ] Store.txn -> Catalog.index -> doc:Bson.t -> record_key:string -> unit
val remove : [ `W ] Store.txn -> Catalog.index -> doc:Bson.t -> record_key:string -> unit
(** The executor scans an index directly via {!Store.iter} over [(Catalog.index).db]; bound predicates
    live in the executor since they depend on the chosen plan. *)
