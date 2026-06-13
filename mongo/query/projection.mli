(** Field projection — true nested (dotted-path) include/exclude, plus the array projection
    operators [$slice] and [$elemMatch]. Takes the projection spec as a BSON document (Minimongo's
    [fields] / [projection] option).

    A spec compiles to a path tree, so [{"a.b": 1}] keeps only [a.b] (the rest of [a] is dropped),
    matching MongoDB. Include vs exclude mode is decided by the first plain [0]/[1] field (other than
    [_id]); [_id] is kept unless explicitly set to [0]. [{"arr": {$slice: 3}}] / [{$slice: [s, n]}]
    limits an array; [{"arr": {$elemMatch: sel}}] keeps the first array element matching [sel]. The
    positional projection operator [$] is not supported (it needs the query selector).

    {[
      (* how Minimongo's [fetch] projects each result document *)
      let proj = Projection.of_fields (Bson.Document [ ("name", Int 1); ("address.city", Int 1) ]) in
      let visible = List.map (Projection.apply proj) docs in   (* keeps _id, name, address.city *)
      (* observeChanges: don't report an unset of a field the projection hides *)
      let reportable = Projection.cleared proj [ "name"; "secret" ]   (* = ["name"] *)
    ]} *)

(** A compiled projection (opaque). Build it with {!of_fields}. *)
type t

(** Compile a projection spec document into a {!t}. A non-document or empty spec yields the identity
    projection (returns documents unchanged). *)
val of_fields : Bson.t -> t

(** [apply proj d] projects document [d]: in include mode, keeps only the selected paths (plus [_id]
    unless excluded); in exclude mode, drops the selected paths. Nested paths and [$slice]/
    [$elemMatch] are applied per the compiled tree. *)
val apply : t -> Bson.t -> Bson.t

(** [cleared proj names] filters a list of unset/cleared field names down to those the projection
    would actually surface — so unsetting a hidden field is not reported to a client. *)
val cleared : t -> string list -> string list
