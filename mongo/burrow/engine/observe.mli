(* Observe — live-query observation (Meteor-style observeChanges): a registry of observers per
   collection. Each observer recomputes its query after every committed write and diffs the new result
   set against the previous one, emitting [added] / [changed] (changed fields + cleared field names) /
   [removed]. Because membership comes from a full recompute, it is correct for selector + projection +
   sort + skip + limit alike. Notification is synchronous within the writing call, so a write fence is
   satisfied immediately once the write returns. *)

type t

val create : unit -> t

val add :
  t ->
  coll:string ->
  recompute:(unit -> (string * Bson.t) list) ->
  added:(string -> Bson.t -> unit) ->
  changed:(string -> Bson.t -> string list -> unit) ->
  removed:(string -> unit) ->
  (unit -> unit)
(** Register an observer, fire its initial [added] burst from the current result, and return its
    unregister function. [recompute ()] yields the current [(id_string, projected fields-without-_id)]
    pairs for the observer's query. *)

val notify : t -> coll:string -> unit
(** Recompute + diff + emit for every observer on [coll]; call once after each committed write. *)

val has_observers : t -> coll:string -> bool
