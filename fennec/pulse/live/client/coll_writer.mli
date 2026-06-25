(** The collection write/read seam behind a model's generated [create] / [save] / [delete] / [find_one]
    / [where] / [all] / [count] (the Meteor-style methods on the module). Isomorphic, the sibling of
    {!Ddp_client}: the server installs the real backend at boot ([Fennec_pulse_app]); on the client these
    are stubs — a client changes data through a method, never the collection — that raise if called and
    are dead-code-eliminated when they aren't. *)

(** The installed backend — one polymorphic op per verb (a first-class-polymorphic record so one
    install covers every collection type). *)
type backend = {
  create : 'a. 'a Def.t -> 'a -> 'a;
  save : 'a. 'a Def.t -> 'a -> 'a;
  delete : 'a. 'a Def.t -> 'a -> unit;
  find_one : 'a. 'a Def.t -> Filter.t list -> 'a option;
  where : 'a. 'a Def.t -> Filter.t list -> 'a list;
  all : 'a. 'a Def.t -> 'a list;
  count : 'a. 'a Def.t -> Filter.t list -> int;
}

(** [install b] makes [b] the live backend (server boot). *)
val install : backend -> unit

val create : 'a Def.t -> 'a -> 'a
val save : 'a Def.t -> 'a -> 'a
val delete : 'a Def.t -> 'a -> unit
val find_one : 'a Def.t -> Filter.t list -> 'a option
val where : 'a Def.t -> Filter.t list -> 'a list
val all : 'a Def.t -> 'a list
val count : 'a Def.t -> Filter.t list -> int

(** The ambient sim for a method's optimistic slot. [with_sim w f] runs [f] (the slot body) with [w]
    bound as the current sim, so the model write verbs called inside predict against the local cache;
    the browser backend reads it via [current_sim]. Restored on exit. *)
val with_sim : Method.sim_writes -> (unit -> 'a) -> 'a
val current_sim : unit -> Method.sim_writes option
