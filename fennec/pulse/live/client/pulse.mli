(** The ambient client facade — Meteor's [Meteor.subscribe] / [Meteor.call] / [Meteor.connect] over
    the ONE page connection recorded by {!connect}. No [client] threading; pairs with the per-model
    [Pulse.Collection] views ([Tasks.find …]) to give the full Meteor surface. *)

(** Open the page connection (the one server a page talks to) and record it as the ambient default.
    Same options as {!Ddp_client.connect}; returns unit (use {!Ddp_client.default} for [close]). *)
val connect : ?path:string -> ?persist:string -> ?chrome:bool -> unit -> unit

(** Subscribe for the calling component's lifetime — auto-stops on cleanup; returns the [ready]
    signal. Meteor's [Meteor.subscribe]. (For an explicit stoppable handle, {!Ddp_client.subscribe}.) *)
val subscribe : name:string -> ?params:Bson.t list -> unit -> bool Fur.signal

(** Call a typed method (Meteor's [Meteor.call]) — optimistic if it declares a stub. *)
val call : ('a, 'r) Method.t -> 'a -> ('r, string * string) result option Fur.signal

(** Connection state / buffered-write count, for offline affordances (ambient). *)
val status : unit -> [ `Connected | `Connecting | `Waiting ] Fur.signal

val pending_writes : unit -> int Fur.signal

(** The per-model client view (Meteor's collection object): bind once, then call verbs with no
    handle. [module Tasks = Pulse.Collection (Task)] then [Tasks.find ~where:[%q …] ()] /
    [Tasks.project [%fields …] ()]. Reads only (writes go through methods). *)
module Collection (M : sig
  type doc
  val collection : doc Def.t
end) : sig
  val find : ?where:Q.t list -> ?sort:Sort.t -> ?skip:int -> ?limit:int -> unit -> M.doc array Fur.signal
  val project :
    'o Proj.t -> ?where:Q.t list -> ?sort:Sort.t -> ?skip:int -> ?limit:int -> unit -> 'o array Fur.signal
end
