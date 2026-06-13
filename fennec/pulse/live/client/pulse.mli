(** The ambient client facade — Meteor's [Meteor.subscribe] / [Meteor.call] / [Meteor.connect] over
    the ONE page connection recorded by {!connect}. No [client] threading; pairs with the per-model
    [Ddp_client.Collection] views ([Tasks.find …]) to give the full Meteor surface. *)

(** Open the page connection (the one server a page talks to) and record it as the ambient default.
    Same options as {!Ddp_client.connect}; returns unit (use {!Ddp_client.default} for [close]). *)
val connect : ?path:string -> ?persist:string -> ?chrome:bool -> unit -> unit

(** Subscribe for the calling component's lifetime — auto-stops on cleanup; returns the [ready]
    signal. Meteor's [Meteor.subscribe]. *)
val use_subscribe : name:string -> ?params:Bson.t list -> unit -> bool Fur.signal

(** Subscribe with an explicit handle (call [.stop] yourself). *)
val subscribe : name:string -> ?params:Bson.t list -> unit -> Ddp_client.subscription

(** Call a typed method (Meteor's [Meteor.call]) — optimistic if it declares a stub. *)
val call : ('a, 'r) Method.t -> 'a -> ('r, string * string) result option Fur.signal

(** Connection state / buffered-write count, for offline affordances (ambient). *)
val status : unit -> [ `Connected | `Connecting | `Waiting ] Fur.signal

val pending_writes : unit -> int Fur.signal
