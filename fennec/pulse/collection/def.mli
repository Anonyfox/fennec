(** The collection DECLARATION — pure and instance-free, shared by server and browser (what the
    [@@fennec.collection] deriver generates): name + shape + indexes. The server attaches it to a
    reactive instance at boot; the browser binds it to the live client. *)

type 'a t

val v : ?indexes:Index.t list -> string -> 'a Codec.t -> 'a t
val name : 'a t -> string
val codec : 'a t -> 'a Codec.t
val indexes : 'a t -> Index.t list

(** The mongod validator document derived from the shape ({!Schema.validator}). *)
val validator : 'a t -> Bson.t
