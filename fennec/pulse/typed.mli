(** The typed collection runtime, server side — a functor over any reactive instance. Verbs
    delegate to the dynamic substrate; this layer adds only the boundary translation: writes
    VALIDATE (an invalid value raises {!Make.Invalid} with collected, path-tagged errors and never
    reaches the database), reads DECODE with the skip policy (documents that no longer match the
    declared shape are skipped — [find_results] exposes every verdict for code that must care).

    {[ module T = Fennec_pulse.Typed.Make (RData)
       let tasks = T.attach Task.collection backend          (* boot, next to publish *)
       let id = T.insert tasks { id = ""; title; done_ = false }
       let open_ = T.find tasks ~where:Q.[ eq Task.Fields.done_ false ] ()
       T.update tasks ~where:Q.[ eq Task.Fields.id id ] M.[ set Task.Fields.done_ true ] ]} *)

module Make (R : Reactive.REACTIVE) : sig
  (** Raised by a write whose value fails the shape's checks — the collected violations. *)
  exception Invalid of Codec.error list

  (** A typed handle: the pure declaration bound to this instance's collection. *)
  type 'a t

  (** Bind a declaration to this instance (boot-time). *)
  val attach : 'a Def.t -> R.backend_collection -> 'a t

  (** The dynamic collection underneath — the escape hatch (aggregations, raw queries). *)
  val collection : 'a t -> R.Collection.t

  val def : 'a t -> 'a Def.t

  (** The form-feedback primitive: every violation, without writing. *)
  val validate : 'a t -> 'a -> (unit, Codec.error list) result

  (** Validating insert: raises {!Invalid} rather than writing a bad document. Returns the [_id]. *)
  val insert : 'a t -> 'a -> string

  (** The substrate's reactive cursor (sorted/windowed) — plugs into [publish] unchanged. *)
  val cursor : 'a t -> ?where:Q.t list -> ?sort:Bson.t -> ?skip:int -> ?limit:int -> unit -> R.Collection.cursor

  (** Typed read; documents that fail decode are SKIPPED (the malformed-doc policy). *)
  val find : 'a t -> ?where:Q.t list -> ?sort:Bson.t -> ?skip:int -> ?limit:int -> unit -> 'a list

  (** Every decode verdict, for code that must care about malformed documents. *)
  val find_results :
    'a t -> ?where:Q.t list -> ?sort:Bson.t -> ?skip:int -> ?limit:int -> unit -> ('a, Codec.error list) result list

  val find_one : 'a t -> ?where:Q.t list -> ?sort:Bson.t -> unit -> 'a option
  val count : 'a t -> ?where:Q.t list -> unit -> int

  (** Typed modifier update over a typed selector ([multi] defaults to [true]). *)
  val update : 'a t -> ?multi:bool -> where:Q.t list -> M.t -> int

  val remove : 'a t -> where:Q.t list -> int
end
