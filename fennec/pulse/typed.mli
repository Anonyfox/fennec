(** The typed collection runtime, server side — a functor over any reactive instance. Verbs
    delegate to the dynamic substrate; this layer adds only the boundary translation: writes
    VALIDATE (an invalid value raises {!Make.Invalid} with collected, path-tagged errors and never
    reaches the database), reads DECODE with the skip policy (documents that no longer match the
    declared shape are skipped — [find_results] exposes every verdict for code that must care).

    {[ module T = Fennec_pulse.Typed.Make (RData)
       let tasks = T.attach Task.collection backend          (* boot, next to publish *)
       let id = T.insert tasks { id = ""; title; done_ = false }
       let open_ = T.find tasks ~where:Filter.[ eq Task.Fields.done_ false ] ()
       T.update tasks ~where:Filter.[ eq Task.Fields.id id ] M.[ set Task.Fields.done_ true ] ]} *)

module Make (R : Reactive.REACTIVE) : sig
  (** Raised by a write whose value fails the shape's checks — the collected violations. *)
  exception Invalid of Codec.error list

  (** A typed handle: the pure declaration bound to this instance's collection. *)
  type 'a t

  (** Bind a declaration to this instance (boot-time) AND reconcile its declared indexes against the
      backend: create the missing, drop fennec-named orphans (idempotent; graceful — a failed build
      is logged unless [~strict], which fails boot). *)
  val attach : ?strict:bool -> 'a Def.t -> R.backend_collection -> 'a t

  (** Wrap an EXISTING reactive collection as a typed handle — cheap, no IO, no new mux uid (lets a
      facade cache one reactive collection and re-wrap per call). *)
  val of_reactive : 'a Def.t -> R.Collection.t -> 'a t

  (** Reconcile declared indexes against a reactive collection (the boot lifecycle step). *)
  val reconcile : ?strict:bool -> R.Collection.t -> Index.t list -> unit

  (** The dynamic collection underneath — the escape hatch (aggregations, raw queries). *)
  val collection : 'a t -> R.Collection.t

  val def : 'a t -> 'a Def.t

  (** The form-feedback primitive: every violation, without writing. *)
  val validate : 'a t -> 'a -> (unit, Codec.error list) result

  (** Validating insert: raises {!Invalid} rather than writing a bad document. Returns the [_id]. *)
  val insert : 'a t -> 'a -> string

  (** The substrate's reactive cursor (sorted/windowed) — plugs into [publish] unchanged.
      [?project] trims to a {!Proj.t}'s fields on the wire (the publication ships only those). *)
  val cursor :
    'a t -> ?where:Filter.t list -> ?sort:Sort.t -> ?skip:int -> ?limit:int -> ?project:_ Proj.t -> unit -> R.Collection.cursor

  (** A PROJECTED read: only the projection's fields cross the boundary, decoded into its object
      type (the full record is never built); malformed rows skipped. *)
  val find_p :
    'a t -> 'o Proj.t -> ?where:Filter.t list -> ?sort:Sort.t -> ?skip:int -> ?limit:int -> unit -> 'o list

  (** Typed read; documents that fail decode are SKIPPED (the malformed-doc policy). *)
  val find : 'a t -> ?where:Filter.t list -> ?sort:Sort.t -> ?skip:int -> ?limit:int -> unit -> 'a list

  (** Every decode verdict, for code that must care about malformed documents. *)
  val find_results :
    'a t -> ?where:Filter.t list -> ?sort:Sort.t -> ?skip:int -> ?limit:int -> unit -> ('a, Codec.error list) result list

  val find_one : 'a t -> ?where:Filter.t list -> ?sort:Sort.t -> unit -> 'a option
  val count : 'a t -> ?where:Filter.t list -> unit -> int

  (** Typed modifier update over a typed selector ([multi] defaults to [true]). *)
  val update : 'a t -> ?multi:bool -> where:Filter.t list -> M.t -> int

  val remove : 'a t -> where:Filter.t list -> int

  (** Typed upsert: update if matched, insert otherwise ([$setOnInsert] covers insert-only fields).
      Returns (number affected, newly-minted id if it inserted). *)
  val upsert : 'a t -> ?multi:bool -> where:Filter.t list -> M.t -> int * string option

  (** Distinct values of one field across matching docs, decoded to the field's type (undecodable
      values skipped). *)
  val distinct : 'a t -> 'b Codec.field -> ?where:Filter.t list -> unit -> 'b list
end
