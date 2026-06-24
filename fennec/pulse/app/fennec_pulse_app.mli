(** The server data facade — the whole realtime data surface in ONE ambient module.

    It wraps the Reactive / server / Typed functors over the runtime-selectable {!Dynamic} backend
    (real MongoDB for a real global Mongo URL — [fennec dev] auto-starts one when mongod is
    available; [fennec test --mongo] supplies one per suite — or the in-memory engine for
    [MONGO_URL=:memory:]) so an app threads NO functors and NO backend instances. Declarations come
    from the [@@deriving collection] models; writes validate against them (an invalid value cannot
    reach the database); reads decode with the skip policy.

    The everyday server file is four lines — seed, publish, a method, the DDP paw (no lifecycle call):

    {[ module Pulse = Fennec_pulse_app

       let ddp = Pulse.serve_ddp ~path:"/ddp" ()           (* the websocket paw, at module init *)

       let setup () =                                       (* inside Fennec.serve ~on_start *)
         Pulse.seed Task.collection [ { Task.id = ""; title = "Buy milk"; body = "" } ];
         Pulse.publish Task.collection;                     (* ONE call: live cursor + SSR seed *)
         Pulse.method_ Site_methods.add_task (fun _inv title ->
             Pulse.insert Task.collection { Task.id = ""; title; body = "" })

       let () = Fennec.serve ~on_start:(fun ~sw:_ ~sleep:_ ~net:_ -> setup ()) [ web ] ]} *)

(** The production backend (mem-or-mongo, chosen by the global Mongo env). *)
module D = Fennec_mongo_dynamic.Dynamic

(** The reactive engine over {!D} — exposed so the publication/invocation/cursor types are nameable
    for advanced publications and method handlers. *)
module R : module type of Fennec_pulse.Reactive.Make (D)

(** The typed collection runtime over {!R} — exposed so [collection]'s [_ T.t] handle is nameable
    (the escape hatch for the full typed verb set: [find_one], [count], [distinct], [find_p], …). *)
module T : module type of Fennec_pulse.Typed.Make (R)

(** {1 Lifecycle} *)

(** The DDP websocket paw for the endpoint pipeline — the one server→client realtime channel. Safe to
    call at module-init time. The data layer needs no app-level start: [Fennec.serve] installs the ambient
    Eio switch and (for a [burrow://] URL with an authority) opens the [mongosh] wire endpoint at boot,
    all decided by [MONGO_URL]. *)
val serve_ddp : ?path:string -> unit -> Paw.t

(** {1 Collections} *)

(** The typed handle for a declaration — cached per name (one reactive collection, indexes reconciled
    once on first use), re-wrapped cheaply per call. The escape hatch when a verb below isn't enough. *)
val collection : 'a Def.t -> 'a T.t

(** {1 Server writes} — used inside method handlers (the one client write path). Each validates
    against the declaration and raises {!T.Make.Invalid} on a bad value rather than writing it. *)

(** Validating insert; returns the new [_id]. *)
val insert : 'a Def.t -> 'a -> string

(** Seed-if-empty: insert the bootstrap documents, but ONLY when the collection is currently empty.
    Idempotent, so it is safe to call unconditionally from [on_start] on every boot. A durable dev DB
    (burrow) is seeded once and survives `fennec dev` restarts without accumulating duplicates; a fresh
    [:memory:] / temp e2e DB is always empty, so it re-seeds every run for a clean slate. To force a
    re-seed of a durable dev DB, reset it first with `fennec clean --db`. *)
val seed : 'a Def.t -> 'a list -> unit

val update : 'a Def.t -> ?multi:bool -> where:Filter.t list -> Update.t -> int
val upsert : 'a Def.t -> ?multi:bool -> where:Filter.t list -> Update.t -> int * string option
val remove : 'a Def.t -> where:Filter.t list -> int

(** {1 Publications and methods} *)

(** [publish ?where def] registers, in ONE call, both the live DDP publication (the server→client
    cursor) AND the flicker-free SSR seed, keyed by the collection's name. [?where] maps the
    subscription params to typed clauses (read as AND); the default publishes the whole collection. *)
val publish : ?where:(Bson.t list -> Filter.t list) -> 'a Def.t -> unit

(** Register a typed method handler — the single client write path. The handler shares the method's
    declarations with the client stub, so a renamed field/method is a compile error in every file. *)
val method_ : ('a, 'r) Method.t -> (R.invocation -> 'a -> 'r) -> unit

(** {1 The non-web core} — re-exported so a server / collections / workflows file opens ONE module. *)

(** Run a block in the transparent transaction: commit on return, roll back on raise, with
    read-your-writes throughout. Re-entrant — a nested call joins the enclosing transaction. *)
val transaction : (unit -> 'a) -> 'a

(** Workflows — ordinary functions wrapped in the transaction, carrying their [before]/[after]
    reactions. See {!Fennec_pulse_workflow.Workflow}. *)
module Workflow : module type of Fennec_pulse_workflow.Workflow

(** Recurring jobs that run at-most-once across replicas — the [@every] / [@cron] targets. *)
module Schedule : sig
  (** [every seconds ~name body] runs [body] every [seconds] (one slot per duration window). *)
  val every : float -> name:string -> (unit -> unit) -> unit

  (** [cron expr ~name body] runs [body] on a 5-field UTC crontab schedule. *)
  val cron : string -> name:string -> (unit -> unit) -> unit

  (** [cron_matches expr t] — does [expr] fire at unix time [t] (UTC)? (Exposed for tests.) *)
  val cron_matches : string -> float -> bool
end

(** Collections' write API — [create] / [delete] / named [transition]s (never raw field mutation),
    each a {!Workflow} that runs in the transparent transaction and carries its [@after] reactions.
    Reads are one-shot ([get]/[all]/[find]); live data is a {!publish}ed publication. *)
module Collection : sig
  (** [get def id] — the aggregate with [_id = id], or [None]. *)
  val get : 'a Def.t -> string -> 'a option

  (** All aggregates (a one-shot server read). *)
  val all : 'a Def.t -> 'a list

  (** [find def ~where] — aggregates matching [where] (a one-shot server read). *)
  val find : 'a Def.t -> where:Filter.t list -> 'a list

  (** [put def v] persists a full aggregate by its [_id], validating it (raises {!Invalid} on a bad
      value, rolling back the enclosing transaction). The low-level write transitions build on. *)
  val put : 'a Def.t -> 'a -> unit

  (** [create def] — the insert workflow: stores a fresh aggregate and returns it with its minted id. *)
  val create : 'a Def.t -> ('a, 'a) Workflow.t

  (** [delete def] — the delete workflow: removes the aggregate by its [_id]. *)
  val delete : 'a Def.t -> ('a, unit) Workflow.t

  (** [transition def name f] — a named state change [f : 'a -> 'a], persisted by [_id], as a workflow.
      [f] may [raise] to veto (the transaction rolls back). [@after] reactions attach to its result. *)
  val transition : 'a Def.t -> string -> ('a -> 'a) -> ('a, 'a) Workflow.t
end
