(** The server data facade — the whole realtime data surface in ONE ambient module.

    It wraps the Reactive / server / Typed functors over the runtime-selectable {!Dynamic} backend
    (real MongoDB for a real global Mongo URL — [fennec dev] auto-starts one when mongod is
    available; [fennec test --mongo] supplies one per suite — or the in-memory engine for
    [MONGO_URL=:memory:]) so an app threads NO functors and NO backend instances. Declarations come
    from the [@@deriving collection] models; writes validate against them (an invalid value cannot
    reach the database); reads decode with the skip policy.

    The everyday server file is five lines — start, seed, publish, a method, the DDP paw:

    {[ module Pulse = Fennec_pulse_app

       let ddp = Pulse.serve_ddp ~path:"/ddp" ()           (* the websocket paw, at module init *)

       let setup ~sw =                                      (* inside Fennec.serve ~on_start *)
         Pulse.start ~sw ~db:"app" ();
         Pulse.seed Task.collection [ { Task.id = ""; title = "Buy milk"; body = "" } ];
         Pulse.publish Task.collection;                     (* ONE call: live cursor + SSR seed *)
         Pulse.method_ Site_methods.add_task (fun _inv title ->
             Pulse.insert Task.collection { Task.id = ""; title; body = "" })

       let () = Fennec.serve ~on_start:(fun ~sw ~sleep:_ -> setup ~sw) [ web ] ]} *)

(** The production backend (mem-or-mongo, chosen by the global Mongo env). *)
module D = Fennec_pulse_mongo.Dynamic

(** The reactive engine over {!D} — exposed so the publication/invocation/cursor types are nameable
    for advanced publications and method handlers. *)
module R : module type of Fennec_pulse.Reactive.Make (D)

(** The typed collection runtime over {!R} — exposed so [collection]'s [_ T.t] handle is nameable
    (the escape hatch for the full typed verb set: [find_one], [count], [distinct], [find_p], …). *)
module T : module type of Fennec_pulse.Typed.Make (R)

(** {1 Lifecycle} *)

(** [start ~sw ~db ()] records the ambient config (the Eio switch + database name) consumed by every
    subsequent collection. Call it once, first, inside [Fennec.serve ~on_start]. *)
val start : sw:Eio.Switch.t -> db:string -> unit -> unit

(** The DDP websocket paw for the endpoint pipeline — the one server→client realtime channel.
    Safe to call at module-init time (it does not need {!start}). *)
val serve_ddp : ?path:string -> unit -> Fennec_paw.Paw.t

(** {1 Collections} *)

(** The typed handle for a declaration — cached per name (one reactive collection, indexes reconciled
    once on first use), re-wrapped cheaply per call. The escape hatch when a verb below isn't enough. *)
val collection : 'a Def.t -> 'a T.t

(** {1 Server writes} — used inside method handlers (the one client write path). Each validates
    against the declaration and raises {!T.Make.Invalid} on a bad value rather than writing it. *)

(** Validating insert; returns the new [_id]. *)
val insert : 'a Def.t -> 'a -> string

(** Insert many (seed/bootstrap) — [List.iter insert]. *)
val seed : 'a Def.t -> 'a list -> unit

val update : 'a Def.t -> ?multi:bool -> where:Filter.t list -> M.t -> int
val upsert : 'a Def.t -> ?multi:bool -> where:Filter.t list -> M.t -> int * string option
val remove : 'a Def.t -> where:Filter.t list -> int

(** {1 Publications and methods} *)

(** [publish ?where def] registers, in ONE call, both the live DDP publication (the server→client
    cursor) AND the flicker-free SSR seed, keyed by the collection's name. [?where] maps the
    subscription params to typed clauses (read as AND); the default publishes the whole collection. *)
val publish : ?where:(Bson.t list -> Filter.t list) -> 'a Def.t -> unit

(** Register a typed method handler — the single client write path. The handler shares the method's
    declarations with the client stub, so a renamed field/method is a compile error in every file. *)
val method_ : ('a, 'r) Method.t -> (R.invocation -> 'a -> 'r) -> unit
