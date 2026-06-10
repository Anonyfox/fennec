(** The client-side merge store — the §5b design (DATAFLOW.md): the server forwards each
    subscription's observe deltas tagged with the sub id, and the {e client} merges them here. Per
    collection there is one Minimongo store of the winning (merged) documents; per [(collection, id)]
    a precedence view tracks which subscriptions include the doc and, per field, whose value wins
    (earliest subscription wins; on clear/remove the next subscription's value takes over). Pure
    (bson + minimongo) — runs native and in the browser. *)

(** A merge store across all subscribed collections. *)
type t

(** A fresh, empty merge store. *)
val create : unit -> t

(** [store t name] is the underlying Minimongo collection of winning documents for [name] (created
    empty if absent). Query it directly, or use {!fetch}. *)
val store : t -> string -> Minimongo.t

(** [version t name] — a counter bumped on every change to the collection (the change signal the Fur
    binding watches). *)
val version : t -> string -> int

(** [on_change t name f] registers [f] to run after each change to collection [name]; returns a
    listener id for {!off_change}. *)
val on_change : t -> string -> (unit -> unit) -> int

(** [off_change t name id] removes the listener. *)
val off_change : t -> string -> int -> unit

(** {2 Sub-tagged DDP data operations} *)

(** [added t ~sub ~collection ~id ~fields] — subscription [sub] now includes document [id] in
    [collection] with [fields]; merges with precedence. *)
val added :
  t -> sub:string -> collection:string -> id:string -> fields:(string * Bson.t) list -> unit

(** [changed t ~sub ~collection ~id ~fields ~cleared] — [sub] updated [fields] / unset [cleared] of
    an existing document; the client sees only the net change in winning values. *)
val changed :
  t -> sub:string -> collection:string -> id:string -> fields:(string * Bson.t) list -> cleared:string list -> unit

(** [removed t ~sub ~collection ~id] — [sub] no longer includes [id]; the document is dropped only
    when no remaining subscription covers it. *)
val removed : t -> sub:string -> collection:string -> id:string -> unit

(** [sub_stopped t sub] drops everything [sub] contributed (cost is O(that sub's documents)). *)
val sub_stopped : t -> string -> unit

(** {2 Queries & seeding} *)

(** [fetch t name ?selector ?sort ?skip ?limit ?fields ()] runs a Minimongo query over the merged
    collection and returns the matching documents. *)
val fetch :
  t ->
  string ->
  ?selector:Bson.t ->
  ?sort:Bson.t ->
  ?skip:int ->
  ?limit:int ->
  ?fields:Bson.t ->
  unit ->
  Bson.t array

(** [aggregate t name pipeline] runs an aggregation pipeline over collection [name]; [$lookup] /
    [$unionWith] foreign collections resolve across the client's OTHER collections, so joins span the
    local cache exactly as they do on the server. *)
val aggregate : t -> string -> Bson.t list -> Bson.t array

(** [seed t ~sub ~collection docs] installs [docs] as if delivered by one subscription — for SSR
    hydration (the inline payload becomes the client's initial cache). Seeded docs are {e tentative}
    until {!quiesce}. *)
val seed : t -> sub:string -> collection:string -> Bson.t list -> unit

(** [quiesce t sub] runs the post-hydration reconciliation for [sub], called on its first live
    [ready]: any document [sub] {!seed}ed but the live snapshot has not re-confirmed (via [added] /
    [changed]) is dropped — so a row deleted server-side between SSR and the socket opening does not
    linger as a stale fast-render artifact. A no-op for a sub that was never seeded. *)
val quiesce : t -> string -> unit

(** [resync_begin t sub] re-marks every document [sub] currently holds as tentative — call it on
    reconnect before resubscribing, so the resubscription's fresh snapshot re-confirms the docs that
    still exist and the following {!quiesce} (on the new [ready]) drops the ones the server stopped
    sending during the outage. Heals the cache after a dropped socket using the same machinery as the
    SSR seed. *)
val resync_begin : t -> string -> unit

(** [snapshot_sub t ~sub] extracts the subscription's current contribution, grouped by collection —
    the SEED format, so a persisted snapshot restores through the same seed → tentative → quiesce
    path SSR hydration uses (the next live [ready] prunes what died offline). Only the sub's own
    field values are taken, never another sub's (or a simulation's) overlays. The PWA persistence
    primitive. *)
val snapshot_sub : t -> sub:string -> (string * Bson.t list) list

(** [begin_sim t sub] registers [sub] as an optimistic SIMULATION: its precedence comes from a
    negative, descending band, so its field writes (the normal {!added}/{!changed} under this sub)
    win over every real subscription instantly, and a later simulation wins over an earlier one.
    Rollback is just {!sub_stopped} — the per-field precedence fallthrough reveals server truth with
    no bespoke undo machinery. *)
val begin_sim : t -> string -> unit

(** [sim_hide t ~sub ~collection ~id] optimistically DELETES a doc for the duration of simulation
    [sub]: it leaves the visible store, but its view survives — dropping the sim restores it (unless
    a real removal landed meanwhile, in which case it stays gone). Precedence can't express absence,
    so hiding is its own axis. *)
val sim_hide : t -> sub:string -> collection:string -> id:string -> unit
