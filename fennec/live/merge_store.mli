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

(** [seed t ~sub ~collection docs] installs [docs] as if delivered by one subscription — for SSR
    hydration (the inline payload becomes the client's initial cache). *)
val seed : t -> sub:string -> collection:string -> Bson.t list -> unit
