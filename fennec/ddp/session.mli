(** The server-side DDP session — transport-agnostic. It consumes decoded {!Message.t} values and
    produces them via [emit]; a websocket shell just pipes the bytes. Extended (sub-tagged) mode:
    the server is stateless per session — each subscription's observe deltas are forwarded tagged
    with the sub id and the client merges (DATAFLOW.md §5b). Publications and methods are
    caller-supplied closures, so this is pure and unit-testable with no socket. *)

(** Where a running publication streams its live deltas; [collection] is per-doc, so one publication
    can feed several collections. The session wires these to tagged [added]/[changed]/[removed]/
    [ready] messages. *)
type sink = {
  added : collection:string -> id:string -> fields:(string * Bson.t) list -> unit;
  changed :
    collection:string -> id:string -> fields:(string * Bson.t) list -> cleared:string list -> unit;
  removed : collection:string -> id:string -> unit;
  ready : unit -> unit;
}

(** A handle to a running publication; [stop] tears it down. *)
type handle = { stop : unit -> unit }

(** The per-subscription context a publication runs in. [user_id] is the connection's current
    Accounts user (None = anonymous), and [params] are the client's subscription arguments. *)
type publication_ctx = {
  user_id : string option;
  params : Bson.t list;
}

(** A publication: given a {!publication_ctx} and a {!sink}, start streaming and return a handle. *)
type publication = publication_ctx -> sink -> handle

(** The per-call context a method runs in: [user_id] is the connection's current user (None =
    anonymous), [set_user_id] rebinds it for the rest of the connection (a login method's job — the
    Meteor [this.setUserId]), and [random_seed] is the client's seed for deterministic id minting
    (latency compensation). *)
type method_ctx = {
  user_id : string option;
  set_user_id : string option -> unit;
  random_seed : Bson.t option;
}

(** A server method: a per-call {!method_ctx} and arguments to a result value. *)
type method_fn = method_ctx -> Bson.t list -> Bson.t

(** A {!method_fn} raises this for an application-level error (code + reason); the session maps it
    to the DDP error payload. Control exceptions ([Stack_overflow]/[Out_of_memory]) propagate; any
    other exception becomes a generic ["500"]. *)
exception Method_error of { code : string; reason : string }

(** Delta resync (v2, [Sub.have]): wraps a sink so the initial replay skips [added]s whose fields
    hash to what the client declared it holds, and [ready] emits explicit [removed] for held docs
    the replay did not cover (replacing the client's quiescence pass for that resubscription).
    Inert after [ready]. Applied automatically by {!dispatch}; exposed for tests. *)
val resync_wrap : have:(string * (string * string) list) list -> sink -> sink

(** A live session. *)
type t

(** [create ?user_id ?fence ~session_id ~emit ~pubs ~methods ()] builds a session: [emit] sends a
    message to the peer; [pubs]/[methods] are the registries the session dispatches [sub]/[method]
    against. [user_id] seeds the connection's authenticated user from an HTTP/browser handshake
    cookie before any method runs. [fence k] must run [k] only once the data deltas of
    already-committed writes have been DELIVERED to this session — the write fence that keeps
    [updated] from overtaking a method's own writes (default: immediate, for tests and fenceless
    transports). *)
val create :
  ?user_id:string ->
  ?fence:((unit -> unit) -> unit) ->
  session_id:string ->
  emit:(Message.t -> unit) ->
  pubs:(string, publication) Hashtbl.t ->
  methods:(string, method_fn) Hashtbl.t ->
  unit ->
  t

(** The connection's current authenticated user (None = anonymous). *)
val user_id : t -> string option

(** [dispatch t m] advances the session on an incoming message: [connect]→[connected], [sub]→ run
    the publication (sub-tagged deltas) + [ready] or [nosub], [unsub]→stop + [nosub],
    [method]→[result] + [updated], [ping]→[pong]. *)
val dispatch : t -> Message.t -> unit

(** [close t] stops all running subscriptions (the connection closed). *)
val close : t -> unit
