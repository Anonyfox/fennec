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

(** A publication: given [params] and a {!sink}, start streaming and return a {!handle}. *)
type publication = params:Bson.t list -> sink -> handle

(** A server method: arguments to a result value. *)
type method_fn = Bson.t list -> Bson.t

(** A {!method_fn} raises this for an application-level error (code + reason); the session maps it
    to the DDP error payload. Control exceptions ([Stack_overflow]/[Out_of_memory]) propagate; any
    other exception becomes a generic ["500"]. *)
exception Method_error of { code : string; reason : string }

(** A live session. *)
type t

(** [create ~session_id ~emit ~pubs ~methods] builds a session: [emit] sends a message to the peer;
    [pubs]/[methods] are the registries the session dispatches [sub]/[method] against. *)
val create :
  session_id:string ->
  emit:(Message.t -> unit) ->
  pubs:(string, publication) Hashtbl.t ->
  methods:(string, method_fn) Hashtbl.t ->
  t

(** [dispatch t m] advances the session on an incoming message: [connect]→[connected], [sub]→ run
    the publication (sub-tagged deltas) + [ready] or [nosub], [unsub]→stop + [nosub],
    [method]→[result] + [updated], [ping]→[pong]. *)
val dispatch : t -> Message.t -> unit

(** [close t] stops all running subscriptions (the connection closed). *)
val close : t -> unit
