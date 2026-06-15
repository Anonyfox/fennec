(** Managed single-node MongoDB replica sets for Fennec CLI dev/test sessions.

    The CLI has one exported database knob for spawned applications: [MONGO_URL]. This module owns
    the lifecycle that fills it when a real local [mongod] is wanted. *)

(** A MongoDB process owned by this CLI process, or an already-running dev instance adopted on a
    stable port. Adopted instances have no owned pid and are not stopped on teardown. *)
type t

(** [start ?port ?dbpath ()] starts a single-node replica set and waits until it is PRIMARY. With a
    fixed [port] and [dbpath], a process already answering on that port is adopted after replica-set
    initialization succeeds. The returned value is not exported automatically; call {!export}. *)
val start : ?port:int -> ?dbpath:string -> unit -> (t, string) result

(** Set [MONGO_URL] in the current process so future children inherit this MongoDB instance. *)
val export : t -> unit

(** Connection string exported by {!export}. *)
val uri : t -> string

(** TCP port. *)
val port : t -> int

(** Data directory. *)
val dbpath : t -> string

(** Owned process id, if this process spawned the mongod. *)
val pid : t -> int option

(** Stop the instance if this process owns it. No-op for adopted dev instances. *)
val stop : t -> unit

(** Compatibility helper for explicit [--mongo]-style callers: start an ephemeral instance, export
    it, print diagnostics, and degrade to [None] on failure. *)
val launch : unit -> t option

(** [ensure_dev ~root ~base_port ()] is the default [fennec dev] behavior. If [MONGO_URL] is already
    set it does nothing (an explicit URL — real mongo, [:memory:], or [:embedded:] — always wins).
    Otherwise it points [MONGO_URL] at the in-process embedded engine ([:embedded:] + a gitignored,
    per-base-port [_build/.fennec/burrow/dev-<base_port>] directory): always available, no [mongod] to
    install or run. Returns [None] (there is no process to own); a real local mongod is opt-in via an
    explicit [MONGO_URL] or [fennec test --mongo]. *)
val ensure_dev : root:string -> base_port:int -> unit -> t option
