(* A minimal MongoDB wire-protocol CLIENT — connect to a wire endpoint (ours or any mongod) and run one
   command synchronously. Reuses {!Mongo_wire.Frame}: OP_MSG is symmetric, so the codec that frames a
   server reply frames a client request. One in-flight request per connection (synchronous request/reply).
   The replication follower's transport ({!Replication}); also handy for tooling and tests. *)

type t

val connect : ?auth:string * string -> sw:Eio.Switch.t -> net:'a Eio.Net.t -> Eio.Net.Sockaddr.stream -> t
(** Open a connection. [?auth:(user, password)] runs the SCRAM-SHA-256 handshake (with mutual server
    verification) before returning; omitted = plaintext, for loopback / trusted-network use. *)

val run : t -> Bson.t -> Bson.t
(** Run one command document (it must carry its own [$db] field for routing); returns the reply document. *)

val authenticate : t -> user:string -> password:string -> unit
(** The SCRAM-SHA-256 client handshake on an open connection (saslStart -> saslContinue -> verify the
    server's signature). [connect ?auth] calls this; exposed for a deferred/re-auth flow. Raises on a
    failed handshake or a server that fails mutual verification. *)
