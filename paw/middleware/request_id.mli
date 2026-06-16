(** Request id — tags each request with a unique id (reusing an inbound one for trace
    propagation), in a typed assign and a response header. Domain-safe: a one-time CSPRNG
    prefix + an atomic counter, so ids stay unique across worker domains.

    {[
      Endpoint.pipe [ Request_id.make (); Logger.make () ]

      (* read it downstream *)
      let id = Request_id.current conn
    ]} *)

(** Build the request-id paw. [header] (default ["x-request-id"]) is both the inbound header
    consulted for an existing id and the response header the id is echoed in. *)
val make : ?header:string -> unit -> Pipeline.t

(** The request id assigned by {!make} on this conn, if any. *)
val current : Conn.t -> string option
