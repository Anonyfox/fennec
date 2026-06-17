(** The compiled within-endpoint route dispatcher. An endpoint's declared routes are compiled ONCE
    (at serve) into a table whose per-request dispatch is an O(1) hash lookup on the request path
    (zero allocation, route-count-independent) plus — only when there are parameterised routes and
    the static lookup didn't answer — an ordered segment match over the few [:param]/[*splat] routes.
    The table DECLINES (returns the conn untouched) when nothing matches, so the endpoint falls
    through exactly as a hand-written paw chain would.

    Precedence: a static (exact) route beats a parameterised one for the same path (most-specific
    wins); among parameterised routes, declaration order. *)

(** A route as declared by a verb: its method, its literal pattern (e.g. ["/users/:id"]), and the
    userland handler (which may read captured path params off the conn). *)
type route = { meth : Http.meth; path : string; handler : Conn.t -> Conn.t }

(** The compiled table (opaque). *)
type t

(** [build routes] compiles a run of declared routes into the table. A duplicate (method, exact path)
    keeps the first declared (the historical first-match-wins behaviour); use {!conflicts} to reject
    such ambiguity at boot instead. *)
val build : route list -> t

(** [conflicts routes] lists duplicate (method, exact path) declarations as human-readable messages
    ([] when the run is clean) — for fail-at-boot validation by {!Fennec.serve} / {!Paw.serve}. *)
val conflicts : route list -> string list

(** [dispatch t] is the route paw: run per request, it answers the matching route's handler or
    declines (returns the conn untouched). Built once; allocates nothing on the static fast path. *)
val dispatch : t -> Conn.t -> Conn.t
