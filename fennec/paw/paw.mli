(** Paw — THE primitive: a paw touches a connection ([Conn.t -> Conn.t]), either declining
    (passing it through) or answering it (which short-circuits the rest of the pipeline).
    Middleware, a route, static serving, the websocket upgrade, the SSR app — all are paws.

    Two halves: the {b algebra} ({!seq}/{!pass}/{!run_conn}/{!run}) and the {b constructors}
    (the route verbs and {!fallthrough} — a route is just a paw guarded by method + path).
    The batteries (logger, auth, …) live in [Fennec_server.Plug]. *)

(** A paw is just a function; write one as [fun c -> …]. *)
type t = Conn.t -> Conn.t

(** {1 Algebra} *)

(** Compose paws left-to-right: each runs only while the conn is unanswered, so order is
    precedence and the first to answer wins. *)
val seq : t list -> t

(** The identity paw — declines everything; the unit of {!seq}, handy as a placeholder. *)
val pass : t

(** Run a pipeline over a request, returning the final conn (the server inspects it for an
    HTTP response or a websocket upgrade). *)
val run_conn : t -> Fennec_core.Http.request -> Conn.t

(** Run a pipeline to a response (an unanswered conn becomes a 404). Handy for pure tests. *)
val run : t -> Fennec_core.Http.request -> Fennec_core.Http.response

(** {1 Constructors} *)

(** A method+path route. The pattern may contain [:name] (captures one segment) and a
    trailing [*name] (captures the rest), read back with {!Conn.path_param}; a plain pattern
    is an exact match. The paw runs the handler on a match, else declines. *)
val on : Fennec_core.Http.meth -> string -> t -> t

val get : string -> t -> t
val post : string -> t -> t
val put : string -> t -> t
val delete : string -> t -> t
val patch : string -> t -> t

(** A paw from a [request -> response option] (e.g. static files): answers on [Some], else
    declines. *)
val fallthrough : (Fennec_core.Http.request -> Fennec_core.Http.response option) -> t
