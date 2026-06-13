(** Paw — THE primitive: a paw touches a connection ([Conn.t -> Conn.t]), either declining
    (passing it through) or answering it (which short-circuits the rest of the pipeline).
    Middleware, a route, static serving, the websocket upgrade, the SSR app — all are paws.

    Two halves: the {b algebra} ({!seq}/{!pass}/{!run_conn}/{!run}) and the {b constructors}
    (the route verbs and {!fallthrough} — a route is just a paw guarded by method + path).
    The batteries (logger, auth, …) live in their own modules under the [Paw] namespace
    ([Paw.Logger], [Paw.Session], …), each a [make] returning one of these paws.

    Compose middleware and routes left-to-right with {!seq}; the first paw to answer wins and
    the rest are skipped. {!run} drives a pipeline to a response (unanswered becomes a 404),
    which is also the handle for pure tests:

    {[
      let app =
        seq
          [ get "/" (fun c -> Conn.html c "<h1>home</h1>");
            get "/users/:id" (fun c ->
              Conn.text c (Option.value (Conn.path_param c "id") ~default:"?"));
            post "/api/ping" (fun c -> Conn.json c {|{"pong":true}|}) ]

      let resp =
        run app (Fennec_core.Http.make_request ~meth:Fennec_core.Http.GET ~path:"/users/42" ())
    ]} *)

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

(** Route a GET request matching [pattern] to [handler]. Sugar for [on GET]. *)
val get : string -> t -> t

(** Route a POST request matching [pattern] to [handler]. *)
val post : string -> t -> t

(** Route a PUT request matching [pattern] to [handler]. *)
val put : string -> t -> t

(** Route a DELETE request matching [pattern] to [handler]. *)
val delete : string -> t -> t

(** Route a PATCH request matching [pattern] to [handler]. *)
val patch : string -> t -> t

(** A paw from a [request -> response option] (e.g. static files): answers on [Some], else
    declines. *)
val fallthrough : (Fennec_core.Http.request -> Fennec_core.Http.response option) -> t
