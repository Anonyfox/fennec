(** The application as a single Plug-style pipeline. One shape — [plug = conn ->
    conn] — composes routes, middleware, static serving, and the 404. The runner
    folds plugs left-to-right and short-circuits the moment one answers, so order
    in the pipe is precedence. Pure: [run] is [request -> response]. *)

type conn = { req : Http.request; resp : Http.response option }
type plug = conn -> conn

(** The empty pipeline. Build with [|> use …] (or the verb helpers below, which
    are themselves plugs passed to {!use}). *)
type t = plug list

val empty : t

(** Append a plug. An already-answered conn passes through untouched, so the order
    of [use] calls is the order of precedence. *)
val use : plug -> t -> t

(** Run the pipeline over a request; an unanswered conn becomes a 404. *)
val run : t -> Http.request -> Http.response

(** {1 Plug constructors — the userland verbs} *)

(** Exact method + path routes. HEAD is matched as GET (the body is stripped
    downstream). *)
val get : string -> (Http.request -> Http.response) -> plug
val post : string -> (Http.request -> Http.response) -> plug
val put : string -> (Http.request -> Http.response) -> plug
val delete : string -> (Http.request -> Http.response) -> plug

(** A middleware that may answer early: [Some resp] halts the pipeline, [None]
    passes through. (e.g. auth → 403.) *)
val filter : (Http.request -> Http.response option) -> plug

(** A fallthrough that answers only when it matches (e.g. static files). *)
val fallthrough : (Http.request -> Http.response option) -> plug

(** Mount a universal page router; the first matching page wins. *)
val pages : (Http.request -> Http.response) Routes.route list -> plug

(** A terminal plug that always answers (e.g. a custom 404 at the tail). *)
val always : (Http.request -> Http.response) -> plug

(** Define a page route with the routes combinators:
    [App.page Routes.(s "tasks" / str /? nil) (fun id req -> …)]. *)
val page :
  ('a, Http.request -> Http.response) Routes.path ->
  'a ->
  (Http.request -> Http.response) Routes.route
