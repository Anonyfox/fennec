(* The application as a single Plug-style pipeline. ONE shape composes everything:

     type plug = conn -> conn

   A [conn] carries the request and an optional response. A plug either passes the
   conn through unchanged (declining) or sets [resp] (answering). The runner folds
   the plugs left-to-right and SHORT-CIRCUITS the moment one sets a response — so a
   route, static serving, middleware, and the 404 are all just plugs, tried in
   order. This is Elixir's Plug, minus the macros: routes are plugs that match the
   path, middleware are plugs that may answer early, and `|>` is the pipeline.

   [run] is pure (request -> response): the whole framework, testable without a
   socket. The Eio server (Fennec_server) is a thin adapter on top. *)

type conn = { req : Http.request; resp : Http.response option }
type plug = conn -> conn

let answered (c : conn) = c.resp <> None

(* set the response on a conn (an answering plug's terminal move) *)
let respond (c : conn) (r : Http.response) : conn = { c with resp = Some r }

(* ---- the pipeline ---- *)

type t = plug list

let empty : t = []

(* append a plug to the pipeline; a plug already-answered conn passes through
   untouched, so order in the pipe is precedence *)
let use (p : plug) (t : t) : t = t @ [ (fun c -> if answered c then c else p c) ]

(* run the pipeline over a request; an unanswered conn at the end is a 404 *)
let run (t : t) (req : Http.request) : Http.response =
  let final = List.fold_left (fun c p -> p c) { req; resp = None } t in
  match final.resp with Some r -> r | None -> Http.text ~status:404 "404 Not Found"

(* ---- plug constructors (the userland verbs) ---- *)

(* HEAD is matched as GET so static/pages answer it; the responder strips the
   body downstream *)
let meth_matches (want : Http.meth) (got : Http.meth) =
  got = want || (want = Http.GET && got = Http.HEAD)

(* an exact method+path route *)
let route (m : Http.meth) (path : string) (h : Http.request -> Http.response) : plug =
 fun c -> if meth_matches m c.req.Http.meth && c.req.Http.path = path then respond c (h c.req) else c

let get path h : plug = route Http.GET path h
let post path h : plug = route Http.POST path h
let put path h : plug = route Http.PUT path h
let delete path h : plug = route Http.DELETE path h

(* a "filter" middleware: may answer early (e.g. auth -> 403), else passes through.
   [f req] returns [Some resp] to halt or [None] to continue. *)
let filter (f : Http.request -> Http.response option) : plug =
 fun c -> match f c.req with Some r -> respond c r | None -> c

(* a fallthrough that answers only when it has something (e.g. static files) *)
let fallthrough (f : Http.request -> Http.response option) : plug =
 fun c -> match f c.req with Some r -> respond c r | None -> c

(* mount a universal page router (anuragsoni/routes): the first matching page wins *)
let pages (routes : (Http.request -> Http.response) Routes.route list) : plug =
  let router = Routes.one_of routes in
  fun c ->
    match Routes.match' router ~target:c.req.Http.path with
    | Routes.FullMatch h | Routes.MatchWithTrailingSlash h -> respond c (h c.req)
    | Routes.NoMatch -> c

(* a terminal handler that always answers (e.g. a custom 404 at the end) *)
let always (h : Http.request -> Http.response) : plug = fun c -> respond c (h c.req)

(* define a page route with the routes combinators:
   App.page Routes.(s "tasks" / str /? nil) (fun id req -> ...) *)
let page = Routes.( @--> )
