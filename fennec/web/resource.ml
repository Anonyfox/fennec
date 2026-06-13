(* RESTful resources — the conventional 7 actions wired onto an Endpoint in one call, Rails-style.
   Only the handlers you pass are registered (omission = that route doesn't exist). Routes are
   ordered so the literal/longer ones win: [/posts/new] before [/posts/:id], [/posts/:id/edit]
   before [/posts/:id]. [update] answers BOTH PUT and PATCH. HTML forms (GET/POST only) reach
   update/destroy via the Method_override paw ([_method=PUT|DELETE]) — add it to the pipeline.

   Conventional routes for base ["/posts"]:
     index    GET    /posts
     new      GET    /posts/new
     create   POST   /posts
     edit     GET    /posts/:id/edit
     show     GET    /posts/:id
     update   PUT|PATCH /posts/:id
     destroy  DELETE /posts/:id
   Handlers read the id with [Action.path conn "id"]. *)

module Endpoint = Fennec_server.Endpoint
module Conn = Fennec_paw.Conn

type handler = Conn.t -> Conn.t

let crud ?index ?show ?new_ ?create ?edit ?update ?destroy (base : string) (ep : Endpoint.t) : Endpoint.t =
  let opt f x ep = match x with Some h -> f h ep | None -> ep in
  ep
  |> opt (fun h -> Endpoint.get base h) index
  |> opt (fun h -> Endpoint.get (base ^ "/new") h) new_ (* before /:id so "new" isn't read as an id *)
  |> opt (fun h -> Endpoint.post base h) create
  |> opt (fun h -> Endpoint.get (base ^ "/:id/edit") h) edit
  |> opt (fun h -> Endpoint.get (base ^ "/:id") h) show
  |> opt (fun h ep -> ep |> Endpoint.put (base ^ "/:id") h |> Endpoint.patch (base ^ "/:id") h) update
  |> opt (fun h -> Endpoint.delete (base ^ "/:id") h) destroy

(* ──────────────────────────── tests ──────────────────────────── *)

module H = Fennec_core.Http

let id_of c = Option.value ~default:"?" (Conn.path_param c "id")

let sample () =
  Endpoint.make ~name:"t" ()
  |> crud "/posts" ~index:(fun c -> Conn.text c "index")
       ~new_:(fun c -> Conn.text c "new") ~create:(fun c -> Conn.text c "created")
       ~edit:(fun c -> Conn.text c ("edit:" ^ id_of c))
       ~show:(fun c -> Conn.text c ("show:" ^ id_of c))
       ~update:(fun c -> Conn.text c ("updated:" ^ id_of c))
       ~destroy:(fun c -> Conn.text c ("destroyed:" ^ id_of c))

let body meth path =
  let h = Endpoint.handler (sample ()) in
  (Fennec_paw.Paw.run h (H.make_request ~meth ~path ())).Fennec_core.Http.body

let%test "crud wires the conventional 7 with correct precedence and methods" =
  body H.GET "/posts" = "index"
  && body H.GET "/posts/new" = "new" (* literal beats /:id *)
  && body H.POST "/posts" = "created"
  && body H.GET "/posts/42" = "show:42"
  && body H.GET "/posts/42/edit" = "edit:42"
  && body H.PUT "/posts/42" = "updated:42"
  && body H.PATCH "/posts/42" = "updated:42"
  && body H.DELETE "/posts/42" = "destroyed:42"

let%test "omitted actions are not registered (404)" =
  let ep = Endpoint.make ~name:"t" () |> crud "/posts" ~index:(fun c -> Conn.text c "index") in
  let h = Endpoint.handler ep in
  let status meth path = (Fennec_paw.Paw.run h (H.make_request ~meth ~path ())).Fennec_core.Http.status in
  status H.GET "/posts" = 200 && status H.POST "/posts" = 404 && status H.GET "/posts/1" = 404
