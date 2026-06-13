(** RESTful resources — the conventional 7 actions wired onto an {!Fennec_server.Endpoint} in one
    call, Rails-style. Only the handlers you pass are registered (omission = no such route). Routes
    are ordered so literals win ([/posts/new] before [/posts/:id]); [update] answers both PUT and
    PATCH. HTML forms reach update/destroy via the [Method_override] paw ([_method=PUT|DELETE]) — add
    it to the pipeline. Handlers read the id with [Action.path conn "id"].

    Conventional routes for base ["/posts"]: index [GET /posts], new [GET /posts/new],
    create [POST /posts], edit [GET /posts/:id/edit], show [GET /posts/:id],
    update [PUT|PATCH /posts/:id], destroy [DELETE /posts/:id].

    {[ let ep =
         Endpoint.make ~name:"web" ()
         |> Method_override.make () |> Endpoint.use
         |> Resource.crud "/posts"
              ~index:(fun c -> View.document c (Posts_view.index (all ())))
              ~create:(fun c ->
                match Form.parse Post.codec c with
                | Ok p -> ignore (save p); Conn.redirect c "/posts"
                | Error errs -> View.document ~status:422 c (Posts_view.form ~conn:c ~errors:errs))
              ~show:(fun c -> match find (Action.path c "id") with Some p -> View.document c (Posts_view.show p) | None -> Conn.text ~status:404 c "Not found") ]} *)

module Endpoint = Fennec_server.Endpoint
module Conn = Fennec_paw.Conn

(** A resource action handler. Reads the id (where relevant) with [Action.path conn "id"]. *)
type handler = Conn.t -> Conn.t

(** Wire the conventional REST routes for [base] onto the endpoint. Pass only the actions you want. *)
val crud :
  ?index:handler ->
  ?show:handler ->
  ?new_:handler ->
  ?create:handler ->
  ?edit:handler ->
  ?update:handler ->
  ?destroy:handler ->
  string ->
  Endpoint.t ->
  Endpoint.t
