(* A CRUD-shaped resource: list / show / create / update / delete. [Method_override] lets an
   HTML form issue PUT and DELETE through a POST [_method] field. Bodies are stubs. *)

let todos =
  Paw.Endpoint.make ~name:"todos" ~hosts:[ "*" ] ()
  |> Paw.Endpoint.use (Paw.Logger.make ())
  |> Paw.Endpoint.use (Paw.Method_override.make ())
  |> Paw.Endpoint.get "/todos" (fun c -> Paw.Conn.json c {|[{"id":1,"title":"buy milk"}]|})
  |> Paw.Endpoint.post "/todos" (fun c ->
         let title = Option.value (Paw.Conn.body_param c "title") ~default:"" in
         Paw.Conn.json ~status:201 c (Printf.sprintf {|{"id":2,"title":%S}|} title))
  |> Paw.Endpoint.get "/todos/:id" (fun c ->
         let id = Option.value (Paw.Conn.path_param c "id") ~default:"0" in
         Paw.Conn.json c (Printf.sprintf {|{"id":%s}|} id))
  |> Paw.Endpoint.put "/todos/:id" (fun c -> Paw.Conn.json c {|{"updated":true}|})
  |> Paw.Endpoint.delete "/todos/:id" (fun c -> Paw.Conn.text ~status:204 c "")

let () = Paw.serve [ todos ]
