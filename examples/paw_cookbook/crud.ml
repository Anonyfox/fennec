(* A CRUD-shaped resource: list / show / create / update / delete. Read the input with [Paw.param]
   / [Paw.body_param], then pipe the response. [Method_override] lets an HTML form issue PUT and
   DELETE through a POST [_method] field. Bodies are stubs. *)

let app =
  Paw.endpoint ()
  |> Paw.use (Paw.Logger.make ())
  |> Paw.use (Paw.Method_override.make ())
  |> Paw.get "/todos" (fun c -> c |> Paw.json {|[{"id":1,"title":"buy milk"}]|})
  |> Paw.post "/todos" (fun c ->
         let title = Option.value (Paw.body_param c "title") ~default:"" in
         c |> Paw.set_status 201 |> Paw.json (Printf.sprintf {|{"id":2,"title":%S}|} title))
  |> Paw.get "/todos/:id" (fun c ->
         c |> Paw.json (Printf.sprintf {|{"id":%s}|} (Option.value (Paw.param c "id") ~default:"0")))
  |> Paw.put "/todos/:id" (fun c -> c |> Paw.json {|{"updated":true}|})
  |> Paw.delete "/todos/:id" (fun c -> c |> Paw.set_status 204 |> Paw.text "")

let () = Paw.serve [ app ]
