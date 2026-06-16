(* A CRUD-shaped resource: list / show / create / update / delete. [Method_override] lets an
   HTML form issue PUT and DELETE through a POST [_method] field. Bodies are stubs. *)

let () =
  Paw.serve
    [ Paw.endpoint
        [ Paw.Logger.make ();
          Paw.Method_override.make ();
          Paw.get "/todos" (fun c -> Paw.json c {|[{"id":1,"title":"buy milk"}]|});
          Paw.post "/todos" (fun c ->
              let title = Option.value (Paw.body_param c "title") ~default:"" in
              Paw.json ~status:201 c (Printf.sprintf {|{"id":2,"title":%S}|} title));
          Paw.get "/todos/:id" (fun c ->
              Paw.json c (Printf.sprintf {|{"id":%s}|} (Option.value (Paw.param c "id") ~default:"0")));
          Paw.put "/todos/:id" (fun c -> Paw.json c {|{"updated":true}|});
          Paw.delete "/todos/:id" (fun c -> Paw.text ~status:204 c "") ] ]
