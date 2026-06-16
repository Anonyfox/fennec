(* A small JSON microservice — a few routes, plus request logging and a correlation id.
   The handler bodies are stubs; the route + middleware shape is the point. *)

let api =
  Paw.Endpoint.make ~name:"api" ~hosts:[ "*" ] ()
  |> Paw.Endpoint.use (Paw.Logger.make ())
  |> Paw.Endpoint.use (Paw.Request_id.make ())
  |> Paw.Endpoint.get "/health" (fun c -> Paw.Conn.json c {|{"status":"ok"}|})
  |> Paw.Endpoint.get "/greet/:name" (fun c ->
         let who = Option.value (Paw.Conn.path_param c "name") ~default:"stranger" in
         Paw.Conn.json c (Printf.sprintf {|{"hello":%S}|} who))

let () = Paw.serve [ api ]
