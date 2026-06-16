(* A small JSON microservice — a few routes, plus request logging and a correlation id.
   The handler bodies are stubs; the route + middleware shape is the point. *)

let app =
  Paw.endpoint
    [ Paw.Logger.make ();
      Paw.Request_id.make ();
      Paw.get "/health" (fun c -> c |> Paw.json {|{"status":"ok"}|});
      Paw.get "/greet/:name" (fun c ->
          let who = Option.value (Paw.param c "name") ~default:"stranger" in
          c |> Paw.json (Printf.sprintf {|{"hello":%S}|} who)) ]

let () = Paw.serve [ app ]
