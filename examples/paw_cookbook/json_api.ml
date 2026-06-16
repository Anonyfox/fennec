(* A small JSON microservice — a few routes, plus request logging and a correlation id.
   The handler bodies are stubs; the route + middleware shape is the point. *)

let () =
  Paw.serve
    [ Paw.endpoint
        [ Paw.Logger.make ();
          Paw.Request_id.make ();
          Paw.get "/health" (fun c -> Paw.json c {|{"status":"ok"}|});
          Paw.get "/greet/:name" (fun c ->
              let who = Option.value (Paw.param c "name") ~default:"stranger" in
              Paw.json c (Printf.sprintf {|{"hello":%S}|} who)) ] ]
