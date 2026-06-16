(* A complete HTTP server built on [fennec-paw] ALONE — this executable links no other fennec
   library (see its dune: [fennec-paw eio_main]). It is the shape of every paw app:

     1. build an {!Paw.Endpoint} from middleware + routes,
     2. put it behind a {!Paw.Host_router} (one table, routed by Host header),
     3. hand it to the Eio acceptor {!Paw.Server.run}.

   Run it:  [dune exec examples/paw_hello/hello.exe]
   then visit http://localhost:4000/  (dev default port; override with FENNEC_PORT). *)

let routes =
  Paw.Endpoint.make ~name:"hello" ~hosts:[ "*" ] ()
  (* always-run middleware, in order *)
  |> Paw.Endpoint.use (Paw.Logger.make ())
  |> Paw.Endpoint.use (Paw.Security_headers.make ())
  (* routes: [:id] captures one path segment, read back with [Conn.path_param] *)
  |> Paw.Endpoint.get "/" (fun c -> Paw.Conn.html c "<h1>hello from fennec-paw</h1>")
  |> Paw.Endpoint.get "/users/:id" (fun c ->
         Paw.Conn.text c ("user " ^ Option.value (Paw.Conn.path_param c "id") ~default:"?"))
  |> Paw.Endpoint.get "/health" (fun c -> Paw.Conn.json c {|{"ok":true}|})

let () =
  match Paw.Host_router.build [ (Paw.Endpoint.name routes, Paw.Endpoint.hosts routes, routes) ] with
  | Error _ ->
    prerr_endline "fennec-paw: invalid routing";
    exit 1
  | Ok router ->
    Eio_main.run @@ fun env ->
    let announce pairs = List.iter (fun (name, url) -> Printf.printf "  %s -> %s\n%!" name url) pairs in
    ignore (Paw.Server.run ~on_listen:announce ~env router)
