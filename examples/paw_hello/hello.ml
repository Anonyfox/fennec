(* The smallest complete server on fennec-paw — it links nothing else (see its dune:
   [fennec-paw eio_main]). [Paw.serve] builds the host router and runs the Eio acceptor,
   owning the event loop.

   Run it:  [dune exec examples/paw_hello/hello.exe]
   then visit http://localhost:4000/  (dev default port; override with FENNEC_PORT). *)

let () =
  Paw.serve
    [ Paw.Endpoint.make ~name:"hello" ~hosts:[ "*" ] ()
      |> Paw.Endpoint.use (Paw.Logger.make ())
      |> Paw.Endpoint.get "/" (fun c -> Paw.Conn.html c "<h1>hello from fennec-paw</h1>")
      |> Paw.Endpoint.get "/users/:id" (fun c ->
             Paw.Conn.text c ("user " ^ Option.value (Paw.Conn.path_param c "id") ~default:"?")) ]
