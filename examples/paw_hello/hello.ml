(* The smallest complete server on fennec-paw — it links nothing else (see its dune:
   [fennec-paw eio_main]). [Paw.serve] builds the host router and owns the Eio loop; [Paw.endpoint]
   bundles middleware + routes; the verbs ([get], [html], [param]) are all one [Paw.] away.

   Run it:  [dune exec examples/paw_hello/hello.exe]
   then visit http://localhost:4000/  (dev default port; override with FENNEC_PORT). *)

let () =
  Paw.serve
    [ Paw.endpoint
        [ Paw.Logger.make ();
          Paw.get "/" (fun c -> Paw.html c "<h1>hello from fennec-paw</h1>");
          Paw.get "/users/:id" (fun c ->
              Paw.text c ("user " ^ Option.value (Paw.param c "id") ~default:"?")) ] ]
