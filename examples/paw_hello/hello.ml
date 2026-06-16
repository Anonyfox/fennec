(* The smallest complete server on fennec-paw — it links nothing else (see its dune:
   [fennec-paw eio_main]). [Paw.endpoint] bundles middleware + routes; the verbs are one [Paw.] away;
   the response pipes ([c |> Paw.html …]); and [Paw.serve] sits on its own line at the bottom.

   Run it:  [dune exec examples/paw_hello/hello.exe]
   then visit http://localhost:4000/  (dev default port; override with FENNEC_PORT). *)

let app =
  Paw.endpoint
    [ Paw.Logger.make ();
      Paw.get "/" (fun c -> c |> Paw.html "<h1>hello from fennec-paw</h1>");
      Paw.get "/users/:id" (fun c ->
          c |> Paw.text ("user " ^ Option.value (Paw.param c "id") ~default:"?")) ]

let () = Paw.serve [ app ]
