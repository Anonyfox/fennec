(* The smallest complete server on fennec-paw — it links nothing else (see its dune:
   [fennec-paw eio_main]). Start at [Paw.endpoint], pipe middleware and routes onto it, and [Paw.serve]
   the result on its own line. The only nesting is the handler — it genuinely is nested.

   Run it:  [dune exec examples/paw_hello/hello.exe]
   then visit http://localhost:4000/  (dev default port; override with FENNEC_PORT). *)

let app =
  Paw.endpoint ()
  |> Paw.use (Paw.Logger.make ())
  |> Paw.get "/" (fun c -> c |> Paw.html "<h1>hello from fennec-paw</h1>")
  |> Paw.get "/users/:id" (fun c ->
         c |> Paw.text ("user " ^ Option.value (Paw.param c "id") ~default:"?"))

let () = Paw.serve [ app ]
