(* fennec-paw benchmark server — the shared contract, written in the idiomatic flat-pipe DX.
   Run in production mode on all cores: FENNEC_ENV=production FENNEC_PORT=8080 ./server.exe *)
let () =
  Paw.serve
    [
      Paw.endpoint ()
      |> Paw.get "/plaintext" (fun c -> Paw.text "Hello, World!" c)
      |> Paw.get "/json" (fun c -> Paw.json {|{"message":"Hello, World!"}|} c)
      |> Paw.get "/user/:id" (fun c -> Paw.text (Option.value (Paw.param c "id") ~default:"") c);
    ]
