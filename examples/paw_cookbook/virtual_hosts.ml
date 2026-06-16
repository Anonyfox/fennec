(* Multiple apps on one server, routed by the Host header — one table, three endpoints: a JSON
   API, an admin panel behind HTTP Basic auth (the whole host is guarded), and a catch-all site. *)

let () =
  Paw.serve
    [ Paw.endpoint ~name:"api" ~hosts:[ "api.example.com" ]
        [ Paw.Logger.make ();
          Paw.get "/v1/ping" (fun c -> Paw.json c {|{"pong":true}|}) ];
      Paw.endpoint ~name:"admin" ~hosts:[ "admin.example.com" ]
        [ Paw.Basic_auth.make ~username:"admin" ~password:"s3cret" ();
          Paw.get "/" (fun c -> Paw.html c "<h1>admin</h1>") ];
      Paw.endpoint ~name:"site" ~hosts:[ "*" ]
        [ Paw.Static.make (Paw.Static.Dir "./public");
          Paw.get "/" (fun c -> Paw.html c "<h1>welcome</h1>") ] ]
