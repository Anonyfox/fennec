(* Multiple apps on one server, routed by the Host header — one table, three endpoints: a JSON
   API, an admin panel behind HTTP Basic auth (the whole host is guarded), and a catch-all site. *)

let api =
  Paw.endpoint ~name:"api" ~hosts:[ "api.example.com" ]
    [ Paw.Logger.make ();
      Paw.get "/v1/ping" (fun c -> c |> Paw.json {|{"pong":true}|}) ]

let admin =
  Paw.endpoint ~name:"admin" ~hosts:[ "admin.example.com" ]
    [ Paw.Basic_auth.make ~username:"admin" ~password:"s3cret" ();
      Paw.get "/" (fun c -> c |> Paw.html "<h1>admin</h1>") ]

let site =
  Paw.endpoint ~name:"site" ~hosts:[ "*" ]
    [ Paw.Static.make (Paw.Static.Dir "./public");
      Paw.get "/" (fun c -> c |> Paw.html "<h1>welcome</h1>") ]

let () = Paw.serve [ api; admin; site ]
