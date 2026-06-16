(* Multiple apps on one server, routed by the Host header — one table, three endpoints: a JSON
   API, an admin panel behind HTTP Basic auth (the whole host is guarded), and a catch-all site. *)

let api =
  Paw.Endpoint.make ~name:"api" ~hosts:[ "api.example.com" ] ()
  |> Paw.Endpoint.use (Paw.Logger.make ())
  |> Paw.Endpoint.get "/v1/ping" (fun c -> Paw.Conn.json c {|{"pong":true}|})

let admin =
  Paw.Endpoint.make ~name:"admin" ~hosts:[ "admin.example.com" ] ()
  |> Paw.Endpoint.use (Paw.Basic_auth.make ~username:"admin" ~password:"s3cret" ())
  |> Paw.Endpoint.get "/" (fun c -> Paw.Conn.html c "<h1>admin</h1>")

let site =
  Paw.Endpoint.make ~name:"site" ~hosts:[ "*" ] ()
  |> Paw.Endpoint.use (Paw.Static.make (Paw.Static.Dir "./public"))
  |> Paw.Endpoint.get "/" (fun c -> Paw.Conn.html c "<h1>welcome</h1>")

let () = Paw.serve [ api; admin; site ]
