(* A stateful web app: signed-cookie sessions, CSRF protection, static asset serving from
   ./public, and a plain response cookie. Use one long random secret (here it's a placeholder). *)

let secret = "replace-with-a-long-random-secret"

let web =
  Paw.Endpoint.make ~name:"web" ~hosts:[ "*" ] ()
  |> Paw.Endpoint.use (Paw.Logger.make ())
  |> Paw.Endpoint.use (Paw.Session.make ~secret ())
  |> Paw.Endpoint.use (Paw.Csrf.make ~secret ())
  |> Paw.Endpoint.use (Paw.Static.make (Paw.Static.Dir "./public"))
  |> Paw.Endpoint.get "/" (fun c ->
         Paw.Conn.html (Paw.Conn.set_cookie c ~max_age:86400 "seen" "1") "<h1>welcome</h1>")
  |> Paw.Endpoint.post "/login" (fun c -> Paw.Conn.redirect c "/")

let () = Paw.serve [ web ]
