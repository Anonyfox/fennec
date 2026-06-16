(* A stateful web app: signed-cookie sessions, CSRF protection, static asset serving from
   ./public, and a plain response cookie. Use one long random secret (here it's a placeholder). *)

let secret = "replace-with-a-long-random-secret"

let () =
  Paw.serve
    [ Paw.endpoint
        [ Paw.Logger.make ();
          Paw.Session.make ~secret ();
          Paw.Csrf.make ~secret ();
          Paw.Static.make (Paw.Static.Dir "./public");
          Paw.get "/" (fun c ->
              Paw.html (Paw.Conn.set_cookie c ~max_age:86400 "seen" "1") "<h1>welcome</h1>");
          Paw.post "/login" (fun c -> Paw.redirect c "/") ] ]
