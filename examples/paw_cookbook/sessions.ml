(* A stateful web app: signed-cookie sessions, CSRF protection, static asset serving from ./public,
   and a plain response cookie — note the pipe: set the cookie, then answer. One long random secret. *)

let secret = "replace-with-a-long-random-secret"

let app =
  Paw.endpoint ()
  |> Paw.use (Paw.Logger.make ())
  |> Paw.use (Paw.Session.make ~secret ())
  |> Paw.use (Paw.Csrf.make ~secret ())
  |> Paw.use (Paw.Static.make (Paw.Static.Dir "./public"))
  |> Paw.get "/" (fun c -> c |> Paw.set_cookie ~max_age:86400 "seen" "1" |> Paw.html "<h1>welcome</h1>")
  |> Paw.post "/login" (fun c -> c |> Paw.redirect "/")

let () = Paw.serve [ app ]
