(* Automatic HTTPS via ACME / Let's Encrypt — production grade, zero cert files. [serve ~acme]
   obtains and renews certificates for the domains the router answers, serves the HTTP-01
   challenge, and runs an HTTP->HTTPS front on :80. Certificates persist in a pluggable store. *)

let app =
  Paw.endpoint ~hosts:[ "example.com"; "www.example.com" ] ()
  |> Paw.use (Paw.Logger.make ())
  |> Paw.get "/" (fun c -> c |> Paw.html "<h1>https, automatically</h1>")

let acme =
  Paw.Acme.auto ~email:"ops@example.com" ~store:(Paw.Cert_store.file ~dir:"/var/lib/myapp/acme") ()

let () = Paw.serve ~acme [ app ]

(* Local development: ~acme no-ops to plain HTTP in dev (a dev build never touches Let's Encrypt).
   To exercise the HTTPS path locally — Secure cookies, HSTS, redirects — run with FENNEC_DEV_TLS=1
   and serve terminates TLS on the dev port with a throwaway self-signed cert for localhost (the
   browser shows the usual self-signed warning to click through). Nothing to install, no cert files. *)
