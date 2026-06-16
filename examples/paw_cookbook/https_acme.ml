(* Automatic HTTPS via ACME / Let's Encrypt — production grade, zero cert files. [serve ~acme]
   obtains and renews certificates for the domains the router answers, serves the HTTP-01
   challenge, and runs an HTTP->HTTPS front on :80. Certificates persist in a pluggable store. *)

let app =
  Paw.endpoint ~hosts:[ "example.com"; "www.example.com" ]
    [ Paw.Logger.make ();
      Paw.get "/" (fun c -> Paw.html c "<h1>https, automatically</h1>") ]

let acme =
  Paw.Acme.auto ~email:"ops@example.com" ~store:(Paw.Cert_store.file ~dir:"/var/lib/myapp/acme") ()

let () = Paw.serve ~acme [ app ]
