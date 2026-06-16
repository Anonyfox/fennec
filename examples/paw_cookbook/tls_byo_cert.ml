(* In-process TLS with your own certificate (no Let's Encrypt). [Force_https] adds HSTS and
   upgrades any plain request; [serve ~tls] terminates TLS in the server itself. *)

let app =
  Paw.Endpoint.make ~name:"app" ~hosts:[ "example.com" ] ()
  |> Paw.Endpoint.use (Paw.Force_https.make ~hsts:31536000 ())
  |> Paw.Endpoint.get "/" (fun c -> Paw.Conn.text c "secure")

(* Load a PEM cert + key from disk. Other flavors: [of_pem ~cert ~key] from in-memory strings,
   or [server_of_chains [c1; c2]] to present several certs and let SNI pick one per host. *)
let tls = Paw.Tls_termination.of_files ~cert:"/etc/ssl/example.com.pem" ~key:"/etc/ssl/example.com.key"

let () = Paw.serve ~tls [ app ]
