(* A minimal self-signed HTTPS server for the hunt TLS proof — pure OCaml (tls-eio + x509),
   no openssl, no python. Generates a cert in-process and serves a fixed JSON body on :8443.
   Spawned by tls_check.ml via `hunt ~spawn`. *)

let () = Mirage_crypto_rng_unix.use_default ()

let own_cert () =
  let key = X509.Private_key.generate `RSA in
  let dn = X509.Distinguished_name.[ Relative_distinguished_name.singleton (CN "localhost") ] in
  let csr = Result.get_ok (X509.Signing_request.create dn key) in
  let valid_from = Ptime.epoch in
  let valid_until = Option.get (Ptime.add_span Ptime.epoch (Ptime.Span.of_int_s (100 * 365 * 86400))) in
  let cert = Result.get_ok (X509.Signing_request.sign csr ~valid_from ~valid_until key dn) in
  `Single ([ cert ], key)

let () =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let net = Eio.Stdenv.net env in
  let cfg = Result.get_ok (Tls.Config.server ~certificates:(own_cert ()) ()) in
  let sock = Eio.Net.listen ~sw ~backlog:16 ~reuse_addr:true net (`Tcp (Eio.Net.Ipaddr.V4.loopback, 8443)) in
  Eio.Net.run_server sock ~on_error:(fun _ -> ()) (fun flow _addr ->
      let tls = Tls_eio.server_of_flow cfg flow in
      let r = Eio.Buf_read.of_flow tls ~max_size:65536 in
      (* drain the request line + headers up to the blank line *)
      let rec drain () = match Eio.Buf_read.line r with "" | "\r" -> () | _ -> drain () in
      (try drain () with _ -> ());
      let body = {|{"secure":true,"via":"tls"}|} in
      let resp =
        Printf.sprintf "HTTP/1.1 200 OK\r\nContent-Type: application/json\r\nContent-Length: %d\r\nConnection: close\r\n\r\n%s"
          (String.length body) body
      in
      try Eio.Flow.copy_string resp tls with _ -> ())
