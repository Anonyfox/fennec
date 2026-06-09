(* End-to-end ACME wire proof against a local pebble server: obtain a REAL certificate through the
   full flow (directory → nonce → JWS account → order → HTTP-01 → finalize → download), proving the
   protocol — not just the crypto vectors. Gated on FENNEC_ACME_TEST_DIR so the normal suite skips it.

   Run pebble first (it validates HTTP-01 on :5002 by default, resolving the domain via system DNS):
     docker run --rm -d -p 14000:14000 -p 15000:15000 -e PEBBLE_VA_NOSLEEP=1 ghcr.io/letsencrypt/pebble
     FENNEC_ACME_TEST_DIR=https://localhost:14000/dir dune exec fennec/server/test/acme_pebble_test.exe *)

module C = Fennec_server.Acme_client

let () = Mirage_crypto_rng_unix.use_default () (* the account keygen below needs the RNG installed *)

let () =
  match Sys.getenv_opt "FENNEC_ACME_TEST_DIR" with
  | None -> print_endline "SKIP acme pebble e2e: set FENNEC_ACME_TEST_DIR=https://localhost:14000/dir (with pebble running)"; exit 0
  | Some directory ->
    let domain = Option.value (Sys.getenv_opt "FENNEC_ACME_TEST_DOMAIN") ~default:"host.docker.internal" in
    let http_port = int_of_string (Option.value (Sys.getenv_opt "FENNEC_ACME_TEST_HTTP_PORT") ~default:"5002") in
    Printf.eprintf "test: start dir=%s domain=%s port=%d\n%!" directory domain http_port;
    Eio_main.run @@ fun env ->
    let net = Eio.Stdenv.net env and clock = Eio.Stdenv.clock env in
    Eio.Switch.run @@ fun sw ->
    (* the HTTP-01 listener pebble's VA connects to (the manager's :80 listener, here on the VA port) *)
    let challenges : (string, string) Hashtbl.t = Hashtbl.create 8 in
    let sock = Eio.Net.listen ~sw ~reuse_addr:true ~backlog:16 net (`Tcp (Eio.Net.Ipaddr.V4.any, http_port)) in
    Eio.Fiber.fork ~sw (fun () ->
        let rec loop () =
          (try
             Eio.Net.accept_fork ~sw sock ~on_error:(fun _ -> ()) (fun flow _ ->
                 let r = Eio.Buf_read.of_flow flow ~max_size:8192 in
                 let line = try Eio.Buf_read.line r with _ -> "" in
                 let prefix = "/.well-known/acme-challenge/" in
                 let resp =
                   match String.split_on_char ' ' line with
                   | _ :: path :: _ when String.length path > String.length prefix && String.sub path 0 (String.length prefix) = prefix -> (
                     match Hashtbl.find_opt challenges (String.sub path (String.length prefix) (String.length path - String.length prefix)) with
                     | Some ka -> Printf.sprintf "HTTP/1.1 200 OK\r\nContent-Length: %d\r\nConnection: close\r\n\r\n%s" (String.length ka) ka
                     | None -> "HTTP/1.1 404 Not Found\r\nContent-Length: 0\r\nConnection: close\r\n\r\n")
                   | _ -> "HTTP/1.1 404 Not Found\r\nContent-Length: 0\r\nConnection: close\r\n\r\n"
                 in
                 try Eio.Flow.copy_string resp flow with _ -> ())
           with _ -> ());
          loop ()
        in
        loop ())
    ;
    Printf.eprintf "test: listening on :%d; generating account key\n%!" http_port;
    let account_key = X509.Private_key.generate ~bits:2048 `RSA in
    (* pebble uses a throwaway self-signed CA — trust anything for the test *)
    let null_auth ?ip:_ ~host:_ _ = Ok None in
    match
      C.obtain ~net ~clock ~authenticator:null_auth ~directory ~account_key ~email:"test@example.com" ~domains:[ domain ]
        ~provision:(fun ~token ~key_auth -> Hashtbl.replace challenges token key_auth)
        ~cleanup:(fun ~token -> Hashtbl.remove challenges token) ()
    with
    | Ok (cert_pem, key_pem) -> (
      match X509.Certificate.decode_pem_multiple cert_pem with
      | Ok (_ :: _) when String.length key_pem > 0 -> Printf.printf "PASS acme pebble e2e: obtained a valid %d-byte certificate chain for %s\n" (String.length cert_pem) domain; exit 0
      | _ -> print_endline "FAIL: certificate chain did not parse"; exit 1)
    | Error m -> Printf.printf "FAIL acme pebble e2e: %s\n" m; exit 1
