(* TLS actually works: expose the wire endpoint behind a self-signed certificate, then have the REAL
   libmongoc driver connect over TLS (tls=true + tlsInsecure to accept the self-signed cert), do the
   SCRAM-SHA-256 handshake over the encrypted channel, and run CRUD. Skips when the native driver isn't
   available. *)

module Mongo = Fennec_mongo_dynamic
module Backend = Fennec_mongo_backend
module B = Bson

let tmpdir () =
  let d = Filename.concat (Filename.get_temp_dir_name ()) (Printf.sprintf "burrow_wiretls_%d" (Unix.getpid ())) in
  (try Unix.mkdir d 0o755 with Unix.Unix_error (Unix.EEXIST, _, _) -> ());
  d

(* a throwaway self-signed RSA cert for "localhost" — the client connects with tlsInsecure, so only the
   encryption matters, not the trust chain *)
let self_signed_tls () =
  Mirage_crypto_rng_unix.use_default ();
  let key = X509.Private_key.generate ~bits:2048 `RSA in
  let dn = X509.Distinguished_name.[ Relative_distinguished_name.singleton (CN "localhost") ] in
  let csr = Result.get_ok (X509.Signing_request.create dn key) in
  let now = Unix.gettimeofday () in
  let valid_from = Option.get (Ptime.of_float_s (now -. 86400.)) in
  let valid_until = Option.get (Ptime.of_float_s (now +. 31_536_000.)) in
  let cert = Result.get_ok (X509.Signing_request.sign csr ~valid_from ~valid_until key dn) in
  Result.get_ok (Tls.Config.server ~certificates:(`Single ([ cert ], key)) ())

let%test "libmongoc driver authenticates + runs CRUD over TLS (self-signed cert)" =
  if not (Mongo.available ()) then true
  else begin
    Eio_main.run @@ fun env ->
    Eio.Switch.run @@ fun sw ->
    let net = Eio.Stdenv.net env in
    let port = 41000 + (Unix.getpid () mod 4000) in
    Mongo.expose ~sw ~net
      ~addr:(`Tcp (Eio.Net.Ipaddr.V4.loopback, port))
      ~base_dir:(tmpdir ())
      ~users:[ Mongo.wire_user ~user:"ada" ~password:"lovelace" ]
      ~tls:(self_signed_tls ())
      ();
    let uri =
      Printf.sprintf "mongodb://ada:lovelace@127.0.0.1:%d/?authSource=admin&tls=true&tlsInsecure=true&serverSelectionTimeoutMS=8000" port
    in
    let conn = Mongo.connect uri in
    let c = Mongo.collection ~sw conn ~db:"sec" ~name:"vault" in
    ignore (Mongo.insert c (B.doc [ ("_id", B.str "a"); ("secret", B.int 42) ]));
    ignore (Mongo.insert c (B.doc [ ("_id", B.str "b"); ("secret", B.int 7) ]));
    Mongo.count c (B.doc [ ("secret", B.doc [ ("$gte", B.int 10) ]) ]) = 1
    && (match Mongo.find c (Backend.query ~selector:(B.doc [ ("_id", B.str "a") ]) ()) with
        | [ d ] -> B.get_int d "secret" = Some 42
        | _ -> false)
  end

let () = exit (Fennec_hunt_unit.run ())
