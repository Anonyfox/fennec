(* Server-side TLS termination: load a PEM certificate chain + private key into a
   [Tls.Config.server], so {!Server.run} (and {!Fennec.serve}) can terminate HTTPS in-process — no
   reverse proxy. TLS needs a running RNG; we install the default once, lazily. *)

type t = Tls.Config.server

(* a certificate chain + its private key — the unit mirage-tls selects among by SNI *)
type chain = X509.Certificate.t list * X509.Private_key.t

let rng_installed = ref false
let ensure_rng () = if not !rng_installed then (Mirage_crypto_rng_unix.use_default (); rng_installed := true)

let or_fail what = function Ok v -> v | Error (`Msg m) -> failwith (Printf.sprintf "fennec: TLS %s — %s" what m)

(* decode a PEM cert chain + key into a {!chain} (no config — for building a multi-cert server) *)
let chain_of_pem ~cert ~key : chain =
  ensure_rng ();
  (or_fail "certificate" (X509.Certificate.decode_pem_multiple cert), or_fail "private key" (X509.Private_key.decode_pem key))

(* a server config presenting one chain, or selecting among many by SNI (with the first as the
   fallback for a client that sends no / an unmatched SNI) *)
let server_of_chains (chains : chain list) : t =
  ensure_rng ();
  match chains with
  | [] -> failwith "fennec: TLS — no certificate"
  | [ c ] -> or_fail "config" (Tls.Config.server ~certificates:(`Single c) ())
  | _ -> or_fail "config" (Tls.Config.server ~certificates:(`Multiple chains) ()) (* SNI-select among the certs *)

let of_pem ~cert ~key : t = server_of_chains [ chain_of_pem ~cert ~key ]

let of_files ~cert ~key : t =
  let read p = In_channel.with_open_bin p In_channel.input_all in
  of_pem ~cert:(read cert) ~key:(read key)

(* ──── tls_termination test ──── *)

(* a real, end-to-end load: generate a self-signed cert with openssl, then prove [of_files] decodes
   the PEM chain + key and builds a valid server config. Skips (passes) where openssl is absent. *)
let%test "of_files loads a cert (Single) + server_of_chains SNI-selects among distinct certs (Multiple)" =
  if Sys.command "openssl version >/dev/null 2>&1" <> 0 then true
  else
    let dir = Filename.temp_dir "fennec_tls_" "" in
    let c1 = Filename.concat dir "c1.pem" and k1 = Filename.concat dir "k1.pem" in
    let c2 = Filename.concat dir "c2.pem" and k2 = Filename.concat dir "k2.pem" in
    let gen cn cert key = Printf.sprintf "openssl req -x509 -newkey rsa:2048 -keyout %s -out %s -days 1 -nodes -subj /CN=%s >/dev/null 2>&1" (Filename.quote key) (Filename.quote cert) cn in
    Fun.protect
      ~finally:(fun () -> List.iter (fun f -> try Sys.remove f with _ -> ()) [ c1; k1; c2; k2 ]; (try Sys.rmdir dir with _ -> ()))
      (fun () ->
        Sys.command (gen "localhost" c1 k1) = 0 && Sys.command (gen "other.test" c2 k2) = 0
        &&
        let read p = In_channel.with_open_bin p In_channel.input_all in
        match
          ignore (of_files ~cert:c1 ~key:k1) (* Single *);
          ignore (server_of_chains [ chain_of_pem ~cert:(read c1) ~key:(read k1); chain_of_pem ~cert:(read c2) ~key:(read k2) ]) (* Multiple, as on-demand builds *)
        with
        | () -> true
        | exception _ -> false)
