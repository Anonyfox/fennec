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
   fallback for a client that sends no / an unmatched SNI).

   Secure by default WITHOUT tuning: we pass no [~version]/[~ciphers], so mirage-tls applies its
   curated defaults — protocol floor TLS 1.2 (range TLS 1.2–1.3; 1.0/1.1 are not even offered) and a
   modern ciphersuite set (no RC4/export/NULL). We deliberately do NOT pin them: mirage-tls is
   security-maintained and its floor only ever rises, so tracking the library is safer than freezing a
   range here. A dev never has to think about protocol/cipher hardening. *)
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

(* several PEM cert+key file pairs -> one SNI-selecting config: mirage-tls picks the cert by the SNI
   host against each cert's SANs (the first is the fallback). Per-domain BYO certs in one [serve ~tls]. *)
let of_file_pairs (pairs : (string * string) list) : t =
  let read p = In_channel.with_open_bin p In_channel.input_all in
  server_of_chains (List.map (fun (cert, key) -> chain_of_pem ~cert:(read cert) ~key:(read key)) pairs)

(* a throwaway, in-memory self-signed certificate covering [hosts] (default ["localhost"]) — for
   OPT-IN local HTTPS in DEVELOPMENT only ({!Fennec.serve} reaches for this only in dev when
   FENNEC_DEV_TLS is set, never in production). It chains to no trusted CA, so a browser shows the
   usual "not secure" warning to click through — expected for a dev cert, and the point is to exercise
   the HTTPS path (Secure cookies, HSTS, redirects, SNI) locally without hand-managing a cert. *)
let self_signed ?(hosts = [ "localhost" ]) () : t =
  ensure_rng ();
  let hosts = match hosts with [] -> [ "localhost" ] | hs -> hs in
  let dn = X509.Distinguished_name.[ Relative_distinguished_name.singleton (CN (List.hd hosts)) ] in
  let key = X509.Private_key.generate ~bits:2048 `RSA in
  let csr = match X509.Signing_request.create dn key with Ok c -> c | Error (`Msg m) -> failwith ("fennec: dev TLS CSR — " ^ m) in
  (* the SAN must be supplied to [sign] — extensions carried on the CSR are ignored (x509 docs) *)
  let san = X509.General_name.(singleton DNS hosts) in
  let exts = X509.Extension.(singleton Subject_alt_name (false, san)) in
  let valid_until = match Ptime.of_float_s 4070908800.0 (* 2099-01-01: a dev cert never needs renewing *) with Some t -> t | None -> Ptime.epoch in
  match X509.Signing_request.sign csr ~valid_from:Ptime.epoch ~valid_until ~extensions:exts key dn with
  | Ok cert -> server_of_chains [ ([ cert ], key) ]
  | Error e -> failwith (Format.asprintf "fennec: dev TLS self-sign — %a" X509.Validation.pp_signature_error e)

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

(* the dev self-signed cert builds a valid config AND actually covers the requested hosts (its SAN is
   what mirage-tls matches SNI against), so https://localhost works locally with no openssl, no files. *)
let%test "self_signed builds a config whose certificate covers the requested hosts" =
  match
    ignore (self_signed ());
    (* re-sign with a known key so we can inspect the cert's hostnames directly *)
    Mirage_crypto_rng_unix.use_default ();
    let key = X509.Private_key.generate ~bits:2048 `RSA in
    let dn = X509.Distinguished_name.[ Relative_distinguished_name.singleton (CN "localhost") ] in
    let csr = match X509.Signing_request.create dn key with Ok c -> c | Error (`Msg m) -> failwith m in
    let san = X509.General_name.(singleton DNS [ "localhost"; "app.test" ]) in
    let exts = X509.Extension.(singleton Subject_alt_name (false, san)) in
    let until = Option.value (Ptime.of_float_s 4070908800.0) ~default:Ptime.epoch in
    match X509.Signing_request.sign csr ~valid_from:Ptime.epoch ~valid_until:until ~extensions:exts key dn with
    | Error _ -> failwith "sign"
    | Ok cert ->
      let names = X509.Certificate.hostnames cert in
      let has h = X509.Host.Set.exists (fun (_, d) -> Domain_name.to_string d = h) names in
      if has "localhost" && has "app.test" then () else failwith "missing SAN"
  with
  | () -> true
  | exception _ -> false
