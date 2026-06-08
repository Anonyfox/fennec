(* Server-side TLS termination: load a PEM certificate chain + private key into a
   [Tls.Config.server], so {!Server.run} (and {!Fennec.serve}) can terminate HTTPS in-process — no
   reverse proxy. TLS needs a running RNG; we install the default once, lazily. *)

type t = Tls.Config.server

let rng_installed = ref false
let ensure_rng () = if not !rng_installed then (Mirage_crypto_rng_unix.use_default (); rng_installed := true)

let or_fail what = function Ok v -> v | Error (`Msg m) -> failwith (Printf.sprintf "fennec: TLS %s — %s" what m)

let of_pem ~cert ~key : t =
  ensure_rng ();
  let certs = or_fail "certificate" (X509.Certificate.decode_pem_multiple cert) in
  let priv = or_fail "private key" (X509.Private_key.decode_pem key) in
  or_fail "config" (Tls.Config.server ~certificates:(`Single (certs, priv)) ())

let of_files ~cert ~key : t =
  let read p = In_channel.with_open_bin p In_channel.input_all in
  of_pem ~cert:(read cert) ~key:(read key)

(* ──── tls_termination test ──── *)

(* a real, end-to-end load: generate a self-signed cert with openssl, then prove [of_files] decodes
   the PEM chain + key and builds a valid server config. Skips (passes) where openssl is absent. *)
let%test "of_files loads a self-signed cert + builds a server config" =
  if Sys.command "openssl version >/dev/null 2>&1" <> 0 then true
  else
    let dir = Filename.temp_dir "fennec_tls_" "" in
    let cert = Filename.concat dir "c.pem" and key = Filename.concat dir "k.pem" in
    let cmd =
      Printf.sprintf "openssl req -x509 -newkey rsa:2048 -keyout %s -out %s -days 1 -nodes -subj /CN=localhost >/dev/null 2>&1"
        (Filename.quote key) (Filename.quote cert)
    in
    Fun.protect
      ~finally:(fun () -> List.iter (fun f -> try Sys.remove f with _ -> ()) [ cert; key ]; (try Sys.rmdir dir with _ -> ()))
      (fun () -> Sys.command cmd = 0 && (match of_files ~cert ~key with (_ : t) -> true | exception _ -> false))
