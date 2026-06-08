(* Automatic HTTPS via ACME (Let's Encrypt). Derives the certifiable domains from the host router
   (done in {!Fennec.serve}), solves HTTP-01 on a dedicated :80 listener, persists the account key +
   issued cert in a {!Cert_store}, and keeps a LIVE cert the server reads per connection — renewed
   before expiry with zero-downtime hot-reload (no restart, no dropped connections).

   Only concrete (Exact) hostnames are auto-certable via HTTP-01; wildcards need DNS-01 and a dynamic
   catch-all needs on-demand TLS — both out of scope here and reported clearly rather than failing. *)

let letsencrypt_prod = "https://acme-v02.api.letsencrypt.org/directory"
let letsencrypt_staging = "https://acme-staging-v02.api.letsencrypt.org/directory"

type config = { email : string; store : Cert_store.t; staging : bool; domains : string list option; directory : string option }

let default_dir () =
  match Sys.getenv_opt "FENNEC_ACME_DIR" with
  | Some d -> d
  | None ->
    let state = match Sys.getenv_opt "XDG_STATE_HOME" with Some x -> x | None -> Filename.concat (Option.value (Sys.getenv_opt "HOME") ~default:".") ".local/state" in
    Filename.concat state "fennec/acme"

let auto ~email ?store ?(staging = false) ?domains ?directory () =
  { email; store = (match store with Some s -> s | None -> Cert_store.file ~dir:(default_dir ())); staging; domains; directory }

let domains_override cfg = cfg.domains

type t = { cfg : config; challenges : (string, string) Hashtbl.t; cert : Tls.Config.server option ref }

(* the account key, persisted so the same ACME account is reused across restarts/replicas *)
let account_key store =
  match Cert_store.(store.get "account.pem") with
  | Some pem -> ( match X509.Private_key.decode_pem pem with Ok k -> k | Error (`Msg m) -> failwith ("acme: stored account key — " ^ m))
  | None ->
    Mirage_crypto_rng_unix.use_default ();
    let k = X509.Private_key.generate ~bits:2048 `RSA in
    Cert_store.(store.put "account.pem" (X509.Private_key.encode_pem k));
    k

(* days until the stored cert expires (None if no/invalid cert) *)
let cert_days_left store ~clock =
  match Cert_store.(store.get "cert.pem") with
  | None -> None
  | Some pem -> (
    match X509.Certificate.decode_pem_multiple pem with
    | Ok (c :: _) ->
      let _, not_after = X509.Certificate.validity c in
      let now = Option.value (Ptime.of_float_s (Eio.Time.now clock)) ~default:Ptime.epoch in
      ( match Ptime.Span.to_int_s (Ptime.diff not_after now) with Some secs -> Some (secs / 86400) | None -> Some 0)
    | _ -> None)

(* install the stored cert as the live one (hot-reloadable: the server reads [t.cert] per connection) *)
let load_cert t =
  match (Cert_store.(t.cfg.store.get "cert.pem"), Cert_store.(t.cfg.store.get "key.pem")) with
  | Some cert, Some key -> ( try t.cert := Some (Tls_termination.of_pem ~cert ~key) with _ -> ())
  | _ -> ()

(* the HTTP response for one challenge request line — pure, so it's unit-testable without binding :80 *)
let challenge_response challenges request_line =
  let prefix = "/.well-known/acme-challenge/" in
  let token =
    match String.split_on_char ' ' request_line with
    | _ :: path :: _ when String.length path > String.length prefix && String.sub path 0 (String.length prefix) = prefix ->
      Some (String.sub path (String.length prefix) (String.length path - String.length prefix))
    | _ -> None
  in
  match Option.bind token (Hashtbl.find_opt challenges) with
  | Some ka -> Printf.sprintf "HTTP/1.1 200 OK\r\nContent-Type: application/octet-stream\r\nContent-Length: %d\r\nConnection: close\r\n\r\n%s" (String.length ka) ka
  | None -> "HTTP/1.1 404 Not Found\r\nContent-Length: 0\r\nConnection: close\r\n\r\n"

(* the dedicated :80 HTTP-01 challenge listener (always on, for issuance + renewals); independent of
   the app's :443 so app routing can't shadow it *)
let serve_challenges ~sw ~net t =
  match (try Some (Eio.Net.listen ~sw ~reuse_addr:true ~backlog:64 net (`Tcp (Eio.Net.Ipaddr.V4.any, 80))) with _ -> None) with
  | None -> Printf.eprintf "fennec acme: could not bind :80 for HTTP-01 (need privilege / port free) — issuance will fail\n%!"
  | Some sock ->
    Eio.Fiber.fork ~sw (fun () ->
        let rec loop () =
          (try
             Eio.Net.accept_fork ~sw sock ~on_error:(fun _ -> ()) (fun flow _ ->
                 let r = Eio.Buf_read.of_flow flow ~max_size:8192 in
                 let line = try Eio.Buf_read.line r with _ -> "" in
                 try Eio.Flow.copy_string (challenge_response t.challenges line) flow with _ -> ())
           with _ -> ());
          loop ()
        in
        loop ())

(* obtain (or, for a non-leaseholder replica, wait for) the cert, then install it *)
let issue t ~clock ~net ~domains =
  let directory = match t.cfg.directory with Some d -> d | None -> if t.cfg.staging then letsencrypt_staging else letsencrypt_prod in
  let key = account_key t.cfg.store in
  let did =
    Cert_store.(
      t.cfg.store.with_lease "acme-issue" (fun () ->
          match
            Acme_client.obtain ~net ~clock ~directory ~account_key:key ~email:t.cfg.email ~domains
              ~provision:(fun ~token ~key_auth -> Hashtbl.replace t.challenges token key_auth)
              ~cleanup:(fun ~token -> Hashtbl.remove t.challenges token) ()
          with
          | Ok (cert_pem, key_pem) ->
            t.cfg.store.put "cert.pem" cert_pem;
            t.cfg.store.put "key.pem" key_pem;
            Printf.eprintf "fennec acme: issued certificate for %s\n%!" (String.concat ", " domains)
          | Error m -> Printf.eprintf "fennec acme: issuance failed for %s — %s\n%!" (String.concat ", " domains) m))
  in
  (* another replica holds the lease and is issuing — wait for the cert to land in the shared store *)
  if not did then (
    let rec wait n = if n <= 0 then () else match Cert_store.(t.cfg.store.get "cert.pem") with Some _ -> () | None -> Eio.Time.sleep clock 2.0; wait (n - 1) in
    wait 60);
  load_cert t

(* the full lifecycle, called from {!Fennec.serve}'s on_start (which runs before the server binds).
   [cert_ref] is the live source the server's TLS wrap reads. *)
let run ~sw ~clock ~net ~domains (cfg : config) (cert_ref : Tls.Config.server option ref) =
  match domains with
  | [] -> Printf.eprintf "fennec acme: no concrete (Exact) domain to certify — HTTPS disabled (wildcards need DNS-01; the catch-all needs on-demand TLS)\n%!"
  | _ ->
    let t = { cfg; challenges = Hashtbl.create 8; cert = cert_ref } in
    serve_challenges ~sw ~net t;
    (match cert_days_left cfg.store ~clock with Some d when d > 30 -> load_cert t | _ -> issue t ~clock ~net ~domains);
    Eio.Fiber.fork ~sw (fun () ->
        let rec loop () =
          Eio.Time.sleep clock (12. *. 3600.);
          (match cert_days_left cfg.store ~clock with Some d when d > 30 -> () | _ -> issue t ~clock ~net ~domains);
          loop ()
        in
        loop ())

(* ──── acme manager tests ──── *)

let%test "HTTP-01 listener serves a provisioned token, 404s everything else" =
  let has ~sub s =
    let n = String.length sub and m = String.length s in
    let rec go i = i + n <= m && (String.sub s i n = sub || go (i + 1)) in
    n = 0 || go 0
  in
  let c = Hashtbl.create 4 in
  Hashtbl.replace c "tokenAAA" "tokenAAA.keyauthZZZ";
  let ok = challenge_response c "GET /.well-known/acme-challenge/tokenAAA HTTP/1.1" in
  let miss = challenge_response c "GET /.well-known/acme-challenge/unknown HTTP/1.1" in
  let other = challenge_response c "GET /index.html HTTP/1.1" in
  has ~sub:"200 OK" ok && has ~sub:"tokenAAA.keyauthZZZ" ok && has ~sub:"404" miss && has ~sub:"404" other
