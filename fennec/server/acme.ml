(* Automatic HTTPS via ACME (Let's Encrypt). Derives the certifiable domains from the host router
   (done in {!Fennec.serve}), solves HTTP-01 on a dedicated :80 listener, persists the account key +
   issued cert in a {!Cert_store}, and keeps a LIVE cert the server reads per connection — renewed
   before expiry with zero-downtime hot-reload (no restart, no dropped connections).

   Only concrete (Exact) hostnames are auto-certable via HTTP-01; wildcards need DNS-01 and a dynamic
   catch-all needs on-demand TLS — both out of scope here and reported clearly rather than failing. *)

let letsencrypt_prod = "https://acme-v02.api.letsencrypt.org/directory"
let letsencrypt_staging = "https://acme-staging-v02.api.letsencrypt.org/directory"

(* a DNS provider for DNS-01 (wildcard certs): set/remove a TXT record. The app implements it for
   its provider (Cloudflare / Route 53 / …) — no provider SDKs baked into fennec, same seam idea as
   {!Cert_store}. [name] is the full record name, e.g. "_acme-challenge.app.com". *)
type dns_provider = { upsert_txt : name:string -> value:string -> unit; remove_txt : name:string -> unit }

type config = {
  email : string option;
  store : Cert_store.t;
  staging : bool;
  domains : string list option;
  directory : string option;
  dns_provider : dns_provider option;
  on_demand : (string -> bool) option; (* allowlist for on-demand issuance: approve an SNI host? *)
}

let default_dir () =
  match Sys.getenv_opt "FENNEC_ACME_DIR" with
  | Some d -> d
  | None ->
    let state = match Sys.getenv_opt "XDG_STATE_HOME" with Some x -> x | None -> Filename.concat (Option.value (Sys.getenv_opt "HOME") ~default:".") ".local/state" in
    Filename.concat state "fennec/acme"

let env_true v = match Sys.getenv_opt v with Some ("1" | "on" | "true" | "yes") -> true | _ -> false

(* [auto] never raises — a missing email just means HTTPS stays off (logged at run), so a dev build
   with no FENNEC_ACME_EMAIL boots fine on plain HTTP. Env overrides code: FENNEC_ACME_EMAIL,
   FENNEC_ACME_STAGING. *)
let auto ?email ?store ?staging ?domains ?directory ?dns_provider ?on_demand () =
  let email = match email with Some _ -> email | None -> Sys.getenv_opt "FENNEC_ACME_EMAIL" in
  let staging = match staging with Some s -> s | None -> env_true "FENNEC_ACME_STAGING" in
  { email; store = (match store with Some s -> s | None -> Cert_store.file ~dir:(default_dir ())); staging; domains; directory; dns_provider; on_demand }

let domains_override cfg = cfg.domains
let dns_enabled cfg = Option.is_some cfg.dns_provider

type chain = Tls_termination.chain

(* the result of running ACME: the live TLS source the server reads per connection (SNI-selecting
   among all current certs), plus an optional on-demand handler (ensure a cert for an SNI host). *)
type running = { source : unit -> Tls.Config.server option; on_demand : (string -> unit) option }

type t = {
  cfg : config;
  challenges : (string, string) Hashtbl.t; (* shared with the :80 front *)
  chains : (string, chain) Hashtbl.t; (* label → cert chain: "managed" = the declared SAN; a host = on-demand *)
  domains_of : (string, string list) Hashtbl.t; (* label → the domains it covers, for renewal *)
}

let cert_key label = label ^ ".cert.pem"
let key_key label = label ^ ".key.pem"

(* the live TLS source: SNI-select among all current chains; None until the first cert lands *)
let source t () = match Hashtbl.fold (fun _ c acc -> c :: acc) t.chains [] with [] -> None | cs -> Some (Tls_termination.server_of_chains cs)

(* the account key, persisted so the same ACME account is reused across restarts/replicas *)
let account_key store =
  match Cert_store.(store.get "account.pem") with
  | Some pem -> ( match X509.Private_key.decode_pem pem with Ok k -> k | Error (`Msg m) -> failwith ("acme: stored account key — " ^ m))
  | None ->
    Mirage_crypto_rng_unix.use_default ();
    let k = X509.Private_key.generate ~bits:2048 `RSA in
    Cert_store.(store.put "account.pem" (X509.Private_key.encode_pem k));
    k

(* days until the stored cert for [label] expires (None if no/invalid cert) *)
let cert_days_left store ~label ~clock =
  match Cert_store.(store.get (cert_key label)) with
  | None -> None
  | Some pem -> (
    match X509.Certificate.decode_pem_multiple pem with
    | Ok (c :: _) ->
      let _, not_after = X509.Certificate.validity c in
      let now = Option.value (Ptime.of_float_s (Eio.Time.now clock)) ~default:Ptime.epoch in
      ( match Ptime.Span.to_int_s (Ptime.diff not_after now) with Some secs -> Some (secs / 86400) | None -> Some 0)
    | _ -> None)

(* install the stored chain for [label] into the live set (hot-reloadable: source reads it per conn) *)
let load_chain t ~label =
  match (Cert_store.(t.cfg.store.get (cert_key label)), Cert_store.(t.cfg.store.get (key_key label))) with
  | Some cert, Some key -> ( try Hashtbl.replace t.chains label (Tls_termination.chain_of_pem ~cert ~key) with _ -> ())
  | _ -> ()

(* the HTTP response for one :80 request — serve the ACME HTTP-01 token if the path matches a
   provisioned one, else 301-redirect to HTTPS. Pure, so it's unit-testable without binding :80. *)
let http_front_response ~challenges ~host request_line =
  let prefix = "/.well-known/acme-challenge/" in
  match String.split_on_char ' ' request_line with
  | _ :: path :: _ when String.length path > String.length prefix && String.sub path 0 (String.length prefix) = prefix -> (
    match Hashtbl.find_opt challenges (String.sub path (String.length prefix) (String.length path - String.length prefix)) with
    | Some ka -> Printf.sprintf "HTTP/1.1 200 OK\r\nContent-Type: application/octet-stream\r\nContent-Length: %d\r\nConnection: close\r\n\r\n%s" (String.length ka) ka
    | None -> "HTTP/1.1 404 Not Found\r\nContent-Length: 0\r\nConnection: close\r\n\r\n")
  | _ :: path :: _ ->
    let host = match String.index_opt host ':' with Some i -> String.sub host 0 i | None -> host (* drop any :port; redirect to standard :443 *) in
    Printf.sprintf "HTTP/1.1 301 Moved Permanently\r\nLocation: https://%s%s\r\nContent-Length: 0\r\nConnection: close\r\n\r\n" host path
  | _ -> "HTTP/1.1 400 Bad Request\r\nContent-Length: 0\r\nConnection: close\r\n\r\n"

(* the dedicated :80 front (HTTP-01 challenge + HTTP→HTTPS redirect), owned by {!Fennec.serve} in
   TLS-mode prod. Independent of the app's :443, so it serves challenges during issuance and renewal
   and redirects plain-HTTP visitors. [challenges] is shared with the ACME issuer (empty for BYO-cert,
   making it redirect-only). *)
let serve_http_front ~sw ~net ~challenges =
  match (try Some (Eio.Net.listen ~sw ~reuse_addr:true ~backlog:64 net (`Tcp (Eio.Net.Ipaddr.V4.any, 80))) with _ -> None) with
  | None -> Printf.eprintf "fennec: could not bind :80 (need privilege / port free) — no HTTP->HTTPS redirect or ACME HTTP-01\n%!"
  | Some sock ->
    Eio.Fiber.fork ~sw (fun () ->
        let rec loop () =
          (try
             Eio.Net.accept_fork ~sw sock ~on_error:(fun _ -> ()) (fun flow _ ->
                 let r = Eio.Buf_read.of_flow flow ~max_size:8192 in
                 let line = try Eio.Buf_read.line r with _ -> "" in
                 let rec host_of () =
                   match Eio.Buf_read.line r with
                   | "" -> "localhost"
                   | h -> ( match String.index_opt h ':' with Some i when String.lowercase_ascii (String.trim (String.sub h 0 i)) = "host" -> String.trim (String.sub h (i + 1) (String.length h - i - 1)) | _ -> host_of ())
                   | exception _ -> "localhost"
                 in
                 try Eio.Flow.copy_string (http_front_response ~challenges ~host:(host_of ()) line) flow with _ -> ())
           with _ -> ());
          loop ()
        in
        loop ())

(* obtain (or, for a non-leaseholder replica, wait for) the cert for [label] covering [domains],
   then install it into the live set *)
let issue t ~clock ~net ~label ~domains =
  let directory = match t.cfg.directory with Some d -> d | None -> if t.cfg.staging then letsencrypt_staging else letsencrypt_prod in
  let key = account_key t.cfg.store in
  (* DNS-01 solver from the configured provider: set the TXT, return a thunk that removes it *)
  let solve_dns01 = match t.cfg.dns_provider with Some p -> Some (fun ~name ~value -> p.upsert_txt ~name ~value; fun () -> p.remove_txt ~name) | None -> None in
  let did =
    Cert_store.(
      t.cfg.store.with_lease ("issue:" ^ label) (fun () ->
          match
            Acme_client.obtain ~net ~clock ?solve_dns01 ~directory ~account_key:key ~email:(Option.value t.cfg.email ~default:"") ~domains
              ~provision:(fun ~token ~key_auth -> Hashtbl.replace t.challenges token key_auth)
              ~cleanup:(fun ~token -> Hashtbl.remove t.challenges token) ()
          with
          | Ok (cert_pem, key_pem) ->
            t.cfg.store.put (cert_key label) cert_pem;
            t.cfg.store.put (key_key label) key_pem;
            Printf.eprintf "fennec acme: issued certificate for %s\n%!" (String.concat ", " domains)
          | Error m -> Printf.eprintf "fennec acme: issuance failed for %s — %s\n%!" (String.concat ", " domains) m))
  in
  (* another replica holds the lease and is issuing — wait for the cert to land in the shared store *)
  if not did then (
    let rec wait n = if n <= 0 then () else match Cert_store.(t.cfg.store.get (cert_key label)) with Some _ -> () | None -> Eio.Time.sleep clock 2.0; wait (n - 1) in
    wait 60);
  Hashtbl.replace t.domains_of label domains;
  load_chain t ~label

(* on-demand: ensure a cert for an SNI [host] (load cached, else issue) when the allowlist approves —
   called during the handshake, so the first hit to a new tenant domain blocks briefly while issuing *)
let ensure t ~clock ~net ~allow host =
  if (not (Hashtbl.mem t.chains host)) && allow host then (
    load_chain t ~label:host;
    if not (Hashtbl.mem t.chains host) then issue t ~clock ~net ~label:host ~domains:[ host ])

(* the full lifecycle, called from {!Fennec.serve} before the server binds. Returns the live TLS
   source + an optional on-demand handler. [challenges] is shared with serve's :80 front. *)
let run ~sw ~clock ~net ~domains ~challenges (cfg : config) : running =
  let t = { cfg; challenges; chains = Hashtbl.create 8; domains_of = Hashtbl.create 8 } in
  (* the declared domains → one SAN cert under the "managed" label *)
  (match (cfg.email, domains) with
  | Some _, _ :: _ -> (
    match cert_days_left cfg.store ~label:"managed" ~clock with
    | Some d when d > 30 -> Hashtbl.replace t.domains_of "managed" domains; load_chain t ~label:"managed"
    | _ -> issue t ~clock ~net ~label:"managed" ~domains)
  | None, _ -> if cfg.on_demand = None then Printf.eprintf "fennec acme: no email (pass ~email or set FENNEC_ACME_EMAIL) — HTTPS disabled\n%!"
  | _, [] -> if cfg.on_demand = None then Printf.eprintf "fennec acme: no concrete (Exact) domain — HTTPS disabled (wildcards need DNS-01; catch-all needs on-demand TLS)\n%!");
  (* renewal: every ~12h re-issue any label under 30 days (declared SAN + each on-demand host) *)
  Eio.Fiber.fork ~sw (fun () ->
      let rec loop () =
        Eio.Time.sleep clock (12. *. 3600.);
        Hashtbl.iter (fun label ds -> match cert_days_left cfg.store ~label ~clock with Some d when d > 30 -> () | _ -> (try issue t ~clock ~net ~label ~domains:ds with _ -> ())) (Hashtbl.copy t.domains_of);
        loop ()
      in
      loop ());
  let on_demand = match (cfg.on_demand, cfg.email) with Some allow, Some _ -> Some (fun host -> try ensure t ~clock ~net ~allow host with _ -> ()) | _ -> None in
  { source = source t; on_demand }

(* ──── acme manager tests ──── *)

let%test ":80 front serves a provisioned HTTP-01 token; redirects everything else to https" =
  let has ~sub s =
    let n = String.length sub and m = String.length s in
    let rec go i = i + n <= m && (String.sub s i n = sub || go (i + 1)) in
    n = 0 || go 0
  in
  let c = Hashtbl.create 4 in
  Hashtbl.replace c "tokenAAA" "tokenAAA.keyauthZZZ";
  let ok = http_front_response ~challenges:c ~host:"ex.com" "GET /.well-known/acme-challenge/tokenAAA HTTP/1.1" in
  let miss = http_front_response ~challenges:c ~host:"ex.com" "GET /.well-known/acme-challenge/unknown HTTP/1.1" in
  let redir = http_front_response ~challenges:c ~host:"ex.com:80" "GET /app/page HTTP/1.1" in
  has ~sub:"200 OK" ok && has ~sub:"tokenAAA.keyauthZZZ" ok && has ~sub:"404" miss
  && has ~sub:"301 Moved Permanently" redir && has ~sub:"Location: https://ex.com/app/page" redir
