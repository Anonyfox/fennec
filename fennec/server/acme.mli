(** Automatic HTTPS via ACME (Let's Encrypt): HTTP-01 for the host router's concrete domains, a
    {!Cert_store}-backed account key + certificate, and zero-downtime renewal / hot-reload. Pass
    {!auto} to {!Fennec.serve} as [~acme]. Wildcards (which need DNS-01) and a dynamic catch-all
    (which needs on-demand TLS) are out of scope and reported, not failed.

    {[
      (* production HTTPS for the host router's concrete domains *)
      Fennec.serve ~acme:(Acme.auto ~email:"ops@app.com" ()) [ (* … endpoints … *) ]

      (* staging (no rate limits, untrusted certs) for a smoke test *)
      Fennec.serve ~acme:(Acme.auto ~email:"ops@app.com" ~staging:true ()) endpoints
    ]} *)

(** ACME configuration. *)
type config

(** A DNS provider for DNS-01 (the only way to get a wildcard cert). Implement it over your provider
    (Cloudflare / Route 53 / …): [upsert_txt] sets the TXT record [name] (e.g.
    ["_acme-challenge.app.com"]) to [value], [remove_txt] deletes it. No provider SDKs are baked into
    fennec — the same seam idea as {!Cert_store}. *)
type dns_provider = { upsert_txt : name:string -> value:string -> unit; remove_txt : name:string -> unit }

(** The Let's Encrypt production directory URL. *)
val letsencrypt_prod : string

(** The Let's Encrypt staging directory URL — no rate limits, untrusted certs; for testing. *)
val letsencrypt_staging : string

(** [auto ?email ?store ?staging ?domains ?directory ()] configures automatic certificates. Never
    raises: a missing email just leaves HTTPS off (logged at run), so a dev build boots on plain
    HTTP. Env overrides code — [FENNEC_ACME_EMAIL] (else [~email]), [FENNEC_ACME_STAGING]. [store]
    defaults to a file store under [$FENNEC_ACME_DIR] (else [$XDG_STATE_HOME/fennec/acme]) — override
    for an ephemeral / multi-replica deployment. [domains] overrides the host-router-derived set.
    [dns_provider] enables DNS-01, so wildcard domains (a [Suffix] host like ["*.app.com"]) are
    certified too. [on_demand] enables on-demand issuance: when an HTTPS connection arrives for an
    SNI host the callback approves (e.g. a known customer domain), its certificate is obtained on the
    first connection and cached — for runtime-added per-tenant domains. *)
val auto :
  ?email:string ->
  ?store:Cert_store.t ->
  ?staging:bool ->
  ?domains:string list ->
  ?directory:string ->
  ?dns_provider:dns_provider ->
  ?on_demand:(string -> bool) ->
  unit ->
  config

(** [domains_override cfg] — the explicit domain list, if one was given (else the caller derives the
    certifiable domains from the host router). *)
val domains_override : config -> string list option

(** [dns_enabled cfg] — whether a DNS provider is configured (so wildcards can be certified via DNS-01). *)
val dns_enabled : config -> bool

(** [serve_http_front ~sw ~net ~challenges] binds :80 and serves ACME HTTP-01 tokens from
    [challenges] (shared with the issuer; empty ⇒ redirect-only for a BYO cert), 301-redirecting
    every other request to HTTPS. Owned by {!Fennec.serve} in TLS-mode production. *)
val serve_http_front : sw:Eio.Switch.t -> net:_ Eio.Net.t -> challenges:(string, string) Hashtbl.t -> unit

(** What {!run} returns: [source] is the live TLS config the server reads per connection (SNI-
    selecting among all current certs; [None] until the first lands), and [on_demand], when present,
    ensures a certificate for an SNI host (issuing on first connection). *)
type running = { source : unit -> Tls.Config.server option; on_demand : (string -> unit) option }

(** [run ~sw ~clock ~net ~domains ~challenges cfg] runs the ACME lifecycle on the server's switch: an
    initial issue-or-load for [domains] (one SAN cert) and the renewal loop, provisioning HTTP-01
    tokens into the shared [challenges] table (DNS-01 via the configured provider for wildcards), and
    returns the live TLS {!running.source}. No-ops with a clear log if there's no email / domain (and
    no on-demand). Called from {!Fennec.serve} before the server binds. *)
val run : sw:Eio.Switch.t -> clock:_ Eio.Time.clock -> net:_ Eio.Net.t -> domains:string list -> challenges:(string, string) Hashtbl.t -> config -> running
