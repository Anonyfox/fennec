(** Automatic HTTPS via ACME (Let's Encrypt): HTTP-01 for the host router's concrete domains, a
    {!Cert_store}-backed account key + certificate, and zero-downtime renewal / hot-reload. Pass
    {!auto} to {!Fennec.serve} as [~acme]. Wildcards (which need DNS-01) and a dynamic catch-all
    (which needs on-demand TLS) are out of scope and reported, not failed. *)

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
    certified too. *)
val auto : ?email:string -> ?store:Cert_store.t -> ?staging:bool -> ?domains:string list -> ?directory:string -> ?dns_provider:dns_provider -> unit -> config

(** [domains_override cfg] — the explicit domain list, if one was given (else the caller derives the
    certifiable domains from the host router). *)
val domains_override : config -> string list option

(** [dns_enabled cfg] — whether a DNS provider is configured (so wildcards can be certified via DNS-01). *)
val dns_enabled : config -> bool

(** [serve_http_front ~sw ~net ~challenges] binds :80 and serves ACME HTTP-01 tokens from
    [challenges] (shared with the issuer; empty ⇒ redirect-only for a BYO cert), 301-redirecting
    every other request to HTTPS. Owned by {!Fennec.serve} in TLS-mode production. *)
val serve_http_front : sw:Eio.Switch.t -> net:_ Eio.Net.t -> challenges:(string, string) Hashtbl.t -> unit

(** [run ~sw ~clock ~net ~domains ~challenges cfg cert_ref] runs the ACME lifecycle on the server's
    switch: an initial issue-or-load and the renewal loop, provisioning HTTP-01 tokens into the
    shared [challenges] table and installing the live certificate into [cert_ref] (which the server's
    TLS source reads per connection). No-ops with a clear log if there's no email or no concrete
    domain. Called from {!Fennec.serve} before the server binds. *)
val run : sw:Eio.Switch.t -> clock:_ Eio.Time.clock -> net:_ Eio.Net.t -> domains:string list -> challenges:(string, string) Hashtbl.t -> config -> Tls.Config.server option ref -> unit
