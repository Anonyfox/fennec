(** OpenID Connect state, nonce, and claim validation helpers.

    This module is transport-neutral. It builds OIDC Authorization Code + PKCE + nonce redirects,
    consumes single-use state, verifies RS256 ID tokens against trusted JWKS, validates ID-token
    claims, and derives canonical identity/email evidence. It does not fetch discovery documents,
    cache JWKS, exchange authorization codes, persist provider tokens, create users, or issue
    Accounts session tokens.

    {[
      let t = Accounts_oidc.make ~challenge in
      let google =
        Result.get_ok (Accounts_oidc.Providers.google ~client_id ~redirect_uri:"https://app/cb" ())
      in
      (* on callback: consume state, then verify the app-fetched ID token + JWKS *)
      match Accounts_oidc.consume_state t ~expected_connection:google.id state_token with
      | Error e -> Error e
      | Ok state ->
          let jwks = Result.get_ok (Accounts_oidc.jwks_of_string jwks_json) in
          Accounts_oidc.verify_id_token google state jwks id_token
    ]} *)

module Challenge = Accounts_challenge
module Identity = Accounts_identity
module OAuth = Accounts_oauth

(** OIDC connection configuration. *)
type connection = {
  id : string;
  issuer : string;
  authorize_url : string;
  client_id : string;
  redirect_uri : string;
  scopes : string list;
  extra_params : (string * string) list;
  org_id : string option;
  domains : string list;
  allow_jit : bool;
}

(** Built-in OIDC provider metadata for discovery, documentation, and setup UIs.

    [parameters] lists values the preset needs before it can become an exact runtime connection,
    such as an Auth0 domain, Okta domain, or Keycloak realm. Static providers have an empty list. *)
type preset = {
  id : string;
  display_name : string;
  issuer : string;
  authorize_url : string;
  default_scopes : string list;
  parameters : string list;
}

(** OIDC helper state. *)
type t

(** OIDC errors. *)
type error =
  | Invalid_connection of string
  | Invalid_callback of string
  | Invalid_state
  | Invalid_jwks of string
  | Invalid_id_token of string
  | Claim_mismatch of string
  | Challenge_error of Challenge.error
  | Identity_error of Identity.error

(** Human-readable error text. *)
val string_of_error : error -> string

(** Build an OIDC helper around the shared challenge service. *)
val make : challenge:Challenge.t -> t

(** Build and validate a connection.

    [id] is normalized to lowercase identity namespace form. [issuer], [authorize_url],
    [client_id], and [redirect_uri] must be non-blank. [openid] is always present in [scopes].
    [extra_params] may not override standard OAuth/OIDC parameters. *)
val connection :
  ?scopes:string list ->
  ?extra_params:(string * string) list ->
  ?org_id:string ->
  ?domains:string list ->
  ?allow_jit:bool ->
  id:string ->
  issuer:string ->
  authorize_url:string ->
  client_id:string ->
  redirect_uri:string ->
  unit ->
  (connection, error) result

(** Named OIDC provider presets.

    These remove rote issuer/authorization endpoint/scope setup while leaving token exchange, JWKS
    retrieval/cache, and profile/token persistence policy in the application. *)
module Providers : sig
  (** Known OIDC providers. Entries with [parameters] contain template strings for display; use the
      matching named function to build an exact connection. *)
  val all : preset list

  (** Find a known provider by normalized id. *)
  val find : string -> preset option

  (** Build a connection from static provider metadata. Returns an error for template presets that
      still contain unresolved parameters. *)
  val from_preset :
    ?id:string ->
    ?scopes:string list ->
    ?extra_params:(string * string) list ->
    ?org_id:string ->
    ?domains:string list ->
    ?allow_jit:bool ->
    client_id:string ->
    redirect_uri:string ->
    preset ->
    (connection, error) result

  (** Google Sign-In preset.

      Defaults to issuer [https://accounts.google.com], authorization endpoint
      [https://accounts.google.com/o/oauth2/v2/auth], and OIDC account scopes [openid email
      profile]. The default connection id is ["google"]. *)
  val google :
    ?id:string ->
    ?scopes:string list ->
    ?extra_params:(string * string) list ->
    ?org_id:string ->
    ?domains:string list ->
    ?allow_jit:bool ->
    client_id:string ->
    redirect_uri:string ->
    unit ->
    (connection, error) result

  (** Microsoft Entra ID preset.

      [tenant_id] must be the concrete tenant id used as the token issuer. The generic
      [common]/[organizations] authorize endpoints are convenient for discovery but are not exact
      enough for strict ID-token issuer validation. *)
  val microsoft_entra :
    ?id:string ->
    ?scopes:string list ->
    ?extra_params:(string * string) list ->
    ?org_id:string ->
    ?domains:string list ->
    ?allow_jit:bool ->
    tenant_id:string ->
    client_id:string ->
    redirect_uri:string ->
    unit ->
    (connection, error) result

  (** Sign in with Apple preset with [openid email name]. *)
  val apple :
    ?id:string ->
    ?scopes:string list ->
    ?extra_params:(string * string) list ->
    ?org_id:string ->
    ?domains:string list ->
    ?allow_jit:bool ->
    client_id:string ->
    redirect_uri:string ->
    unit ->
    (connection, error) result

  (** LinkedIn OIDC preset with [openid profile email]. *)
  val linkedin :
    ?id:string ->
    ?scopes:string list ->
    ?extra_params:(string * string) list ->
    ?org_id:string ->
    ?domains:string list ->
    ?allow_jit:bool ->
    client_id:string ->
    redirect_uri:string ->
    unit ->
    (connection, error) result

  (** Sign in with Slack OIDC preset with [openid email profile]. *)
  val slack :
    ?id:string ->
    ?scopes:string list ->
    ?extra_params:(string * string) list ->
    ?org_id:string ->
    ?domains:string list ->
    ?allow_jit:bool ->
    client_id:string ->
    redirect_uri:string ->
    unit ->
    (connection, error) result

  (** GitLab OIDC preset with [openid profile email]. *)
  val gitlab :
    ?id:string ->
    ?scopes:string list ->
    ?extra_params:(string * string) list ->
    ?org_id:string ->
    ?domains:string list ->
    ?allow_jit:bool ->
    client_id:string ->
    redirect_uri:string ->
    unit ->
    (connection, error) result

  (** Twitch OIDC preset. Twitch reports only [openid] in discovery; request additional API scopes
      explicitly when the app needs Twitch API access. *)
  val twitch :
    ?id:string ->
    ?scopes:string list ->
    ?extra_params:(string * string) list ->
    ?org_id:string ->
    ?domains:string list ->
    ?allow_jit:bool ->
    client_id:string ->
    redirect_uri:string ->
    unit ->
    (connection, error) result

  (** Salesforce OIDC preset for login.salesforce.com. *)
  val salesforce :
    ?id:string ->
    ?scopes:string list ->
    ?extra_params:(string * string) list ->
    ?org_id:string ->
    ?domains:string list ->
    ?allow_jit:bool ->
    client_id:string ->
    redirect_uri:string ->
    unit ->
    (connection, error) result

  (** Dropbox OIDC preset with [openid profile email]. *)
  val dropbox :
    ?id:string ->
    ?scopes:string list ->
    ?extra_params:(string * string) list ->
    ?org_id:string ->
    ?domains:string list ->
    ?allow_jit:bool ->
    client_id:string ->
    redirect_uri:string ->
    unit ->
    (connection, error) result

  (** Auth0 OIDC preset. [domain] may be [example.us.auth0.com] or a full [https://] URL. *)
  val auth0 :
    ?id:string ->
    ?scopes:string list ->
    ?extra_params:(string * string) list ->
    ?org_id:string ->
    ?domains:string list ->
    ?allow_jit:bool ->
    domain:string ->
    client_id:string ->
    redirect_uri:string ->
    unit ->
    (connection, error) result

  (** Okta OIDC preset. Defaults to the [default] custom authorization server. *)
  val okta :
    ?id:string ->
    ?authorization_server_id:string ->
    ?scopes:string list ->
    ?extra_params:(string * string) list ->
    ?org_id:string ->
    ?domains:string list ->
    ?allow_jit:bool ->
    domain:string ->
    client_id:string ->
    redirect_uri:string ->
    unit ->
    (connection, error) result

  (** Keycloak OIDC preset. [base_url] may include a path, for example
      [https://idp.example/auth]. *)
  val keycloak :
    ?id:string ->
    ?scopes:string list ->
    ?extra_params:(string * string) list ->
    ?org_id:string ->
    ?domains:string list ->
    ?allow_jit:bool ->
    base_url:string ->
    realm:string ->
    client_id:string ->
    redirect_uri:string ->
    unit ->
    (connection, error) result
end

(** Authorization URL issued for an OIDC provider redirect. *)
type authorization = {
  url : string;
  state : Challenge.token;
  pkce : OAuth.pkce;
  nonce : string;
  record : Challenge.record;
  connection : connection;
}

(** Issue an OIDC authorization URL plus single-use state.

    State metadata binds connection id, issuer, client id, redirect URI, PKCE verifier, nonce,
    optional user id, optional org id, and optional post-login redirect. *)
val authorize :
  t ->
  ?ttl:float ->
  ?user_id:string ->
  ?org_id:string ->
  ?redirect:string ->
  connection ->
  (authorization, error) result

(** Parsed callback query. *)
type callback = OAuth.callback =
  | Code of { code : string; state : Challenge.token }
  | Callback_error of { error : string; description : string option; state : Challenge.token option }

(** Parse a provider callback query string. *)
val parse_callback : string -> (callback, error) result

(** Consumed and verified OIDC state. *)
type state = {
  connection_id : string;
  issuer : string;
  client_id : string;
  code_verifier : string;
  nonce : string;
  redirect_uri : string;
  user_id : string option;
  org_id : string option;
  redirect : string option;
  record : Challenge.record;
}

(** Consume an OIDC state challenge.

    [expected_connection] rejects callbacks routed to the wrong connection before consuming state. *)
val consume_state : t -> ?expected_connection:string -> Challenge.token -> (state, error) result

(** Normalized claims extracted from a cryptographically verified ID token. *)
type claims = {
  issuer : string;
  subject : string;
  audience : string list;
  expires_at : float;
  not_before : float option;
  issued_at : float option;
  nonce : string option;
  email : string option;
  email_verified : bool option;
  hosted_domain : string option;
  tenant : string option;
  groups : string list;
}

(** Valid OIDC principal derived from consumed state and verified claims. *)
type principal = {
  identity : Identity.key;
  email_identity : Identity.key option;
  email : string option;
  email_verified : bool;
  org_id : string option;
  groups : string list;
  claims : claims;
}

(** Trusted OIDC signing key from a JWKS document. Only RSA keys for RS256 are accepted by the
    native verifier. *)
type jwk = {
  kid : string option;
  alg : string option;
  key : X509.Public_key.t;
}

(** Parse a JWKS document and keep usable RSA signing keys. *)
val jwks_of_string : string -> (jwk list, error) result

(** Validate verified ID-token claims against the connection and consumed state.

    This checks exact issuer, audience/client id, expiry, not-before, issued-at skew, nonce,
    configured domain policy, and derives canonical OIDC/email identities. [now] defaults to
    [Unix.gettimeofday] and [leeway] defaults to 60 seconds. *)
val validate_claims :
  ?now:(unit -> float) -> ?leeway:float -> connection -> state -> claims -> (principal, error) result

(** Verify an RS256 ID token against a trusted JWKS, parse claims, and then call
    {!validate_claims}. The token header [kid], when present, selects the matching key; without
    [kid], the verifier tries every compatible key. *)
val verify_id_token :
  ?now:(unit -> float) ->
  ?leeway:float ->
  connection ->
  state ->
  jwk list ->
  string ->
  (principal, error) result
