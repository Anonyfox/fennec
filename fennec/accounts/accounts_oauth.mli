(** OAuth 2.0 Authorization Code + PKCE helpers.

    This module is provider- and transport-neutral. It builds authorization URLs, stores transient
    state/PKCE material through {!Accounts_challenge}, parses callback query strings, and builds
    provider-subject identity keys. It does not perform HTTP token exchange, fetch provider profiles,
    persist provider tokens, create users, or issue Accounts session tokens.

    {[
      let t = Accounts_oauth.make ~challenge in
      let github =
        Result.get_ok
          (Accounts_oauth.Providers.github ~client_id ~redirect_uri:"https://app/cb" ())
      in
      (* redirect to [auth.url], then on callback consume the signed state *)
      match Accounts_oauth.authorize t github with
      | Ok auth -> Conn.redirect auth.url conn
      | Error e -> Conn.text ~status:400 conn (Accounts_oauth.string_of_error e)
    ]} *)

module Challenge = Accounts_challenge
module Identity = Accounts_identity

(** OAuth provider/client configuration. *)
type provider = {
  name : string;
  authorize_url : string;
  client_id : string;
  redirect_uri : string;
  scopes : string list;
  extra_params : (string * string) list;
}

(** Built-in provider metadata for discovery, documentation, and setup UIs. *)
type preset = {
  id : string;
  display_name : string;
  authorize_url : string;
  default_scopes : string list;
}

(** OAuth helper state. *)
type t

(** OAuth errors. *)
type error =
  | Invalid_provider of string
  | Invalid_callback of string
  | Invalid_state
  | Challenge_error of Challenge.error
  | Identity_error of Identity.error

(** Human-readable error text. *)
val string_of_error : error -> string

(** Build an OAuth helper around the shared challenge service. *)
val make : challenge:Challenge.t -> t

(** Build and validate a provider configuration.

    [name] is normalized to lowercase identity namespace form. [authorize_url], [client_id], and
    [redirect_uri] must be non-blank. [scopes] are trimmed and blank scopes are dropped.
    [extra_params] are appended to the authorization URL after the standard parameters. Standard
    OAuth/PKCE parameters such as [state], [redirect_uri], [scope], and [code_challenge] are
    reserved and rejected in [extra_params]. *)
val provider :
  ?scopes:string list ->
  ?extra_params:(string * string) list ->
  name:string ->
  authorize_url:string ->
  client_id:string ->
  redirect_uri:string ->
  unit ->
  (provider, error) result

(** PKCE pair. [challenge] is the base64url SHA-256 challenge for [verifier]. *)
type pkce = {
  verifier : string;
  challenge : string;
}

(** Generate a high-entropy S256 PKCE verifier/challenge pair. *)
val pkce : unit -> pkce

(** Authorization URL issued for a provider redirect. *)
type authorization = {
  url : string;
  state : Challenge.token;
  pkce : pkce;
  record : Challenge.record;
  provider : provider;
}

(** Issue an authorization URL plus a single-use OAuth state challenge.

    The state challenge metadata stores provider name, redirect URI, PKCE verifier, optional user id,
    optional org id, and optional post-login redirect. *)
val authorize :
  t ->
  ?ttl:float ->
  ?user_id:string ->
  ?org_id:string ->
  ?redirect:string ->
  provider ->
  (authorization, error) result

(** Parsed callback query. *)
type callback =
  | Code of { code : string; state : Challenge.token }
  | Callback_error of { error : string; description : string option; state : Challenge.token option }

(** Parse a provider callback query string. *)
val parse_callback : string -> (callback, error) result

(** Consumed and verified OAuth state. *)
type state = {
  provider : string;
  code_verifier : string;
  redirect_uri : string;
  user_id : string option;
  org_id : string option;
  redirect : string option;
  record : Challenge.record;
}

(** Consume an OAuth callback state challenge.

    [expected_provider] rejects callbacks routed to the wrong provider without relying on request
    parameters. A mismatch is reported as [Invalid_state]. *)
val consume_state : t -> ?expected_provider:string -> Challenge.token -> (state, error) result

(** Build the stable global identity key for a provider subject. *)
val identity : provider -> subject:string -> (Identity.key, error) result

(** Named OAuth provider presets.

    These only preconfigure provider identity, authorization endpoint, and conservative account
    scopes. Token exchange and profile fetching stay app-owned because each deployment decides which
    provider profile fields and access tokens, if any, are persisted. *)
module Providers : sig
  (** Known OAuth providers with stable public authorization endpoints. *)
  val all : preset list

  (** Find a known provider by normalized id. *)
  val find : string -> preset option

  (** Build a provider from known metadata. *)
  val from_preset :
    ?scopes:string list ->
    ?extra_params:(string * string) list ->
    client_id:string ->
    redirect_uri:string ->
    preset ->
    (provider, error) result

  (** GitHub OAuth app preset.

      Defaults to GitHub's web authorization endpoint and the account scopes commonly needed to
      fetch the signed-in user's public profile plus verified email addresses. Override [scopes] when
      the app intentionally needs less or more. *)
  val github :
    ?scopes:string list ->
    ?extra_params:(string * string) list ->
    client_id:string ->
    redirect_uri:string ->
    unit ->
    (provider, error) result

  (** Facebook Login preset. Defaults to the current Graph login dialog endpoint and
      [email public_profile]. Override [version] when the app is pinned to a different Graph API
      version. *)
  val facebook :
    ?version:string ->
    ?scopes:string list ->
    ?extra_params:(string * string) list ->
    client_id:string ->
    redirect_uri:string ->
    unit ->
    (provider, error) result

  (** Discord OAuth preset with [identify email]. *)
  val discord :
    ?scopes:string list ->
    ?extra_params:(string * string) list ->
    client_id:string ->
    redirect_uri:string ->
    unit ->
    (provider, error) result

  (** X OAuth 2.0 preset with the minimal user-read scopes commonly required by the API. *)
  val x :
    ?scopes:string list ->
    ?extra_params:(string * string) list ->
    client_id:string ->
    redirect_uri:string ->
    unit ->
    (provider, error) result

  (** Spotify OAuth preset with private profile and email scopes. *)
  val spotify :
    ?scopes:string list ->
    ?extra_params:(string * string) list ->
    client_id:string ->
    redirect_uri:string ->
    unit ->
    (provider, error) result

  (** Reddit OAuth preset with [identity]. A [duration=temporary] extra parameter is included by
      default because login normally does not need a provider refresh token. *)
  val reddit :
    ?scopes:string list ->
    ?extra_params:(string * string) list ->
    client_id:string ->
    redirect_uri:string ->
    unit ->
    (provider, error) result

  (** Login with Amazon preset with [profile]. *)
  val amazon :
    ?scopes:string list ->
    ?extra_params:(string * string) list ->
    client_id:string ->
    redirect_uri:string ->
    unit ->
    (provider, error) result

  (** Bitbucket Cloud OAuth preset with account and email scopes. *)
  val bitbucket :
    ?scopes:string list ->
    ?extra_params:(string * string) list ->
    client_id:string ->
    redirect_uri:string ->
    unit ->
    (provider, error) result
end
