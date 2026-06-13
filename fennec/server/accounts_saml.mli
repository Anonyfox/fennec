(** SAML 2.0 enterprise SSO state, XML, and assertion validation helpers.

    This module implements Fennec's constrained native SAML profile for statically linked OCaml
    deployments: SP-initiated request state, AuthnRequest rendering, RelayState replay protection,
    HTTP-POST response parsing, enveloped XML signature validation, assertion validation, and
    canonical SAML/email identity evidence. It deliberately does not implement IdP metadata import,
    certificate-chain policy, user creation, identity merging, or Accounts session issuance.

    {[
      let t = Accounts_saml.make ~challenge in
      let conn =
        Result.get_ok
          (Accounts_saml.connection ~id:"acme" ~issuer:"https://sp.example"
             ~sso_url:"https://idp.example/sso" ~entity_id:"https://sp.example"
             ~acs_url:"https://app/acs" ())
      in
      let trusted = Result.get_ok (Accounts_saml.trusted_keys_of_pem idp_pem) in
      (* redirect via issue_request; on ACS callback consume the response atomically *)
      Accounts_saml.consume_response t conn ~trusted_keys:trusted
        ~relay_state ~saml_response
    ]} *)

module Challenge = Accounts_challenge
module Identity = Accounts_identity

(** SAML service-provider connection configuration. *)
type connection = {
  id : string;
  issuer : string;
  sso_url : string;
  entity_id : string;
  acs_url : string;
  org_id : string option;
  domains : string list;
  external_id_attribute : string option;
  email_attribute : string option;
  trust_email : bool;
  allow_jit : bool;
}

(** SAML helper state. *)
type t

(** SAML errors. *)
type error =
  | Invalid_connection of string
  | Invalid_state
  | Invalid_key_material of string
  | Response_too_large of int
  | Signing_error of string
  | Assertion_mismatch of string
  | Challenge_error of Challenge.error
  | Identity_error of Identity.error

(** Human-readable error text. *)
val string_of_error : error -> string

(** Build a SAML helper around the shared challenge service. *)
val make : challenge:Challenge.t -> t

(** Build and validate a connection.

    [id] is normalized to lowercase identity namespace form. [issuer], [sso_url], [entity_id], and
    [acs_url] must be non-blank. [domains] are lowercased routing/admission hints, not identity
    proof. *)
val connection :
  ?org_id:string ->
  ?domains:string list ->
  ?external_id_attribute:string ->
  ?email_attribute:string ->
  ?trust_email:bool ->
  ?allow_jit:bool ->
  id:string ->
  issuer:string ->
  sso_url:string ->
  entity_id:string ->
  acs_url:string ->
  unit ->
  (connection, error) result

(** Extract trusted IdP public keys from PEM material.

    Certificate PEM bundles are preferred and all certificates are returned as public keys. A single
    PEM public key is also accepted for local fixtures and hand-pinned deployments. *)
val trusted_keys_of_pem : string -> (X509.Public_key.t list, error) result

(** SP-initiated SAML request state.

    [request_id] is the public AuthnRequest ID to put into the XML request. [relay_state] is the
    opaque challenge token to round-trip through RelayState and consume on ACS callback. *)
type request = {
  request_id : string;
  relay_state : Challenge.token;
  record : Challenge.record;
  connection : connection;
}

(** Issue a single-use SP-initiated SAML request state. *)
val issue_request :
  t ->
  ?ttl:float ->
  ?user_id:string ->
  ?org_id:string ->
  ?redirect:string ->
  connection ->
  (request, error) result

(** Render a minimal SP-initiated AuthnRequest XML document for [request]. *)
val authn_request_xml : request -> string

(** Build the HTTP-Redirect URL for [request].

    The AuthnRequest is raw-deflated, base64-encoded, and percent-encoded as [SAMLRequest].
    RelayState carries the challenge token. *)
val redirect_url : request -> string

(** Build a signed HTTP-Redirect URL for IdPs that require signed AuthnRequests.

    The query string is signed according to the SAML HTTP-Redirect binding with RSA-SHA256 over
    [SAMLRequest], [RelayState], and [SigAlg]. *)
val signed_redirect_url : request -> signing_key:X509.Private_key.t -> (string, error) result

(** Verified request state metadata.

    {!consume_state} returns this after consuming RelayState. {!consume_response} uses the same
    shape internally before final consume so invalid SAMLResponse payloads do not burn state. *)
type state = {
  request_id : string;
  connection_id : string;
  issuer : string;
  entity_id : string;
  acs_url : string;
  user_id : string option;
  org_id : string option;
  redirect : string option;
  record : Challenge.record;
}

(** Consume RelayState for an ACS callback.

    [expected_connection] rejects callbacks routed to the wrong connection before consuming state. *)
val consume_state : t -> ?expected_connection:string -> Challenge.token -> (state, error) result

(** Assertion facts extracted after XML signature and wrapping validation. *)
type assertion = {
  issuer : string;
  audience : string;
  recipient : string;
  destination : string option;
  in_response_to : string option;
  not_before : float option;
  not_on_or_after : float option;
  name_id : string;
  name_id_format : string option;
  external_id : string option;
  email : string option;
  attributes : (string * string list) list;
  session_index : string option;
}

(** Valid SAML principal derived from consumed state and verified assertion facts. *)
type principal = {
  identity : Identity.key;
  email_identity : Identity.key option;
  email : string option;
  (** Connection provisioning policy copied onto the verified principal so account-linking code does
      not need to keep separate connection state around. *)
  allow_jit : bool;
  org_id : string option;
  session_index : string option;
  (** SHA-256 public-key fingerprint of the trusted key that verified the response, when XML
      signature verification happened in this module. *)
  signature_key_fingerprint : string option;
  attributes : (string * string list) list;
  assertion : assertion;
}

(** Validate signature-verified assertion facts against the connection and consumed state.

    This checks issuer, audience, recipient, destination, InResponseTo, time conditions, configured
    domain policy, and derives SAML/email identities. [now] defaults to [Unix.gettimeofday] and
    [leeway] defaults to 60 seconds. *)
val validate_assertion :
  ?now:(unit -> float) -> ?leeway:float -> connection -> state -> assertion -> (principal, error) result

(** Verify a base64-encoded HTTP-POST SAMLResponse end to end.

    This accepts only Fennec's constrained SAML profile: exactly one enveloped XML Signature over
    either the Response or Assertion, RSA-SHA256, SHA-256 digest, exclusive C14N without comments,
    exact same-document ID reference, strict ID uniqueness, one bearer assertion, and no encrypted
    assertions. XML parsing, canonicalization, signature verification, assertion extraction, and
    {!validate_assertion} are all performed before returning a principal. [max_response_bytes]
    defaults to a conservative built-in limit and is checked before XML parsing. *)
val verify_response :
  ?now:(unit -> float) ->
  ?leeway:float ->
  ?max_response_bytes:int ->
  connection ->
  state ->
  trusted_keys:X509.Public_key.t list ->
  saml_response:string ->
  (principal, error) result

(** Consume an ACS callback atomically at the Accounts boundary.

    [consume_response] first loads RelayState metadata without consuming it, verifies the
    base64-encoded HTTP-POST SAMLResponse against [trusted_keys], validates assertion facts against
    the stored request state, then consumes RelayState. Invalid signatures, malformed XML, assertion
    mismatches, and wrong-connection callbacks leave RelayState reusable for a corrected retry; a
    valid callback succeeds only if final consume wins. [max_response_bytes] is passed through to
    {!verify_response}. *)
val consume_response :
  t ->
  ?now:(unit -> float) ->
  ?leeway:float ->
  ?max_response_bytes:int ->
  ?expected_connection:string ->
  connection ->
  trusted_keys:X509.Public_key.t list ->
  relay_state:Challenge.token ->
  saml_response:string ->
  (principal, error) result
