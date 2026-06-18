(* accounts.ml — the public Accounts facade. It assembles the carved engine + its satellites into
   the one module that satisfies accounts.mli; it holds no logic of its own, only re-exports:

     include Accounts_base       the engine (types/secrets/identity-bridge/runtime/request/
                                 lifecycle/login/http), flattened into this namespace
     include Accounts_session    the HTTP session-view serializers (session_doc / session_paw / …)
     module Methods              the DDP method-handler functor (server does Accounts.Methods(R))
     module Store + boot/…       the storage backends + the process-native singleton

   The inline tests ride with the code they exercise, in the carved modules: store/backend in
   {!Accounts_native}, the DDP handlers in {!Accounts_methods}, and the engine-integration +
   HTTP/DDP-system suite in {!Accounts_integration_test} (a dedicated test module: its shared
   in-memory fixture lives at the {!Accounts_native} layer, above the whole engine, so it cannot be
   pushed down into the engine leaves). *)

include Accounts_base

(* [Codec] / [Collection_store] (the low-level record-codec + backend-blind collection-store builders)
   are the storage plumbing under [Store]; the mli scopes them to {!Advanced} only, so they are aliased
   there (below), not at this top level. *)

(* the typed-BSON shape language, re-exported so apps can build a {!profile_codec} for the optional
   typed [user.profile] without depending on fennec.pulse.sift by its own name. *)
module Sift = Sift

(* the config-level provider PRESETS (Stage 3b): one-liner constructors that return the closed
   [external_identity provider] variant with the token-exchange recipe bundled. They live in
   {!Accounts_provider_presets} (above the engine, below this facade) because the variant + the
   [external_identity] bridge are engine types. Re-export each onto its capability submodule so the
   public paths are [Accounts.OAuth.github] / [Accounts.Oidc.google] / [Accounts.Saml.okta], next to
   the existing protocol surface ([OAuth.provider], [OAuth.Providers.*], …) — additive, nothing
   removed. The presets default their exchange HTTP to the ambient {!Accounts_http_client} transport,
   so the call site is a true one-liner; the explicit app-owned [~exchange] stays via [OAuth.custom]
   / [Oidc.connection] / [Saml.connection]. *)
module OAuth = struct
  include Accounts_oauth

  let github = Accounts_provider_presets.OAuth.github
  let google = Accounts_provider_presets.OAuth.google
  let custom = Accounts_provider_presets.OAuth.custom
end

module Oidc = struct
  include Accounts_oidc

  let google = Accounts_provider_presets.Oidc.google
  let microsoft = Accounts_provider_presets.Oidc.microsoft
  let okta = Accounts_provider_presets.Oidc.okta
  let auth0 = Accounts_provider_presets.Oidc.auth0
  let keycloak = Accounts_provider_presets.Oidc.keycloak

  (* the escape hatch ([from_connection], not [connection] — that name is the connection builder above). *)
  let from_connection = Accounts_provider_presets.Oidc.from_connection
end

module Saml = struct
  include Accounts_saml

  let okta = Accounts_provider_presets.Saml.okta
  let from_connection = Accounts_provider_presets.Saml.from_connection
end

(* the ambient outbound-HTTPS transport the presets drive (Paw.Https_client by default). {!Fennec.serve}
   installs it at boot with the server's Eio net (mirroring Fennec_mail); exposed so the runtime — and
   tests — can set it. *)
module Http_transport = Accounts_http_client

let set_http_transport = Accounts_http_client.set_transport

(* the HTTP session-view serializers live in {!Accounts_session}; re-export the two public entry
   points at the [Accounts] top level so accounts.mli is satisfied unchanged. *)
let session_doc = Accounts_session.session_doc
let session_paw = Accounts_session.session_paw

(* the DDP method handlers + their support helpers live in {!Accounts_methods}; re-export the
   [Methods] functor at the same path the server expects ([Accounts.Methods(R)]). *)
module Methods = Accounts_methods.Methods

(* the storage backends + the process-native singleton live in {!Accounts_native}; re-export the
   public surface ([Store], [current], [native_paw], [boot]) unchanged. ([memory_store] is re-exported
   under {!Advanced} — it is an examples/tests helper, not a daily driver.) *)
module Store = Accounts_native.Store

let current = Accounts_native.current
let start = Accounts_native.start

(* the config -> auto-wire seam: native_paw/boot already consult [Wiring.routes (current ())]; the
   pulse server consults [Wiring.method_enabled] for the DDP method gate. *)
module Wiring = Accounts_native.Wiring

let native_paw = Accounts_native.native_paw
let boot = Accounts_native.boot

(* ---- the RBAC route guards: ONE policy, ONE instance (Stage 3c) ---------------------------------

   The daily-driver guards [require_role] / [require_permission] / [require_org] now read the policy
   AND the instance from the process-native singleton ([current ()]) — the SAME configured instance
   the app's routes, methods, and [native_paw] already run against. That kills the old footgun where a
   guard took an explicit [t] (and once a [~policy]): a route could be guarded against a DIFFERENT
   Accounts instance / a different policy than the one actually authenticating the request — two RBAC
   models in one app. There is now exactly one: the policy on [current ()], code-declared once via
   [start ~config:{ defaults with rbac = Some policy }] (or [Fennec.serve ?accounts]).

   The explicit-[t] forms are NOT gone — they move to {!Advanced} as [@@deprecated] shims for one
   release (advanced multi-instance setups + the migration window). [require_user] and
   [require_assurance] never took a [t]/policy, so they are unchanged at the top level. *)

let require_role ?redirect role () : Paw.t = Accounts_base.require_role (current ()) ?redirect role ()
let require_permission ?redirect permission () : Paw.t = Accounts_base.require_permission (current ()) ?redirect permission ()
let require_org ?redirect ?permission () : Paw.t = Accounts_base.require_org (current ()) ?redirect ?permission ()

(* ---- the advanced / internal surface, behind ONE namespace (Stage 3c facade re-layer) ------------

   Newcomers meet the daily-drivers at the top level; the escape hatches, custom-layout building blocks,
   and low-level engine entry points are grouped here. The ~25 [*_paw] route constructors — the biggest
   newcomer-noise source — are EXCLUSIVE to this namespace (gone from the top level: the umbrella
   [config] auto-mounts the default routes, so the 95% app never names them); so are [memory_store],
   [Codec]/[Collection_store], and the deprecated explicit-[t] guards. The remaining advanced families
   (the MFA [*_completion] branches, the protocol identity resolvers + [external_identity] builders,
   identity-link admin, the passkey ceremony, the lower-level challenge issue/consume) stay reachable at
   the [Accounts] top level ONLY — they were once ALSO mirrored here for discoverability, but that
   double-listing bloated accounts.mli by ~270 lines and was pruned. Nothing was removed from the
   library; every symbol stays where it always was. The implementations stay flattened at the [Accounts]
   top level (via [include Accounts_base]); accounts.mli is what scopes a name to [Advanced]. *)
module Advanced = struct
  (* explicit-instance RBAC guards — DEPRECATED: prefer the top-level no-[t] forms (they bind the
     configured [current ()] for you, so a route can't be guarded against a different RBAC model). *)
  let require_role = Accounts_base.require_role
  let require_permission = Accounts_base.require_permission
  let require_org = Accounts_base.require_org

  (* the low-level record-codec + backend-blind collection-store builders (the storage plumbing under
     [Store]); apps build a [Store.t], not these directly. *)
  module Codec = Accounts_codec
  module Collection_store = Accounts_collection_store

  (* the in-process memory store — examples/tests; the framework path uses [current ()]'s store. *)
  let memory_store = Accounts_native.memory_store

  (* NOTE: the external-identity FACT builders, identity-link administration, the lower-level
     email-challenge issue/consume pair, the MFA-aware login [*_completion] branches, and the protocol
     identity resolvers / passkey ceremony primitives are NOT re-bound here. They live ONLY at the
     [Accounts] top level (flattened via [include Accounts_base]); accounts.mli used to double-list them
     under [Advanced] too, but that ~270-line mirror was confusing and has been removed — the symbols
     are unaffected and stay reachable top-level. Only the genuinely [Advanced]-exclusive surface
     remains here: the deprecated explicit-[t] guards, [Codec]/[Collection_store], [memory_store], and
     the [*_paw] route constructors below. *)

  (* the HTTP route constructors for CUSTOM layouts. The config auto-wires the default URLs under
     [routes.auth_prefix]; mount these by hand only for bespoke paths. ([native_paw] / [session_paw]
     stay at the top level — they are the framework wiring + the common "/me" helper.) *)
  let password_reset_request_paw = password_reset_request_paw
  let password_reset_paw = password_reset_paw
  let enrollment_paw = enrollment_paw
  let email_verification_request_paw = email_verification_request_paw
  let email_verification_paw = email_verification_paw
  let email_login_link_request_paw = email_login_link_request_paw
  let email_login_link_paw = email_login_link_paw
  let email_otp_request_paw = email_otp_request_paw
  let email_otp_paw = email_otp_paw
  let mfa_totp_paw = mfa_totp_paw
  let mfa_backup_code_paw = mfa_backup_code_paw
  let passkey_registration_options_paw = passkey_registration_options_paw
  let passkey_registration_finish_paw = passkey_registration_finish_paw
  let passkey_assertion_options_paw = passkey_assertion_options_paw
  let passkey_assertion_finish_paw = passkey_assertion_finish_paw
  let mfa_passkey_assertion_options_paw = mfa_passkey_assertion_options_paw
  let mfa_passkey_assertion_finish_paw = mfa_passkey_assertion_finish_paw
  let scim_paw = scim_paw
  let oauth_authorize_paw = oauth_authorize_paw
  let oauth_callback_paw = oauth_callback_paw
  let oauth_callback_popup_paw = oauth_callback_popup_paw
  let oidc_authorize_paw = oidc_authorize_paw
  let oidc_callback_paw = oidc_callback_paw
  let saml_authorize_paw = saml_authorize_paw
  let saml_callback_paw = saml_callback_paw
end

(* the OPTIONAL Sift-typed view of [user.profile] (Stage 3c). [user.profile] stays [Bson.t option];
   these are a thin convenience for apps that describe the profile shape as a ['p Sift.t]. Live in
   {!Accounts_profile}; re-export onto the facade. *)
type 'p profile_codec = 'p Accounts_profile.profile_codec

let profile_codec = Accounts_profile.profile_codec
let typed_profile = Accounts_profile.typed_profile
let typed_profile_result = Accounts_profile.typed_profile_result
let set_typed_profile = Accounts_profile.set_typed_profile
let clear_profile = Accounts_profile.clear_profile
