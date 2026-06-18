(* accounts.ml — the public Accounts facade. It assembles the carved engine + its satellites into
   the one module that satisfies accounts.mli; it holds no logic of its own, only re-exports:

     include Accounts_base       the engine (types/secrets/identity-bridge/runtime/request/
                                 lifecycle/login/http), flattened into this namespace
     module Codec                the record BSON codecs
     module Collection_store     the backend-blind collection store builder
     include Accounts_session    the HTTP session-view serializers (session_doc / session_paw / …)
     module Methods              the DDP method-handler functor (server does Accounts.Methods(R))
     module Store + boot/…       the storage backends + the process-native singleton

   The inline tests ride with the code they exercise, in the carved modules: store/backend in
   {!Accounts_native}, the DDP handlers in {!Accounts_methods}, and the engine-integration +
   HTTP/DDP-system suite in {!Accounts_integration_test} (a dedicated test module: its shared
   in-memory fixture lives at the {!Accounts_native} layer, above the whole engine, so it cannot be
   pushed down into the engine leaves). *)

include Accounts_base
module Codec = Accounts_codec
module Collection_store = Accounts_collection_store

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
   public surface ([Store], [memory_store], [current], [native_paw], [boot]) unchanged. *)
module Store = Accounts_native.Store

let memory_store = Accounts_native.memory_store
let current = Accounts_native.current
let start = Accounts_native.start

(* the config -> auto-wire seam: native_paw/boot already consult [Wiring.routes (current ())]; the
   pulse server consults [Wiring.method_enabled] for the DDP method gate. *)
module Wiring = Accounts_native.Wiring

let native_paw = Accounts_native.native_paw
let boot = Accounts_native.boot
