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
let native_paw = Accounts_native.native_paw
let boot = Accounts_native.boot
