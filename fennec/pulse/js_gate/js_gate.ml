(* JS-BOUNDARY GATE. The reactive layer (fennec.pulse) MUST cross-compile to JavaScript over the
   in-memory backend (minimongo) — that is the whole point of the client cache. This executable links the
   reactive stack over Fennec_mongo_backend.Mini and is built to JavaScript (.bc.js). If a native dependency ever
   leaks into fennec.pulse or the pure seam (libmongoc, LMDB, a TLS/C stub from fennec-mongo.dynamic),
   js_of_ocaml can't find the C primitive and THIS target stops building — so the regression is caught at
   build time instead of silently shipping a broken (or impossible) client bundle. Keep it minimal:
   linking is what matters, not behavior. *)

module R = Fennec_pulse.Reactive.Make (Fennec_mongo_backend.Mini)

(* reference the reactive functor application + the pure seam so the whole of fennec.pulse + the seam +
   minimongo are linked and translated to JS *)
let () = ignore (R.publish : string -> _ -> unit)
let () = ignore (Fennec_mongo_backend.query : ?selector:_ -> ?sort:_ -> ?skip:_ -> ?limit:_ -> ?fields:_ -> unit -> _)

(* also pin the browser-shipped Accounts client into the closure — it transitively links Fur + the DDP
   client, so if a native dependency ever leaks into the client-side accounts facade (or its frontend
   deps), it surfaces in this same closure check instead of breaking a real client bundle silently *)
let () = ignore (Fennec_accounts_client.current_user_id : unit -> _)
