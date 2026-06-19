(* PROOF #5 — a Server_only secret CANNOT be leaked out of the fetcher: a COMPILE error, by type.
   [Fur.Data.Server_only] exposes only [fn] ([type 'a t] is abstract, no [get]/[run]), exactly like a
   handler's Server_only (no identity converter). So a fetcher that tries to RETURN the wrapped secret as
   its value — or to unwrap it — fails to typecheck. This file is compiled with (with-accepted-exit-codes
   (not 0)): a build SUCCESS here would be the regression. *)

let _secret : string Fur.Data.Server_only.t = Fur.Data.Server_only.fn (fun () -> "sk-live-DEADBEEF")

(* THE LEAK ATTEMPT: the fetcher must return the resource's VALUE (a string), but we try to hand back the
   Server_only-wrapped secret itself. There is no way to unwrap it (no [get]/[run] in the signature), and
   [Server_only.t] is not a [string], so this does not typecheck — the secret cannot escape into the seed. *)
let _g : string Fur.Data.t =
  Fur.Data.local "leak" ~fallback:"…" (Fur.Data.Server_only.fn (fun () -> _secret))
