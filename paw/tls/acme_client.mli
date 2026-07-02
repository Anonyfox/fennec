(* The low-level ACME (RFC 8555) protocol client behind {!Acme} — one function running the whole
   HTTP-01 (or delegated DNS-01) order flow against a directory URL. JWS/JWK/CSR plumbing is private.
   Most consumers want {!Acme}, the managed on-demand certificate source; this surface exists for the
   pebble end-to-end wire test and direct protocol use. *)

val obtain :
  net:'a Eio.Net.t ->
  clock:'b Eio.Time.clock ->
  ?authenticator:X509.Authenticator.t ->
  ?solve_dns01:(name:string -> value:string -> unit -> unit) ->
  directory:string ->
  account_key:X509.Private_key.t ->
  email:string ->
  domains:string list ->
  provision:(token:string -> key_auth:string -> unit) ->
  cleanup:(token:string -> unit) ->
  unit ->
  (string * string, string) result
(** Run the full order flow for [domains] (one SAN certificate): account, order, challenges,
    finalize, download. [provision ~token ~key_auth] must make the token reachable at
    [/.well-known/acme-challenge/<token>] (the manager's :80 listener does); [cleanup] removes it.
    [?solve_dns01 ~name ~value] switches to the DNS-01 challenge (wildcard domains): install the
    TXT record and return the thunk that removes it. [?authenticator] pins the directory's TLS
    (the pebble test's self-signed CA). Returns [(cert_chain_pem, server_key_pem)] or a
    human-readable error. *)
