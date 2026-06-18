(* accounts_http_client.ml — the ambient outbound-HTTPS transport for the provider presets.

   The presets' default token-exchange runs inside a request handler (only a [Conn.t] in scope, no
   Eio [net]). So we mirror {!Fennec_mail}: capture the network handle once at boot into a ref, read
   it back when an exchange fires. The transport is {!Paw.Https_client} — the same prod-safe
   tls-eio/x509/ca-certs stack the server already links, with the OS trust store authenticating the
   peer by default (no silent TLS downgrade). Until [set_transport] runs, [transport ()] is
   fail-closed (every call [Error]s) so a preset never attempts an unauthenticated connection. *)

module H = Paw.Http

type response = Paw.Https_client.response = {
  status : int;
  headers : (string * string) list;
  body : string;
}

type transport = meth:string -> url:string -> headers:(string * string) list -> body:string -> (response, string) result

let header_get = Paw.Https_client.header_get

let form_encode pairs = String.concat "&" (List.map (fun (k, v) -> H.percent_encode k ^ "=" ^ H.percent_encode v) pairs)

(* the production transport: one HTTPS request per call over Paw.Https_client. A malformed URL,
   resolution failure, or TLS error raises inside Https_client.request; we catch it and return a
   transport-level error so the exchange fails closed (redirect to the provider's error route)
   rather than crashing the request fiber. *)
let default ~net ?authenticator () : transport =
 fun ~meth ~url ~headers ~body ->
  match Paw.Https_client.request ~net ?authenticator ~meth ~headers ~body url with
  | resp -> Ok resp
  | exception Failure msg -> Error msg
  | exception exn -> Error (Printexc.to_string exn)

(* the fail-closed default: no transport installed ⇒ never connect. *)
let unconfigured : transport = fun ~meth:_ ~url:_ ~headers:_ ~body:_ -> Error "Accounts: outbound HTTP transport not configured (Fennec.serve installs it at boot)"

let _transport : transport ref = ref unconfigured
let set_transport t = _transport := t
let transport () = !_transport
