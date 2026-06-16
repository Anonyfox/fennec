(** Extract the SNI server-name from a TLS ClientHello record — for on-demand TLS, where the cert to
    present is chosen by the requested hostname before the handshake completes. Pure, read-only, and
    fail-safe: any malformation yields [None]. The ClientHello wire format is stable across TLS
    1.0–1.3, so no protocol library is needed. *)

(** [host_of_client_hello bytes] is the SNI hostname carried in the ClientHello record [bytes], or
    [None] if there's no server-name extension or the bytes are truncated / not a ClientHello.

    Peek the first bytes of a fresh connection, then ensure a certificate before the handshake:
    {[
      match Sni.host_of_client_hello peeked with
      | Some host -> on_demand host (* obtain / load the cert for this tenant *)
      | None -> () (* no SNI: fall through to the default certificate *)
    ]} *)
val host_of_client_hello : string -> string option
