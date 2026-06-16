(** Fennec's outbound email — the [Mail] facade. One env knob ([MAIL_URL]) picks the transport, exactly
    like the data layer's [MONGO_URL]; with it unset, mail is logged to stdout so development needs zero
    configuration. Compose a {!t}, then {!send} it through the ambient transport.

    {[ let m = Mail.make ~from:(Mail.Address.v ~name:"Acme" "no-reply@acme.com")
                 ~to_:[ Mail.Address.v "ada@example.com" ] ~subject:"Hello"
                 ~text:"Welcome!" ~html:"<h1>Welcome!</h1>" ()
       in
       match Mail.send m with Ok () -> () | Error e -> log (Mail.string_of_error e) ]} *)

(** Email addresses (see {!Mail_address}). *)
module Address = Mail_address

(** The low-level SMTP client (see {!Mail_smtp}) — exposed for building a custom-port SMTP transport. *)
module Smtp = Mail_smtp

(** A file attachment (see {!Mail_mime.attachment}). *)
type attachment = Mail_mime.attachment = { filename : string; content_type : string; content : string }

(** A message to send. Provide [from], at least one recipient ([to_]/[cc]/[bcc]), and at least one body
    ([text]/[html]). [bcc] recipients receive the mail but never appear in any header. *)
type t = {
  from : Address.t;
  to_ : Address.t list;
  cc : Address.t list;
  bcc : Address.t list;
  reply_to : Address.t option;
  subject : string;
  text : string option;
  html : string option;
  headers : (string * string) list;
  attachments : attachment list;
}

(** Convenience builder with the optional fields defaulted to empty/none. *)
val make :
  from:Address.t ->
  ?to_:Address.t list ->
  ?cc:Address.t list ->
  ?bcc:Address.t list ->
  ?reply_to:Address.t ->
  subject:string ->
  ?text:string ->
  ?html:string ->
  ?headers:(string * string) list ->
  ?attachments:attachment list ->
  unit ->
  t

type error =
  | Invalid_message of string  (** the message itself is unsendable (no recipient / no body) *)
  | Transport_error of string  (** the transport failed (connection / TLS / SMTP rejection) *)

val string_of_error : error -> string

(** The serialized RFC-5322/MIME bytes — exposed for custom transports and tests. *)
val to_mime : t -> string

(** A transport delivers a message. The whole pluggability of the system is this one type. *)
type transport = t -> (unit, error) result

(** {1 Built-in transports} *)

(** Render the message to [out] (default: stdout) instead of sending — the dev default and the target of
    an unset [MAIL_URL]. Verification/reset links land in the log, clickable. *)
val log_transport : ?out:(string -> unit) -> unit -> transport

(** A test transport: it records every message and never sends. The returned thunk reads them back. *)
val capture : unit -> transport * (unit -> t list)

(** An SMTP transport over {!Smtp} (the EHLO domain defaults to the sender's domain per message). *)
val smtp_transport : sw:Eio.Switch.t -> net:_ Eio.Net.t -> Smtp.config -> transport

(** Parse [MAIL_URL] into a transport: [smtp://user:pass@host:port] (STARTTLS), [smtps://…] (implicit
    TLS), or — when unset/blank/unrecognized — {!log_transport}. The single, dynamic knob. *)
val transport_of_env : sw:Eio.Switch.t -> net:_ Eio.Net.t -> unit -> transport

(** {1 The ambient transport} *)

(** Install the ambient transport from [MAIL_URL]. {!Fennec.serve} calls this once, inside its switch,
    next to the data-layer boot. *)
val boot : sw:Eio.Switch.t -> net:_ Eio.Net.t -> unit -> unit

(** Override the ambient transport at runtime — Meteor's [Email.customTransport] (e.g. POST to a provider
    HTTP API via {!Paw.Https_client}). *)
val set_transport : transport -> unit

(** Register a hook run before each send; returning [false] silently drops the message — Meteor's
    [Email.hookSend] (filtering, allow-lists, a global send freeze in tests). *)
val on_before_send : (t -> bool) -> unit

(** Tee a copy of every accepted message to [f] (in addition to — not instead of — the real transport):
    the seam the dev outbox uses to mirror sends into a live collection. One observer; unset by default
    (production keeps it off, so nothing is retained). [f] is run inside a [try] and never breaks a send. *)
val set_dev_capture : (t -> unit) -> unit

(** Remove the {!set_dev_capture} observer. *)
val clear_dev_capture : unit -> unit

(** Send through the ambient transport (or {!log_transport} if {!boot} has not run). Runs the
    {!on_before_send} hooks first. *)
val send : t -> (unit, error) result
