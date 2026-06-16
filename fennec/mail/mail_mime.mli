(** RFC 5322 + MIME serialization of an outbound message into 7-bit-clean bytes (CRLF line endings).

    Bodies are base64-encoded UTF-8 — always correct, no charset guessing. The structure is the minimal
    correct one for the content: a single [text/plain] or [text/html] part; a [multipart/alternative] when
    both text and html are present; the whole wrapped in [multipart/mixed] when there are attachments.
    [Date] and [Message-ID] are generated. Bcc recipients are deliberately NOT written to any header — they
    travel only in the SMTP envelope. *)

type attachment = {
  filename : string;
  content_type : string;  (** e.g. ["application/pdf"] *)
  content : string;  (** raw bytes — base64-encoded by this module *)
}

val serialize :
  from:Mail_address.t ->
  to_:Mail_address.t list ->
  cc:Mail_address.t list ->
  ?reply_to:Mail_address.t ->
  subject:string ->
  ?text:string ->
  ?html:string ->
  ?headers:(string * string) list ->
  ?attachments:attachment list ->
  unit ->
  string
