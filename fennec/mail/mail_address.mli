(** An email address with an optional display name — the value behind every [From]/[To]/[Cc]/[Reply-To].

    The address is kept structured (rather than a bare string) so the MIME layer can quote / RFC-2047
    encode the display name correctly, and so the SMTP envelope can use just the bare [email]. *)

type t = {
  name : string option;  (** display name, e.g. [Some "Ada Lovelace"] *)
  email : string;  (** the bare address, e.g. ["ada@example.com"] *)
}

(** [v ?name email] builds an address, trimming [email] and treating an empty [name] as none. *)
val v : ?name:string -> string -> t

(** Best-effort parse of ["Name <email>"] or a bare ["email"]. Never raises — anything it can't split is
    taken as the bare address. *)
val of_string : string -> t

(** The bare address for the SMTP envelope ([MAIL FROM] / [RCPT TO]) — no display name, no angle brackets. *)
val email : t -> string

(** The RFC 5322 header form: ["email"] when there is no name, else ["phrase <email>"] with the phrase
    quoted (ASCII specials) or RFC 2047 'B'-encoded (non-ASCII), so a header is always 7-bit clean. *)
val to_header : t -> string

(** [header_list addrs] joins [to_header] of each with [", "] — the value of a [To]/[Cc] header. *)
val header_list : t list -> string
