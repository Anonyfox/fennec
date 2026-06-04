(** Security headers — adds conservative defaults ([X-Content-Type-Options: nosniff],
    [X-Frame-Options: SAMEORIGIN], [Referrer-Policy: strict-origin-when-cross-origin]) to
    every response, each only if absent so an explicit header wins. Declines. *)

(** Build the security-headers paw. [extra] headers (e.g. a [Content-Security-Policy] or
    [Strict-Transport-Security]) are added and take precedence over the defaults and any
    existing same-named header (matched case-insensitively). *)
val make : ?extra:(string * string) list -> unit -> Fennec_paw.Paw.t
