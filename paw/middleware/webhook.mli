(** Verify the HMAC signature on an inbound webhook (GitHub, Slack, and any provider that signs the raw
    body with a shared secret) before trusting it.

    {[
      (* the common case — GitHub-style sha256= header over the body: *)
      hooks |> Endpoint.use (Webhook.guard ~secret:gh_secret ()) |> Endpoint.post "/gh" handle

      (* a custom scheme (e.g. Stripe's t=…,v1=…) — split the header yourself, then: *)
      if Webhook.verify ~secret ~body:(Paw.Conn.req c).body v1 then … else …
    ]} *)

(** The HMAC hash function. [SHA256] is the modern default; [SHA1] is for legacy signatures. *)
type algo = SHA1 | SHA256

(** [verify ?algo ~secret ~body signature] — does [signature] match HMAC([secret], [body])? Compares in
    constant time, tolerates a [sha256=]/[sha1=] provider prefix and upper/lower-case hex, and never
    raises on a malformed signature. Use this to wire a provider's exact scheme. *)
val verify : ?algo:algo -> secret:string -> body:string -> string -> bool

(** A paw that 401s ([Invalid signature]) unless the [header] signature verifies over the raw request
    body. [header] defaults to GitHub's [x-hub-signature-256], [algo] to [SHA256]. Mount it ahead of the
    webhook route. *)
val guard : ?header:string -> ?algo:algo -> secret:string -> unit -> Pipeline.t
