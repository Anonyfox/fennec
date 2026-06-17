(** Recover the real client IP and scheme behind a reverse proxy / load balancer, by reading
    [X-Forwarded-For] and [X-Forwarded-Proto] — but only when the connecting peer is itself trusted,
    so a direct client cannot forge them. The client IP is the rightmost forwarded entry that isn't a
    trusted proxy (proxies append the peer they saw, so this skips anything spoofed on the left and
    walks past known proxies); the scheme is the original (leftmost) forwarded proto. Both become conn
    overrides, so {!Conn.remote_ip} / {!Conn.scheme} and every paw built on them (Force_https,
    Rate_limit, Logger, …) observe the true values.

    Transparent — it never answers. Mount it FIRST, ahead of anything that reads the IP or scheme:

    {[ endpoint |> Endpoint.prepend (Trusted_proxy.make ()) |> Endpoint.use (Force_https.make ()) |> … ]} *)

(** [true] for the IP ranges a reverse proxy realistically occupies — IPv4 loopback ([127.0.0.0/8]),
    RFC1918 private ([10/8], [172.16/12], [192.168/16]), and IPv6 loopback ([::1]) / ULA ([fc00::/7]).
    This is the default [trusted] predicate. *)
val default_trusted : string -> bool

(** Build the paw. [trusted] decides whether a connecting peer (and each forwarded hop) is a trusted
    proxy whose forwarding headers may be believed — default {!default_trusted}; pass your own to trust
    a specific load-balancer subnet. [for_header] / [proto_header] override the header names (defaults
    ["x-forwarded-for"] / ["x-forwarded-proto"]). *)
val make : ?trusted:(string -> bool) -> ?for_header:string -> ?proto_header:string -> unit -> Pipeline.t
