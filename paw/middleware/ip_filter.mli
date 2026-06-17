(** Allow or deny a request by client IP — lock an admin/internal endpoint to known networks, or pin a
    webhook to its sender's ranges. Rules are exact IPs ([203.0.113.7], [::1]) or IPv4 CIDR
    ([10.0.0.0/8], [192.168.1.0/24]). Reads {!Conn.remote_ip}, so mount {!Trusted_proxy} first to
    filter the real client behind a reverse proxy. Rules are parsed once; matching is an int compare.

    {[ admin |> Endpoint.use (Ip_filter.allow [ "10.0.0.0/8"; "203.0.113.7" ]) |> … ]} *)

(** Pass only clients matching one of [ips]; everything else (and an unknown peer IP) gets [403]. *)
val allow : string list -> Pipeline.t

(** Block clients matching one of [ips] with [403]; everything else (and an unknown peer IP) passes. *)
val deny : string list -> Pipeline.t
