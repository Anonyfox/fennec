(* Prebuilt paws — the framework's batteries, each a plain [Paw.t] you drop into
   an endpoint pipeline (or write your own; a paw is trivial to unit-test). The
   server already applies compression + ETag/304 + Date + Content-Length to every
   response (see Responder), so those are not paws — they are unconditional and
   correct for all responses. The paws here are the OPT-IN, ordered concerns. *)

module Conn = Fennec_paw.Conn
module Paw = Fennec_paw.Paw
module Route = Fennec_paw.Route
module H = Fennec_core.Http

(* Serve static files from a web root (disk in dev, embedded map in prod). Answers
   when the path matches an asset, else declines. *)
let static (src : Static.source) : Paw.t = Route.fallthrough (Static.handler src)

(* Add common security headers to every response via a before_send hook. Declines
   (passes through) — it only registers the hook. *)
let security_headers : Paw.t =
 fun c ->
  Conn.before_send c (fun r ->
      let add k v hs = if List.mem_assoc k hs then hs else (k, v) :: hs in
      let h = r.H.headers in
      let h = add "X-Content-Type-Options" "nosniff" h in
      let h = add "X-Frame-Options" "SAMEORIGIN" h in
      let h = add "Referrer-Policy" "strict-origin-when-cross-origin" h in
      { r with H.headers = h })

(* Request logger: prints method + path, and the status via a before_send hook.
   Declines. [sink] defaults to stderr. *)
let logger ?(sink = prerr_string) () : Paw.t =
 fun c ->
  let meth =
    match Conn.meth c with
    | H.GET -> "GET" | H.POST -> "POST" | H.PUT -> "PUT" | H.DELETE -> "DELETE"
    | H.PATCH -> "PATCH" | H.HEAD -> "HEAD" | H.OPTIONS -> "OPTIONS" | H.Other s -> s
  in
  let path = Conn.path c in
  Conn.before_send c (fun r ->
      sink (Printf.sprintf "[fennec] %s %s -> %d\n" meth path r.H.status);
      r)

(* A websocket endpoint as a paw. When the request is a ws upgrade for [path],
   answer by upgrading and running [setup] on the live channel; otherwise decline.
   The actual RFC 6455 handshake/framing is the server's job (it sees the pending
   upgrade on the conn). The websocket is thus just a paw — livereload is built on
   this same primitive (see Livereload.paw). *)
let websocket (path : string) (setup : Fennec_core.Ws_channel.t -> unit) : Paw.t =
 fun c ->
  if Conn.path c = path then Conn.upgrade c setup else c
