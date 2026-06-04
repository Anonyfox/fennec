(* Prebuilt paws — the framework's batteries, each a plain [Paw.t] you drop into
   an endpoint pipeline (or write your own; a paw is trivial to unit-test). The
   server already applies compression + ETag/304 + Date + Content-Length to every
   response (see Responder), so those are not paws — they are unconditional and
   correct for all responses. The paws here are the OPT-IN, ordered concerns. *)

module Conn = Fennec_paw.Conn
module Paw = Fennec_paw.Paw
module Route = Fennec_paw.Route
module Assigns = Fennec_paw.Assigns
module H = Fennec_core.Http

(* constant-time string equality (no early exit) — for comparing secrets/credentials *)
let constant_eq (a : string) (b : string) : bool =
  String.length a = String.length b
  &&
  let acc = ref 0 in
  String.iteri (fun i ch -> acc := !acc lor (Char.code ch lxor Char.code b.[i])) a;
  !acc = 0

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
  let meth = H.string_of_meth (Conn.meth c) in
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

(* ---- request id: tag each request with a unique id (reusing an inbound one for
   trace propagation), in an assign and a response header. Domain-SAFE: a one-time
   CSPRNG prefix + an Atomic counter, so ids stay unique across worker domains. ---- *)
let rid_prefix =
  let bytes =
    try
      let ic = open_in_bin "/dev/urandom" in
      Fun.protect ~finally:(fun () -> close_in_noerr ic) (fun () -> really_input_string ic 4)
    with _ -> "seed"
  in
  String.concat "" (List.init (String.length bytes) (fun i -> Printf.sprintf "%02x" (Char.code bytes.[i])))
let rid_counter = Atomic.make 0
let request_id_key : string Assigns.key = Assigns.key "fennec.request_id"

let request_id ?(header = "x-request-id") () : Paw.t =
 fun c ->
  let id =
    match Conn.req_header c header with
    | Some v when v <> "" -> v
    | _ -> Printf.sprintf "%s-%x" rid_prefix (Atomic.fetch_and_add rid_counter 1)
  in
  Conn.set_header (Conn.assign c request_id_key id) header id

(* the request id assigned by {!request_id}, if any *)
let current_request_id (c : Conn.t) : string option = Conn.get c request_id_key

(* ---- method override: let an HTML form POST act as PUT/PATCH/DELETE, via a
   [_method] form field or the X-HTTP-Method-Override header. ---- *)
let method_override ?(field = "_method") ?(header = "x-http-method-override") () : Paw.t =
 fun c ->
  if Conn.meth c <> H.POST then c
  else
    let ov = match Conn.req_header c header with Some v -> Some v | None -> Conn.body_param c field in
    match Option.map String.uppercase_ascii ov with
    | Some (("PUT" | "PATCH" | "DELETE") as m) -> Conn.override_method c (H.meth_of_string m)
    | _ -> c

(* ---- HTTP Basic auth: 401 with a challenge unless the credentials match. ---- *)
let basic_auth ~username ~password ?(realm = "Restricted") () : Paw.t =
 fun c ->
  let ok =
    match Conn.req_header c "authorization" with
    | Some v when String.length v > 6 && String.sub v 0 6 = "Basic " -> (
      match Base64.decode (String.sub v 6 (String.length v - 6)) with
      | Ok creds -> constant_eq creds (username ^ ":" ^ password)
      | Error _ -> false)
    | _ -> false
  in
  if ok then c
  else
    Conn.text ~status:401 (Conn.set_header c "www-authenticate" (Printf.sprintf "Basic realm=\"%s\"" realm))
      "Unauthorized"

(* ---- force HTTPS: redirect plain-http requests to https (honouring an upstream
   X-Forwarded-Proto from a TLS-terminating proxy). ---- *)
let force_https ?(status = 301) () : Paw.t =
 fun c ->
  let proto = match Conn.req_header c "x-forwarded-proto" with Some p -> p | None -> Conn.scheme c in
  if String.lowercase_ascii proto = "https" || Conn.host c = "" then c
  else
    let qs = (Conn.req c).H.query_string in
    let target = "https://" ^ Conn.host c ^ Conn.path c ^ (if qs = "" then "" else "?" ^ qs) in
    Conn.redirect ~status c target

(* ---- metrics/telemetry: time each request and report (method, path, status,
   duration) once the response is finalized. ---- *)
let metrics (report : meth:string -> path:string -> status:int -> duration_ms:float -> unit) : Paw.t =
 fun c ->
  let t0 = Unix.gettimeofday () in
  let meth = H.string_of_meth (Conn.meth c) and path = Conn.path c in
  Conn.before_send c (fun r ->
      report ~meth ~path ~status:r.H.status ~duration_ms:((Unix.gettimeofday () -. t0) *. 1000.0);
      r)
