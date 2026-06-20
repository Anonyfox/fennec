(* The structured ACCESS EVENT — one value describing one finished HTTP request.

   Every per-request observability concern (the human log line, an NDJSON feed for a log
   shipper, a metrics callback, the dev supervisor's pretty view) wants the SAME handful of
   facts: what was requested, how it answered, how long it took, how big the answer was. Rather
   than have each consumer re-derive those (and the old code did — three middlewares each
   re-timed the request, and the logger could not even SEE the response size), we capture them
   ONCE, in one record, at the single point where they are all finally known: the server's
   per-request chokepoint, AFTER {!Responder.finalize} (so [bytes] is the real on-the-wire,
   post-gzip length and [status] is the final status).

   This module is the shared vocabulary. {!Logger} turns an [Access.t] into text (pretty / JSON /
   logfmt / Apache); {!Dev_proto} frames it for the dev supervisor; a custom [~on_access] sink
   gets it raw. Plain record over Stdlib only — no dependency, so it can sit at the bottom of the
   stack where conn, server, and the dev wire can all reach it. *)

(* One finished request. Every field is what an operator reads in a log: the request line, the
   outcome, the cost, and the correlation hooks. Times are captured with the wall clock — the same
   source the framework already used for request timing — so [ts] is directly an epoch and
   [dur_us] is the elapsed microseconds across the request. *)
type t = {
  ts : float;            (* when the request COMPLETED, epoch seconds (Unix.gettimeofday). Drives
                            the human timestamp + the Apache/logfmt/JSON [ts] field. *)
  meth : string;         (* the HTTP method, already stringified ("GET", "POST", …) *)
  path : string;         (* the request path (no query string) *)
  status : int;          (* the FINAL response status, read after finalize (so a 304 shows as 304) *)
  dur_us : int;          (* total request duration in MICROSECONDS (handler + finalize). µs, not ms,
                            so a sub-millisecond request is still legible (e.g. 420 not "0.4ms"). *)
  bytes : int;           (* the response BODY size in bytes, AFTER finalize — i.e. post-gzip, the
                            real number of body bytes written to the socket. *)
  ip : string option;    (* the peer IP when the transport knows it (a trusted-proxy paw may have
                            rewritten it to the real client); [None] for a unix socket / unknown. *)
  req_id : string option;(* the request id, if a {!Request_id} paw ran upstream — for log↔trace
                            correlation; echoed in the response's X-Request-Id too. *)
  error : string option; (* a short error message when the request failed through the error funnel
                            (a handler exception / timeout); [None] on a normal response. *)
  version : string;      (* the HTTP version ("HTTP/1.1") — cheap to carry, used by the Apache line *)
  host : string;         (* the normalized Host header ("" when absent) — for a per-vhost log feed *)
}

(* ── the one request timer ────────────────────────────────────────────────────────────────────
   Timing used to be re-implemented three times (logger, metrics, response_time each grabbed
   [Unix.gettimeofday] at entry and diffed it in a before_send hook). They now share THIS: a start
   stamp is taken once when the conn is born (see {!Conn.make} / {!Conn.started_at}) and the elapsed
   microseconds are computed from it. One source, one truth, no drift between the log line, the
   header, and the metrics callback.

   The clock is the wall clock — the same one the old timers used. On the scale of a single request
   it is monotone enough in practice; we keep it (rather than reach for an Eio mono-clock that the
   middleware layer has no handle to) so the timer needs nothing but Stdlib and can be read from any
   paw. A backward jump can only ever clamp to 0 (see [elapsed_us]), never produce a negative dur. *)

(* a fresh start stamp (epoch seconds, sub-microsecond resolution) *)
let now () : float = Unix.gettimeofday ()

(* microseconds elapsed since [start], clamped at 0 so a clock adjustment can never log a negative
   duration. Rounds to the nearest µs. *)
let elapsed_us ~(start : float) : int =
  let us = (now () -. start) *. 1_000_000.0 in
  if us <= 0.0 then 0 else int_of_float (us +. 0.5)

(* ── construction ─────────────────────────────────────────────────────────────────────────────
   [make] builds the event from the request facts + the measured outcome. The server is the
   intended caller (it alone knows the post-finalize status/bytes); the optional fields default to
   absent so a test or a non-HTTP caller can build a minimal event. *)
let make ?(ip = None) ?(req_id = None) ?(error = None) ?(version = "HTTP/1.1") ?(host = "") ~meth
    ~path ~status ~dur_us ~bytes () : t =
  { ts = now (); meth; path; status; dur_us; bytes; ip; req_id; error; version; host }

(* the duration rendered in milliseconds (a float), for formats/humans that prefer ms over raw µs *)
let dur_ms (a : t) : float = float_of_int a.dur_us /. 1000.0

(* the status class: 2/3/4/5 (or 1 for 1xx, 0 for anything weird) — the axis colour/grouping keys on *)
let status_class (a : t) : int = a.status / 100

(* did this request FAIL? — the error funnel set a message, OR the status is 5xx. The single shared
   definition of "a request worth surfacing as a problem": the dev UI promotes these to a prominent
   line and the agent error stream records exactly these. Defined here so the policy can't drift. *)
let is_error (a : t) : bool = a.error <> None || a.status >= 500

(* ── compact JSON (hand-rolled, prod-lean: no yojson) ─────────────────────────────────────────
   ONE flat object, used by both the NDJSON log format ({!Logger}) and the dev wire frame
   ({!Dev_proto.http_line}). Encoder + a tailored parser live together here so the wire shape and
   its inverse can never drift, and {!Dev_proto} (which must stay Stdlib-only) gets the round-trip
   for free. The value space is deliberately small — strings, ints, one float, and absent optionals
   — so the parser is a few lines rather than a general JSON reader. *)

(* escape a string for a JSON double-quoted literal: the mandatory controls + quote/backslash. The
   common case (no special byte) allocates only the surrounding quotes. *)
let json_escape (s : string) : string =
  let needs = String.exists (fun c -> c = '"' || c = '\\' || Char.code c < 0x20) s in
  if not needs then s
  else begin
    let b = Buffer.create (String.length s + 8) in
    String.iter
      (fun c ->
        match c with
        | '"' -> Buffer.add_string b {|\"|}
        | '\\' -> Buffer.add_string b {|\\|}
        | '\n' -> Buffer.add_string b {|\n|}
        | '\r' -> Buffer.add_string b {|\r|}
        | '\t' -> Buffer.add_string b {|\t|}
        | c when Char.code c < 0x20 -> Buffer.add_string b (Printf.sprintf {|\u%04x|} (Char.code c))
        | c -> Buffer.add_char b c)
      s;
    Buffer.contents b
  end

(* a "key":"value" pair (string value, escaped) *)
let kv_str k v = Printf.sprintf {|"%s":"%s"|} k (json_escape v)

(* the compact one-line JSON object. [error]/[ip]/[req_id] appear ONLY when present (an absent
   optional is omitted, not rendered null — leaner lines, and the parser treats missing = None).
   [ts] is fixed to 6 decimals so the epoch round-trips exactly through {!of_compact_json}. *)
let to_compact_json (a : t) : string =
  let b = Buffer.create 192 in
  Buffer.add_char b '{';
  Buffer.add_string b (Printf.sprintf {|"ts":%.6f|} a.ts);
  Buffer.add_char b ','; Buffer.add_string b (kv_str "method" a.meth);
  Buffer.add_char b ','; Buffer.add_string b (kv_str "path" a.path);
  Buffer.add_char b ','; Buffer.add_string b (Printf.sprintf {|"status":%d|} a.status);
  Buffer.add_char b ','; Buffer.add_string b (Printf.sprintf {|"dur_us":%d|} a.dur_us);
  Buffer.add_char b ','; Buffer.add_string b (Printf.sprintf {|"bytes":%d|} a.bytes);
  (match a.ip with Some ip -> Buffer.add_char b ','; Buffer.add_string b (kv_str "ip" ip) | None -> ());
  (match a.req_id with Some id -> Buffer.add_char b ','; Buffer.add_string b (kv_str "req_id" id) | None -> ());
  Buffer.add_char b ','; Buffer.add_string b (kv_str "version" a.version);
  Buffer.add_char b ','; Buffer.add_string b (kv_str "host" a.host);
  (match a.error with Some e -> Buffer.add_char b ','; Buffer.add_string b (kv_str "error" e) | None -> ());
  Buffer.add_char b '}';
  Buffer.contents b

(* ── the inverse parser ───────────────────────────────────────────────────────────────────────
   Reads exactly the object {!to_compact_json} writes. Not a general JSON reader: it walks the
   string, and for each known key extracts the value (a JSON string with unescaping, or a number).
   Returns [None] if the mandatory keys are missing/garbled — the dev supervisor then treats the
   line as ordinary chatter rather than crashing. *)

(* unescape a JSON string body (between the quotes) — the inverse of {!json_escape}. *)
let json_unescape (s : string) : string =
  if not (String.contains s '\\') then s
  else begin
    let b = Buffer.create (String.length s) in
    let n = String.length s and i = ref 0 in
    while !i < n do
      if s.[!i] = '\\' && !i + 1 < n then begin
        (match s.[!i + 1] with
         | '"' -> Buffer.add_char b '"'
         | '\\' -> Buffer.add_char b '\\'
         | 'n' -> Buffer.add_char b '\n'
         | 'r' -> Buffer.add_char b '\r'
         | 't' -> Buffer.add_char b '\t'
         | 'u' when !i + 5 < n ->
           (* only the low-control \uXXXX our encoder emits; decode the byte *)
           (match int_of_string_opt ("0x" ^ String.sub s (!i + 2) 4) with
            | Some code -> Buffer.add_char b (Char.chr (code land 0xff)); i := !i + 4
            | None -> Buffer.add_char b s.[!i + 1]);
         | c -> Buffer.add_char b c);
        i := !i + 2
      end
      else (Buffer.add_char b s.[!i]; incr i)
    done;
    Buffer.contents b
  end

(* find the string value for ["key":"…"] starting anywhere in [s], honouring backslash escapes so a
   quote INSIDE the value doesn't end it early. Returns the unescaped value. *)
let find_str (s : string) (key : string) : string option =
  let pat = Printf.sprintf {|"%s":"|} key in
  let plen = String.length pat and n = String.length s in
  let rec find_pat i = if i + plen > n then None else if String.sub s i plen = pat then Some (i + plen) else find_pat (i + 1) in
  match find_pat 0 with
  | None -> None
  | Some start ->
    (* scan to the closing unescaped quote *)
    let rec scan j = if j >= n then None else if s.[j] = '\\' then scan (j + 2) else if s.[j] = '"' then Some j else scan (j + 1) in
    (match scan start with Some stop -> Some (json_unescape (String.sub s start (stop - start))) | None -> None)

(* find the numeric value for ["key":<num>] (int or float), returned as the raw token string *)
let find_num (s : string) (key : string) : string option =
  let pat = Printf.sprintf {|"%s":|} key in
  let plen = String.length pat and n = String.length s in
  let rec find_pat i = if i + plen > n then None else if String.sub s i plen = pat then Some (i + plen) else find_pat (i + 1) in
  match find_pat 0 with
  | None -> None
  | Some start ->
    let is_num_ch c = (c >= '0' && c <= '9') || c = '-' || c = '+' || c = '.' || c = 'e' || c = 'E' in
    let j = ref start in
    while !j < n && is_num_ch s.[!j] do incr j done;
    if !j > start then Some (String.sub s start (!j - start)) else None

let find_int s key = Option.bind (find_num s key) int_of_string_opt
let find_float s key = Option.bind (find_num s key) float_of_string_opt

(* parse the compact object back into a [t]. The mandatory keys (method/path/status/dur_us/bytes)
   must all be present + well-formed; the optionals default to absent. *)
let of_compact_json (s : string) : t option =
  match (find_str s "method", find_str s "path", find_int s "status", find_int s "dur_us", find_int s "bytes") with
  | Some meth, Some path, Some status, Some dur_us, Some bytes ->
    Some
      {
        ts = Option.value (find_float s "ts") ~default:0.0;
        meth; path; status; dur_us; bytes;
        ip = find_str s "ip";
        req_id = find_str s "req_id";
        error = find_str s "error";
        version = Option.value (find_str s "version") ~default:"HTTP/1.1";
        host = Option.value (find_str s "host") ~default:"";
      }
  | _ -> None

(* ──── access tests ──── *)

let sample =
  { ts = 1_700_000_000.0; meth = "GET"; path = "/users/42"; status = 200; dur_us = 1402; bytes = 2150;
    ip = Some "203.0.113.7"; req_id = Some "ab12-7"; error = None; version = "HTTP/1.1"; host = "example.com" }

let%test "elapsed_us never negative on a backward clock" =
  (* start in the FUTURE → elapsed clamps to 0, never a negative duration *)
  elapsed_us ~start:(now () +. 100.0) = 0

let%test "elapsed_us measures forward time" =
  let start = now () -. 0.002 (* ~2ms ago *) in
  let us = elapsed_us ~start in
  us >= 1000 (* at least ~1ms; loose bound so it's not flaky *)

let%test "dur_ms converts µs to ms" = dur_ms sample = 1.402
let%test "status_class of a 200" = status_class sample = 2
let%test "status_class of a 503" = status_class { sample with status = 503 } = 5

let%test "make fills ts and carries the fields" =
  let a = make ~meth:"POST" ~path:"/x" ~status:201 ~dur_us:500 ~bytes:3 ~ip:(Some "1.2.3.4") () in
  a.ts > 0.0 && a.meth = "POST" && a.status = 201 && a.dur_us = 500 && a.bytes = 3 && a.ip = Some "1.2.3.4"

let%test "make defaults the optionals to absent" =
  let a = make ~meth:"GET" ~path:"/" ~status:200 ~dur_us:1 ~bytes:0 () in
  a.req_id = None && a.error = None && a.host = "" && a.version = "HTTP/1.1"

(* ──── compact JSON encode/decode ──── *)

let%test "to_compact_json: shape + the fields" =
  let j = to_compact_json sample in
  j.[0] = '{'
  && j.[String.length j - 1] = '}'
  && (let has sub = let n = String.length j and m = String.length sub in let rec go i = i + m <= n && (String.sub j i m = sub || go (i+1)) in go 0 in
      has {|"method":"GET"|} && has {|"path":"/users/42"|} && has {|"status":200|}
      && has {|"dur_us":1402|} && has {|"bytes":2150|} && has {|"ip":"203.0.113.7"|}
      && has {|"req_id":"ab12-7"|} && has {|"host":"example.com"|})

let%test "compact JSON round-trips the sample" = of_compact_json (to_compact_json sample) = Some sample

let%test "compact JSON round-trips an error event with absent ip/req_id" =
  let a = { sample with ip = None; req_id = None; error = Some "Failure(\"boom\")"; status = 500 } in
  of_compact_json (to_compact_json a) = Some a

let%test "compact JSON: error omitted when absent" =
  let has sub hay = let n = String.length hay and m = String.length sub in let rec go i = i + m <= n && (String.sub hay i m = sub || go (i+1)) in go 0 in
  not (has "error" (to_compact_json sample))

let%test "json_escape / unescape round-trip a quote + newline + control" =
  let s = "a\"b\\c\nd\te\001f" in
  json_unescape (json_escape s) = s

let%test "find_str stops at the unescaped closing quote (an embedded quote survives)" =
  let a = { sample with path = {|/q?x="y"|} } in
  (of_compact_json (to_compact_json a)) = Some a

let%test "of_compact_json: garbage → None" = of_compact_json "not json at all" = None
let%test "of_compact_json: missing mandatory key → None" = of_compact_json {|{"method":"GET","path":"/"}|} = None
