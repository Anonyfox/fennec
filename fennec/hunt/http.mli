(* fennec_hunt.Http — full-featured, deterministic HTTP testing.

   Every request is ONE call → ONE response → immediate pass or fail.
   Works against any URL — local, remote, spawned or pre-existing.

   {[open Fennec_hunt.Http

     let () = hunt "my API" ~url:"http://localhost:4000" ~spawn:["./server"] @@ fun () ->

       check "create user" (fun () ->
         post "/users" ~json:(`Assoc [("name", `String "alice")])
           ~expect:[status 201; json_path_is "name" "alice"]);

       check "list users" (fun () ->
         get "/users" ~expect:[status 200; is_json; json_length "items" 1])
   ]} *)

(** {1 Types} *)

type response = { status : int; headers : (string * string) list; body : string }
type assertion = response -> unit

(** {1 The hunt block} *)

(** [hunt label ?url ?spawn ?env ?timeout ?request_timeout body] — REGISTER a test suite against a
    target server; it runs when {!run} runs (so the runner can filter by source file). Prefer
    [let%http]; this is the no-ppx form. The target is [~url] if given, else the harness-assigned
    [FENNEC_TEST_URL] (set per-suite by [fennec test], so a suite gets its own isolated instance and
    can't hardcode a colliding port), else a clear error. Optionally spawns a command and waits for
    the target to respond. [~timeout] is the readiness deadline (default 30s); [~request_timeout] is
    the default per-request deadline (default 10s, overridable per request) — a server that accepts
    but never answers fails the check, not the whole run. *)
val hunt :
  string ->
  ?url:string ->
  ?spawn:string list ->
  ?env:string array ->
  ?timeout:float ->
  ?request_timeout:float ->
  (unit -> unit) ->
  unit

(** ppx-generated registration ([let%http]), carrying the source file for [--only-file]. *)
val hunt_loc :
  name:string ->
  file:string ->
  ?url:string ->
  ?spawn:string list ->
  ?env:string array ->
  ?timeout:float ->
  ?request_timeout:float ->
  (unit -> unit) ->
  unit

(** Run every registered suite (honouring [--grep] on checks and [--only-file] on suites), print
    the per-suite tallies, and return [0] if all passed else [1]. The whole body of a runner:
    [let () = exit (Fennec_hunt.Http.run ())]. *)
val run : unit -> int

(** {1 Test cases} *)

(** [check label body] — one test case. Fresh cookie jar. Reports pass/fail. *)
val check : string -> (unit -> unit) -> unit

(** {1 Multipart parts (file uploads)} *)

(** One part of a [multipart/form-data] body — a text field or a file. Construct with
    {!field} / {!file}; pass a list as [~multipart]. *)
type part

(** A plain text field: [field name value]. *)
val field : string -> string -> part

(** A file part: [file ~name ~filename ?content_type content]. *)
val file : name:string -> filename:string -> ?content_type:string -> string -> part

(** {1 Requests}

    All methods send ONE request and store the response. Body sources (highest priority first):
    [~json] (serializes + sets Content-Type), [~multipart] (file uploads + sets Content-Type),
    [~form] (URL-encodes + sets Content-Type), [~body] (raw string). [~query] appends query
    parameters to the path. [~host] sets the Host header. [~timeout] overrides the per-request
    deadline (default from [hunt ~request_timeout]). [~follow:true] chases 3xx redirects to the
    final response (re-GETting each Location with refreshed cookies; bounded to 10 hops). Cookies
    from prior responses in the same [check] are sent automatically. *)

val get : ?headers:(string * string) list -> ?host:string -> ?query:(string * string) list -> ?follow:bool -> ?timeout:float -> ?expect:assertion list -> string -> unit
val post : ?headers:(string * string) list -> ?host:string -> ?body:string -> ?query:(string * string) list -> ?form:(string * string) list -> ?json:Yojson.Safe.t -> ?multipart:part list -> ?follow:bool -> ?timeout:float -> ?expect:assertion list -> string -> unit
val put : ?headers:(string * string) list -> ?host:string -> ?body:string -> ?query:(string * string) list -> ?form:(string * string) list -> ?json:Yojson.Safe.t -> ?multipart:part list -> ?follow:bool -> ?timeout:float -> ?expect:assertion list -> string -> unit
val patch : ?headers:(string * string) list -> ?host:string -> ?body:string -> ?query:(string * string) list -> ?form:(string * string) list -> ?json:Yojson.Safe.t -> ?multipart:part list -> ?follow:bool -> ?timeout:float -> ?expect:assertion list -> string -> unit
val delete : ?headers:(string * string) list -> ?host:string -> ?query:(string * string) list -> ?follow:bool -> ?timeout:float -> ?expect:assertion list -> string -> unit
val head : ?headers:(string * string) list -> ?host:string -> ?query:(string * string) list -> ?follow:bool -> ?timeout:float -> ?expect:assertion list -> string -> unit
val options : ?headers:(string * string) list -> ?host:string -> ?query:(string * string) list -> ?follow:bool -> ?timeout:float -> ?expect:assertion list -> string -> unit

(** {1 Async — explicit, bounded polling} *)

(** [eventually ?within ?interval body] re-runs [body] until it stops raising (its assertions
    all pass) or [within] seconds elapse (default 5.0; polling every [interval], default 0.2s),
    then re-raises the last failure. For genuinely async expectations — poll a job's status
    until it's done, wait for eventual consistency. This is the ONE place a test waits after
    setup; it is NOT for masking flaky assertions (wrap only what is truly asynchronous).
    {[ eventually (fun () -> get "/jobs/42" ~expect:[json_path_is "state" "done"]) ]} *)
val eventually : ?within:float -> ?interval:float -> (unit -> unit) -> unit

(** {1 Status assertions} *)

val status : int -> assertion
val status_2xx : assertion
val status_3xx : assertion
val status_4xx : assertion
val status_5xx : assertion

(** Status is anything EXCEPT the given code (e.g. [status_not 500] for smoke tests). *)
val status_not : int -> assertion

(** {1 Body assertions} *)

val body_contains : string -> assertion
val body_is : string -> assertion
val body_not_contains : string -> assertion
val body_length : int -> assertion
val min_body_length : int -> assertion

(** Body matches a PCRE regex pattern. *)
val body_matches : string -> assertion

(** Body is empty (zero bytes). *)
val body_empty : assertion

(** Body is non-empty. *)
val body_not_empty : assertion

(** {1 Header assertions} *)

val header_is : string -> string -> assertion
val header_contains : string -> string -> assertion
val has_header : string -> assertion
val no_header : string -> assertion

(** {1 Content-type shorthands} *)

val content_type : string -> assertion
val is_json : assertion
val is_html : assertion

(** {1 Redirect + timing assertions} *)

(** Status 3xx + Location header contains the target. *)
val redirect_to : string -> assertion

(** Response time must be under [ms] milliseconds. *)
val max_elapsed : float -> assertion

(** {1 JSON assertions (dotted path, e.g. ["user.name"])} *)

(** Assert a dotted JSON path equals a string value. *)
val json_path_is : string -> string -> assertion

(** Assert a dotted JSON path contains a substring (string values only). *)
val json_path_contains : string -> string -> assertion

(** Assert a dotted JSON path exists (any type). *)
val json_has : string -> assertion

(** Assert a JSON array at the path has exactly [n] elements. *)
val json_length : string -> int -> assertion

(** {1 JSON type assertions} *)

val json_is_string : string -> assertion
val json_is_number : string -> assertion
val json_is_bool : string -> assertion
val json_is_null : string -> assertion
val json_is_array : string -> assertion

(** Assert a JSON string field at the path matches a PCRE regex. *)
val json_path_matches : string -> string -> assertion

(** Assert a JSON string field is a UUID (hex-8-4-4-4-12). *)
val json_is_uuid : string -> assertion

(** Assert a JSON string field is an ISO 8601 datetime. *)
val json_is_datetime : string -> assertion

(** {1 Cookie assertions (on Set-Cookie in the response)} *)

(** Assert the response sets a cookie with this name. *)
val has_cookie : string -> assertion

(** Assert the response does NOT set a cookie with this name. *)
val no_cookie : string -> assertion

(** {1 Custom assertion — the escape hatch} *)

(** [expect f] runs [f r] on the response. For anything the built-in assertions don't cover. *)
val expect : (response -> unit) -> assertion

(** {1 Extractors — from the last response} *)

(** Read a header value. Raises if absent. *)
val header : string -> string

val header_opt : string -> string option
val response_body : unit -> string
val response_status : unit -> int
val elapsed_ms : unit -> float

(** Extract a top-level JSON field (string value, or stringified). *)
val json_field : string -> string

(** Extract a nested JSON value via dotted path (string value, or stringified). *)
val json_path : string -> string

(** Parse the full response body as JSON. *)
val json : unit -> Yojson.Safe.t

(** Read a cookie value from the jar (accumulated from prior responses in this check). *)
val cookie : string -> string

val cookie_opt : string -> string option

(** {1 Helpers} *)

(** Authorization header for HTTP Basic Auth. *)
val basic_auth : string -> string -> string * string

(** Authorization header for Bearer token auth. *)
val bearer : string -> string * string

(** Content-Type header for JSON bodies. (Prefer [~json] on the request instead.) *)
val json_content_type : string * string

(** {1 Internal — exposed for tests; not a stable API}

    Pure cores of the I/O features, with effects (clock, sleep) injected so they can be
    unit-tested deterministically without a server. Subject to change; do not depend on this. *)
module For_test : sig
  (** The pure poll policy behind {!eventually}: re-run [body] until it stops raising or
      [now ()] passes the [within] deadline, sleeping [interval] between tries. *)
  val poll : now:(unit -> float) -> sleep:(float -> unit) -> within:float -> interval:float -> (unit -> unit) -> unit

  (** Decode a chunked transfer-encoding body to its content. Total — best-effort on malformed input. *)
  val decode_chunked : string -> string

  (** Encode a multipart/form-data body with the given boundary (the request uses a fixed one). *)
  val encode_multipart : boundary:string -> part list -> string

  (** The pure redirect-following policy: from [first], while [location] yields a hop, [fetch]
      it, up to [max] hops. Generic over the response type. *)
  val follow_redirects : max:int -> location:('r -> string option) -> fetch:(string -> 'r) -> 'r -> 'r

  (** Resolve a Location header to a path on the current target. *)
  val redirect_path : string -> string

  (** Parse a URL into [(scheme, host, port, base_path)]. Total. *)
  val parse_url : string -> string * string * int * string

  (** URL-encode query/form pairs. *)
  val encode_query : (string * string) list -> string
  val encode_form : (string * string) list -> string * string

  (** Parse [Set-Cookie] response headers into [(name, value)] pairs. *)
  val parse_set_cookies : (string * string) list -> (string * string) list

  (** Merge new cookies into a jar (new values overwrite by name). *)
  val update_jar : (string * string) list -> (string * string) list -> (string * string) list
end
