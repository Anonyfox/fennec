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

(** [hunt label ~url ?spawn ?env ?timeout body] — test suite against [url].
    Optionally spawns a command and waits for the URL to respond. *)
val hunt : string -> url:string -> ?spawn:string list -> ?env:string array -> ?timeout:float -> (unit -> unit) -> unit

(** {1 Test cases} *)

(** [check label body] — one test case. Fresh cookie jar. Reports pass/fail. *)
val check : string -> (unit -> unit) -> unit

(** {1 Requests}

    All methods send ONE request and store the response. Body sources (highest priority first):
    [~json] (serializes + sets Content-Type), [~form] (URL-encodes + sets Content-Type),
    [~body] (raw string). [~query] appends query parameters to the path. [~host] sets the Host
    header. Cookies from prior responses in the same [check] are sent automatically. *)

val get : ?headers:(string * string) list -> ?host:string -> ?query:(string * string) list -> ?expect:assertion list -> string -> unit
val post : ?headers:(string * string) list -> ?host:string -> ?body:string -> ?query:(string * string) list -> ?form:(string * string) list -> ?json:Yojson.Safe.t -> ?expect:assertion list -> string -> unit
val put : ?headers:(string * string) list -> ?host:string -> ?body:string -> ?query:(string * string) list -> ?form:(string * string) list -> ?json:Yojson.Safe.t -> ?expect:assertion list -> string -> unit
val patch : ?headers:(string * string) list -> ?host:string -> ?body:string -> ?query:(string * string) list -> ?form:(string * string) list -> ?json:Yojson.Safe.t -> ?expect:assertion list -> string -> unit
val delete : ?headers:(string * string) list -> ?host:string -> ?query:(string * string) list -> ?expect:assertion list -> string -> unit
val head : ?headers:(string * string) list -> ?host:string -> ?query:(string * string) list -> ?expect:assertion list -> string -> unit
val options : ?headers:(string * string) list -> ?host:string -> ?query:(string * string) list -> ?expect:assertion list -> string -> unit

(** {1 Status assertions} *)

val status : int -> assertion
val status_2xx : assertion
val status_3xx : assertion
val status_4xx : assertion
val status_5xx : assertion

(** {1 Body assertions} *)

val body_contains : string -> assertion
val body_is : string -> assertion
val body_not_contains : string -> assertion
val body_length : int -> assertion
val min_body_length : int -> assertion

(** Body matches a PCRE regex pattern. *)
val body_matches : string -> assertion

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
