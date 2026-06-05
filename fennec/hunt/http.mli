(* fennec_hunt.Http — full-featured, deterministic HTTP testing.

   Every request is ONE call → ONE response → immediate pass or fail. No polling, no retry.
   Works against any URL — local, remote, spawned or pre-existing.

   {[open Fennec_hunt.Http

     let () = hunt "my API" ~url:"http://localhost:4000" ~spawn:["./server"] @@ fun () ->
       check "health" (fun () ->
         get "/health" ~expect:[status 200; is_json; body_contains {|"ok":true|}])
   ]} *)

(** {1 Types} *)

type response = { status : int; headers : (string * string) list; body : string }
type assertion = response -> unit

(** {1 The hunt block} *)

(** [hunt label ~url ?spawn ?env ?timeout body] sets up a test suite against [url]:
    - If [~spawn] is given, starts the command and waits for [url] to respond (up to [timeout])
    - Runs [body] with an ambient context (bare functions work inside)
    - Tears down the spawned process on exit
    Without [~spawn], tests against an already-running server. *)
val hunt : string -> url:string -> ?spawn:string list -> ?env:string array -> ?timeout:float -> (unit -> unit) -> unit

(** {1 Test cases} *)

(** [check label body] — one test case. Resets cookies + last response. Reports pass/fail. *)
val check : string -> (unit -> unit) -> unit

(** {1 Requests} *)

(** All methods send ONE request and store the response. [~expect] runs assertions inline.
    [~host] sets the Host header (virtual-host testing). Cookies from prior responses in the
    same [check] are sent automatically. *)

val get : ?headers:(string * string) list -> ?host:string -> ?expect:assertion list -> string -> unit
val post : ?headers:(string * string) list -> ?host:string -> ?body:string -> ?expect:assertion list -> string -> unit
val put : ?headers:(string * string) list -> ?host:string -> ?body:string -> ?expect:assertion list -> string -> unit
val patch : ?headers:(string * string) list -> ?host:string -> ?body:string -> ?expect:assertion list -> string -> unit
val delete : ?headers:(string * string) list -> ?host:string -> ?expect:assertion list -> string -> unit
val head : ?headers:(string * string) list -> ?host:string -> ?expect:assertion list -> string -> unit
val options : ?headers:(string * string) list -> ?host:string -> ?expect:assertion list -> string -> unit

(** {1 Assertion constructors — for [~expect] lists} *)

val status : int -> assertion
val status_2xx : assertion
val status_3xx : assertion
val status_4xx : assertion
val status_5xx : assertion

val body_contains : string -> assertion
val body_is : string -> assertion
val body_not_contains : string -> assertion

val header_is : string -> string -> assertion
val header_contains : string -> string -> assertion
val has_header : string -> assertion
val no_header : string -> assertion

val content_type : string -> assertion
val is_json : assertion
val is_html : assertion

(** Status 3xx + Location header contains the target. *)
val redirect_to : string -> assertion

(** Response time must be under [ms] milliseconds. *)
val max_elapsed : float -> assertion

(** Response body must be at least [n] bytes. *)
val min_body_length : int -> assertion

(** {1 Extractors — from the last response} *)

(** Read a header value from the last response. Raises if absent. *)
val header : string -> string

(** Read a header value, or [None] if absent. *)
val header_opt : string -> string option

val response_body : unit -> string
val response_status : unit -> int
val elapsed_ms : unit -> float

(** Extract a top-level JSON field from the body (string value, or stringified). *)
val json_field : string -> string

(** {1 Helpers} *)

(** Authorization header for HTTP Basic Auth. *)
val basic_auth : string -> string -> string * string

(** Authorization header for Bearer token auth. *)
val bearer : string -> string * string

(** Content-Type header for JSON bodies. *)
val json_content_type : string * string 
