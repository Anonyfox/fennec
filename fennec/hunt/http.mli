(* The Http testing layer — bare functions, ambient context, zero ceremony.

   {[open Fennec_hunt.Http

     let () = hunt "site" ~cmd:["fennec"; "dev"] ~port:4001 @@ fun () ->
       check "health" @@ fun () ->
         get "/health" ~expect:[status 200; body_contains "ok"]
   ]}

   See {!hunt} for the block, {!check} for test cases, and the request/assertion/extractor
   sections below. *)

(** {1 Types} *)

type response = { status : int; headers : (string * string) list; body : string }

(** An assertion on a response — used in [~expect] lists. *)
type assertion = response -> unit

(** {1 The hunt block — spawn, test, teardown} *)

(** [hunt label ?cmd ?port ?env ?timeout body] sets up a test suite:
    - Spawns the server [cmd] (if given), waits for TCP readiness on [port]
    - Runs [body] with an ambient context (all bare functions work inside)
    - Reports check results and tears down the server on exit
    Without [~cmd], the block runs without a managed server (for manual process tests).
    [~port] defaults to 4000; [~env] is appended to the environment. *)
val hunt :
  string ->
  ?cmd:string list ->
  ?port:int ->
  ?env:string array ->
  ?timeout:float ->
  (unit -> unit) ->
  unit

(** [check label body] runs one test case inside a {!hunt} block. Resets the last response,
    times the body, catches failures, and reports pass/fail with the label. *)
val check : string -> (unit -> unit) -> unit

(** {1 Request functions} *)

val get : ?headers:(string * string) list -> ?host:string -> ?expect:assertion list -> string -> unit
val post : ?headers:(string * string) list -> ?host:string -> ?body:string -> ?expect:assertion list -> string -> unit
val put : ?headers:(string * string) list -> ?host:string -> ?body:string -> ?expect:assertion list -> string -> unit
val delete : ?headers:(string * string) list -> ?host:string -> ?expect:assertion list -> string -> unit

(** {1 Assertion constructors — for [~expect] lists} *)

val status : int -> assertion
val status_2xx : assertion
val body_contains : string -> assertion
val body_is : string -> assertion
val header_is : string -> string -> assertion
val header_contains : string -> string -> assertion

(** {1 Extractors — read from the last response} *)

val header : string -> string
val response_body : unit -> string
val response_status : unit -> int
val elapsed_ms : unit -> float

(** {1 Helpers} *)

val basic_auth : string -> string -> string * string

(** {1 Process lifecycle} *)

val signal : int -> unit
val wait_port_free : ?timeout:float -> unit -> unit
val port_held : unit -> bool
