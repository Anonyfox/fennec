(* Pipe-shaped HTTP response assertions: [response -> response].

   Each validates one property and returns the response unchanged, so assertions chain:
   {[server |> get "/health" |> status 200 |> body_contains "ok"]}

   On failure, raises {!Expect_failed} with a structured diagnostic. *)

(** A structured failure: what was expected, what was found, and the response context. *)
type diagnostic = {
  check : string;
  expected : string;
  actual : string;
  status : int;
  url : string;
  body_preview : string;
}

exception Expect_failed of diagnostic

(** {1 Status assertions} *)

val status : int -> Http_client.response -> Http_client.response
val status_2xx : Http_client.response -> Http_client.response

(** {1 Body assertions} *)

val body_contains : string -> Http_client.response -> Http_client.response
val body_is : string -> Http_client.response -> Http_client.response

(** {1 Header assertions} *)

val header_is : string -> string -> Http_client.response -> Http_client.response
val header_contains : string -> string -> Http_client.response -> Http_client.response
val has_header : string -> Http_client.response -> Http_client.response

(** {1 Diagnostics} *)

val render_diagnostic : diagnostic -> string
