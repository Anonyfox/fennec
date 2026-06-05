(* Pipe-shaped HTTP response assertions: each takes a response, validates one property, and
   returns the response unchanged — so assertions chain naturally:

     server |> get "/api/health" |> status 200 |> body_contains {|"ok":true|}

   On failure, the assertion raises [Expect_failed] with a structured diagnostic: what was
   expected, what was found, and the full response context (status, headers, body preview).
   This is the Http-test analog of the Browser DSL's [expect_text] / [expect_url] — same
   philosophy (typed conditions, structured failures), different medium. *)

type diagnostic = {
  check : string; (* what was being checked: "status", "body contains", ... *)
  expected : string;
  actual : string;
  status : int;
  url : string; (* the path that was requested, for context *)
  body_preview : string; (* first 200 chars of the body *)
}

exception Expect_failed of diagnostic

let preview body = if String.length body <= 200 then body else String.sub body 0 197 ^ "..."

let fail ~check ~expected ~actual resp =
  raise (Expect_failed { check; expected; actual; status = resp.Http_client.status; url = ""; body_preview = preview resp.Http_client.body })

(* ---- response assertions (response -> response) ---- *)

let status (code : int) (resp : Http_client.response) : Http_client.response =
  if resp.status <> code then fail ~check:"status" ~expected:(string_of_int code) ~actual:(string_of_int resp.status) resp;
  resp

let status_2xx (resp : Http_client.response) : Http_client.response =
  if resp.status < 200 || resp.status > 299 then fail ~check:"status 2xx" ~expected:"200–299" ~actual:(string_of_int resp.status) resp;
  resp

let body_contains (needle : string) (resp : Http_client.response) : Http_client.response =
  if not (Fennec_hunt_util.contains resp.body needle) then fail ~check:"body contains" ~expected:needle ~actual:(preview resp.body) resp;
  resp

let body_is (expected : string) (resp : Http_client.response) : Http_client.response =
  if resp.body <> expected then fail ~check:"body is" ~expected ~actual:(preview resp.body) resp;
  resp

let header_is (name : string) (expected : string) (resp : Http_client.response) : Http_client.response =
  match Http_client.header_value name resp with
  | Some v when v = expected -> resp
  | Some v -> fail ~check:(Printf.sprintf "header %S" name) ~expected ~actual:v resp
  | None -> fail ~check:(Printf.sprintf "header %S" name) ~expected ~actual:"(absent)" resp

let header_contains (name : string) (needle : string) (resp : Http_client.response) : Http_client.response =
  match Http_client.header_value name resp with
  | Some v when Fennec_hunt_util.contains v needle -> resp
  | Some v -> fail ~check:(Printf.sprintf "header %S contains" name) ~expected:needle ~actual:v resp
  | None -> fail ~check:(Printf.sprintf "header %S contains" name) ~expected:needle ~actual:"(absent)" resp

let has_header (name : string) (resp : Http_client.response) : Http_client.response =
  match Http_client.header_value name resp with
  | Some _ -> resp
  | None -> fail ~check:(Printf.sprintf "has header %S" name) ~expected:"(present)" ~actual:"(absent)" resp

(* ---- render a diagnostic for the test reporter ---- *)

let render_diagnostic (d : diagnostic) : string =
  Printf.sprintf "  %s: expected %s, got %s\n  response: %d\n  body: %s" d.check d.expected d.actual d.status d.body_preview
