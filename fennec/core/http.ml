(* Portable HTTP request/response — pure (Stdlib only), so the same types are
   shared by the native Eio server and (later) the Melange client router. No Eio,
   no cohttp here: this layer is just data + helpers. *)

type meth = GET | POST | PUT | DELETE | PATCH | HEAD | OPTIONS | Other of string

let meth_of_string = function
  | "GET" -> GET
  | "POST" -> POST
  | "PUT" -> PUT
  | "DELETE" -> DELETE
  | "PATCH" -> PATCH
  | "HEAD" -> HEAD
  | "OPTIONS" -> OPTIONS
  | s -> Other s

let string_of_meth = function
  | GET -> "GET" | POST -> "POST" | PUT -> "PUT" | DELETE -> "DELETE"
  | PATCH -> "PATCH" | HEAD -> "HEAD" | OPTIONS -> "OPTIONS" | Other s -> s

let%test "meth GET"           = meth_of_string "GET" = GET
let%test "meth POST"          = meth_of_string "POST" = POST
let%test "meth PATCH"         = meth_of_string "PATCH" = PATCH
let%test "meth unknown"       = meth_of_string "BREW" = Other "BREW"
let%test "meth empty"         = meth_of_string "" = Other ""
let%test "meth case-sensitive" = meth_of_string "get" = Other "get"
let%test "meth round-trips"   = string_of_meth (meth_of_string "DELETE") = "DELETE"

(* The standard reason phrase for a status code. An unknown code yields "" — which
   is a legal (empty) reason phrase — rather than a wrong one. *)
let reason_phrase = function
  | 100 -> "Continue" | 101 -> "Switching Protocols"
  | 200 -> "OK" | 201 -> "Created" | 202 -> "Accepted"
  | 203 -> "Non-Authoritative Information" | 204 -> "No Content" | 205 -> "Reset Content"
  | 206 -> "Partial Content"
  | 300 -> "Multiple Choices" | 301 -> "Moved Permanently" | 302 -> "Found"
  | 303 -> "See Other" | 304 -> "Not Modified" | 307 -> "Temporary Redirect"
  | 308 -> "Permanent Redirect"
  | 400 -> "Bad Request" | 401 -> "Unauthorized" | 402 -> "Payment Required"
  | 403 -> "Forbidden" | 404 -> "Not Found" | 405 -> "Method Not Allowed"
  | 406 -> "Not Acceptable" | 408 -> "Request Timeout" | 409 -> "Conflict"
  | 410 -> "Gone" | 411 -> "Length Required" | 412 -> "Precondition Failed"
  | 413 -> "Payload Too Large" | 414 -> "URI Too Long" | 415 -> "Unsupported Media Type"
  | 416 -> "Range Not Satisfiable" | 417 -> "Expectation Failed" | 418 -> "I'm a teapot"
  | 422 -> "Unprocessable Entity" | 425 -> "Too Early" | 426 -> "Upgrade Required"
  | 428 -> "Precondition Required" | 429 -> "Too Many Requests"
  | 431 -> "Request Header Fields Too Large" | 451 -> "Unavailable For Legal Reasons"
  | 500 -> "Internal Server Error" | 501 -> "Not Implemented" | 502 -> "Bad Gateway"
  | 503 -> "Service Unavailable" | 504 -> "Gateway Timeout"
  | 505 -> "HTTP Version Not Supported"
  | _ -> ""

let%test "reason 200" = reason_phrase 200 = "OK"
let%test "reason 201" = reason_phrase 201 = "Created"
let%test "reason 404" = reason_phrase 404 = "Not Found"
let%test "reason 422" = reason_phrase 422 = "Unprocessable Entity"
let%test "reason 429" = reason_phrase 429 = "Too Many Requests"
let%test "reason unknown" = reason_phrase 999 = ""

type request = {
  meth : meth;
  path : string;              (* path only, no query string (raw, not percent-decoded) *)
  query_string : string;      (* the raw query, parsed lazily by the conn on demand *)
  headers : (string * string) list;
  body : string;
  host : string;              (* normalized Host without a port (""=absent) *)
  scheme : string;            (* "http" | "https" *)
  remote_ip : string option;  (* the peer's IP, when the transport knows it *)
  version : string;           (* "HTTP/1.1" etc. *)
}

(* Build a request; the connection metadata defaults sensibly so tests (and a future
   client) need only the essentials. *)
let make_request ?(query_string = "") ?(headers = []) ?(body = "") ?(host = "")
    ?(scheme = "http") ?(remote_ip = None) ?(version = "HTTP/1.1") ~meth ~path () : request =
  { meth; path; query_string; headers; body; host; scheme; remote_ip; version }

type response = { status : int; headers : (string * string) list; body : string }

let respond ?(status = 200) ?(headers = []) ?(content_type = "text/plain; charset=utf-8") body =
  { status; headers = ("content-type", content_type) :: headers; body }

let text ?status ?headers s = respond ?status ?headers ~content_type:"text/plain; charset=utf-8" s
let html ?status ?headers s = respond ?status ?headers ~content_type:"text/html; charset=utf-8" s
let json ?status ?headers s = respond ?status ?headers ~content_type:"application/json" s

(* ──── respond / text / html / json ──── *)
let%test "json status"        = (json ~status:201 ~headers:[("x","y")] "{}").status = 201
let%test "json ct"            = List.assoc "content-type" (json ~status:201 ~headers:[("x","y")] "{}").headers = "application/json"
let%test "json extra header"  = List.mem ("x","y") (json ~status:201 ~headers:[("x","y")] "{}").headers
let%test "text default 200"   = (text "hi").status = 200
let%test "html ct"            = List.assoc "content-type" (html "x").headers = "text/html; charset=utf-8"

(* Percent-decode a query/form component: %XX hex escapes, and '+' as a space (the
   application/x-www-form-urlencoded convention). Allocation-free fast path when there
   is nothing to decode. *)
let percent_decode (s : string) : string =
  if not (String.contains s '%' || String.contains s '+') then s
  else begin
    let n = String.length s in
    let b = Buffer.create n in
    let hex c =
      match c with
      | '0' .. '9' -> Char.code c - Char.code '0'
      | 'a' .. 'f' -> Char.code c - Char.code 'a' + 10
      | 'A' .. 'F' -> Char.code c - Char.code 'A' + 10
      | _ -> -1
    in
    let i = ref 0 in
    while !i < n do
      (match s.[!i] with
       | '+' -> Buffer.add_char b ' '; incr i
       | '%' when !i + 2 < n ->
         let h = hex s.[!i + 1] and l = hex s.[!i + 2] in
         if h >= 0 && l >= 0 then (Buffer.add_char b (Char.chr ((h * 16) + l)); i := !i + 3)
         else (Buffer.add_char b '%'; incr i)
       | c -> Buffer.add_char b c; incr i)
    done;
    Buffer.contents b
  end

let%test "decode %20"         = percent_decode "a%20b" = "a b"
let%test "decode +"           = percent_decode "a+b" = "a b"
let%test "decode utf-8"       = percent_decode "%C3%A9" = "\xc3\xa9"
let%test "decode no-op"       = percent_decode "plain" = "plain"
let%test "decode lone %"      = percent_decode "100%" = "100%"
let%test "decode bad hex"     = percent_decode "%ZZ" = "%ZZ"

(* Percent-encode a string, escaping everything but the RFC 3986 unreserved set
   ([A-Za-z0-9-_.~]). The inverse of {!percent_decode} (which also accepts '+'). *)
let percent_encode (s : string) : string =
  let b = Buffer.create (String.length s) in
  String.iter
    (fun c ->
      match c with
      | 'A' .. 'Z' | 'a' .. 'z' | '0' .. '9' | '-' | '_' | '.' | '~' -> Buffer.add_char b c
      | c -> Buffer.add_string b (Printf.sprintf "%%%02X" (Char.code c)))
    s;
  Buffer.contents b

(* parse "a=1&b=two+words" into percent-decoded pairs *)
let parse_query (q : string) : (string * string) list =
  if q = "" then []
  else
    String.split_on_char '&' q
    |> List.filter_map (fun kv ->
           if kv = "" then None
           else
             match String.index_opt kv '=' with
             | Some i ->
               Some
                 ( percent_decode (String.sub kv 0 i),
                   percent_decode (String.sub kv (i + 1) (String.length kv - i - 1)) )
             | None -> Some (percent_decode kv, ""))

let%test "query empty"        = parse_query "" = []
let%test "query single"       = parse_query "a=1" = [("a", "1")]
let%test "query multi"        = parse_query "a=1&b=2" = [("a", "1"); ("b", "2")]
let%test "query flag"         = parse_query "debug" = [("debug", "")]
let%test "query decode +"     = parse_query "q=hello+world" = [("q", "hello world")]
let%test "query decode %"     = parse_query "q=%C3%A9" = [("q", "\xc3\xa9")]
let%test "query empty val"    = parse_query "k=" = [("k", "")]
let%test "query trailing &"   = parse_query "a=1&" = [("a", "1")]
let%test "query leading &"    = parse_query "&a=1" = [("a", "1")]
let%test "query skip &&"      = parse_query "a=1&&b=2" = [("a", "1"); ("b", "2")]
let%test "query value with =" = parse_query "a=1=2" = [("a", "1=2")]
let%test "query only &"       = parse_query "&&&" = []
let%test "query %20 in value" = parse_query "q=John%20Doe" = [("q", "John Doe")]
let%test "query decoded key"  = parse_query "a%2Bb=1" = [("a+b", "1")]

(* split "/path?a=1" into (path, raw query string) *)
let split_target (target : string) : string * string =
  match String.index_opt target '?' with
  | Some i -> (String.sub target 0 i, String.sub target (i + 1) (String.length target - i - 1))
  | None -> (target, "")

let%test "split no query"     = split_target "/path" = ("/path", "")
let%test "split with query"   = split_target "/p?a=1" = ("/p", "a=1")
let%test "split empty query"  = split_target "/p?" = ("/p", "")
let%test "split root"         = split_target "/" = ("/", "")
let%test "split empty"        = split_target "" = ("", "")
let%test "split double ?"     = split_target "/p?a?b" = ("/p", "a?b")
let%test "split query only"   = split_target "?x=1" = ("", "x=1")
