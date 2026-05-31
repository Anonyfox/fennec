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

type request = {
  meth : meth;
  path : string; (* path only, no query string *)
  query : (string * string) list;
  headers : (string * string) list;
  body : string;
}

type response = { status : int; headers : (string * string) list; body : string }

let respond ?(status = 200) ?(headers = []) ?(content_type = "text/plain; charset=utf-8") body =
  { status; headers = ("content-type", content_type) :: headers; body }

let text ?status ?headers s = respond ?status ?headers ~content_type:"text/plain; charset=utf-8" s
let html ?status ?headers s = respond ?status ?headers ~content_type:"text/html; charset=utf-8" s
let json ?status ?headers s = respond ?status ?headers ~content_type:"application/json" s

(* parse "a=1&b=two" into pairs (no percent-decoding in the lean first cut) *)
let parse_query (q : string) : (string * string) list =
  if q = "" then []
  else
    String.split_on_char '&' q
    |> List.filter_map (fun kv ->
           match String.index_opt kv '=' with
           | Some i -> Some (String.sub kv 0 i, String.sub kv (i + 1) (String.length kv - i - 1))
           | None -> if kv = "" then None else Some (kv, ""))

(* split "/path?a=1" into ("/path", query pairs) *)
let split_target (target : string) : string * (string * string) list =
  match String.index_opt target '?' with
  | Some i ->
    (String.sub target 0 i, parse_query (String.sub target (i + 1) (String.length target - i - 1)))
  | None -> (target, [])

let query req k = List.assoc_opt k req.query
