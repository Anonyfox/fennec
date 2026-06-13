(* Typed request extraction — the input half of an action. JSON bodies decode straight through the
   Codec model (API input); path/query scalars coerce with the stdlib. HTML form bodies go through
   {!Form.parse} (string coercion); this module is the JSON + scalar side. *)

module Conn = Fennec_paw.Conn
module Bson_json = Fennec_mongo_bson_json.Bson_json

(* the raw request body *)
let body (conn : Conn.t) : string = (Conn.req conn).Fennec_core.Http.body

(* decode a JSON request body into a codec's type — the API-input counterpart of {!Form.parse}.
   Validation/refinements apply exactly as for forms; errors come back per-field. *)
let json (c : 'a Codec.t) (conn : Conn.t) : ('a, Codec.error list) result =
  match Bson_json.of_string_opt (body conn) with
  | None -> Error [ { Codec.path = []; msg = "invalid JSON body" } ]
  | Some b -> Codec.decode c b

(* substring test (no Str dependency) *)
let contains hay needle =
  let nh = String.length hay and nn = String.length needle in
  let rec go i = if i + nn > nh then false else if String.sub hay i nn = needle then true else go (i + 1) in
  nn = 0 || go 0

(* Parse the request into [codec]'s type from WHICHEVER format the client sent: a JSON body
   ([Content-Type: application/json]) decodes through {!json}; anything else is treated as an HTML
   form body through {!Form.parse} (so [~inject]/[~ignore] apply). Lets ONE handler accept both a
   browser form POST and an API JSON POST. *)
let input ?inject ?ignore (c : 'a Codec.t) (conn : Conn.t) : ('a, Codec.error list) result =
  match Conn.req_header conn "content-type" with
  | Some ct when contains (String.lowercase_ascii ct) "application/json" -> json c conn
  | _ -> Form.parse ?inject ?ignore c conn

(* path / query scalars *)
let path (conn : Conn.t) (name : string) : string option = Conn.path_param conn name
let query (conn : Conn.t) (name : string) : string option = Conn.query conn name
let path_int (conn : Conn.t) (name : string) : int option = Option.bind (path conn name) (fun s -> int_of_string_opt (String.trim s))
let query_int (conn : Conn.t) (name : string) : int option = Option.bind (query conn name) (fun s -> int_of_string_opt (String.trim s))

(* a query scalar with a fallback (e.g. pagination [?page=]) *)
let query_default ~default (conn : Conn.t) (name : string) : string = Option.value ~default (query conn name)
let query_int_default ~default (conn : Conn.t) (name : string) : int = Option.value ~default (query_int conn name)

(* ──────────────────────────── tests ──────────────────────────── *)

module H = Fennec_core.Http

type t = { name : string; age : int }

let codec =
  Codec.(seal (record (fun name age -> { name; age }) |> field (req "name" (non_empty string)) (fun t -> t.name) |> field (req "age" int) (fun t -> t.age)))

let conn_with_body ?(headers = []) b = Conn.make (H.make_request ~meth:H.POST ~path:"/x" ~headers ~body:b ())

let%test "json decodes a valid body through the codec" =
  match json codec (conn_with_body {|{"name":"Ada","age":36}|}) with
  | Ok v -> v.name = "Ada" && v.age = 36
  | Error _ -> false

let%test "json reports a malformed body and codec validation errors" =
  (match json codec (conn_with_body "not json") with Error [ e ] -> e.Codec.msg = "invalid JSON body" | _ -> false)
  && match json codec (conn_with_body {|{"name":"","age":36}|}) with Error errs -> errs <> [] | Ok _ -> false

let%test "query_int_default coerces or falls back" =
  let c = Conn.make (H.make_request ~meth:H.GET ~path:"/x" ~query_string:"page=3" ()) in
  query_int_default ~default:1 c "page" = 3
  && query_int_default ~default:1 (Conn.make (H.make_request ~meth:H.GET ~path:"/x" ())) "page" = 1

let%test "input dispatches by content-type: JSON body vs form body" =
  let json_conn =
    conn_with_body ~headers:[ ("content-type", "application/json") ] {|{"name":"Ada","age":36}|}
  in
  let form_conn =
    conn_with_body ~headers:[ ("content-type", "application/x-www-form-urlencoded") ] "name=Bo&age=20"
  in
  (match input codec json_conn with Ok v -> v.name = "Ada" && v.age = 36 | Error _ -> false)
  && match input codec form_conn with Ok v -> v.name = "Bo" && v.age = 20 | Error _ -> false
