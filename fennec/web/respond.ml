(* Content negotiation + JSON output, so ONE resource action can serve a browser (HTML) and an API
   client (JSON) off the same handler. JSON is encoded straight from the Codec model (its total
   [enc]), so the wire shape matches the DB + form shape — no separate serializer. *)

module Conn = Fennec_paw.Conn
module Bson_json = Fennec_mongo_bson_json.Bson_json

(* Does the client prefer JSON? Walks the Accept header in order (ignoring q-weights — the common
   ordering is decisive): the first entry that is application/json wins JSON; the first text/html,
   */*, or xhtml wins HTML. Default HTML (the browser case, and no Accept header). *)
let prefers_json (conn : Conn.t) : bool =
  match Conn.req_header conn "accept" with
  | None -> false
  | Some a ->
      let entries =
        String.split_on_char ',' a
        |> List.map (fun s ->
               let s = String.trim s in
               let s = match String.index_opt s ';' with Some i -> String.sub s 0 i | None -> s in
               String.lowercase_ascii (String.trim s))
      in
      let rec decide = function
        | [] -> false
        | "application/json" :: _ -> true
        | ("text/html" | "*/*" | "application/xhtml+xml") :: _ -> false
        | _ :: rest -> decide rest
      in
      decide entries

(* [negotiate conn ~html ~json] runs [json] when the client prefers JSON, else [html] — the one-line
   way to make an action dual-format. *)
let negotiate (conn : Conn.t) ~html ~json : Conn.t = if prefers_json conn then json conn else html conn

(* answer with a raw BSON value as JSON *)
let bson ?(status = 200) (conn : Conn.t) (b : Bson.t) : Conn.t = Conn.json ~status conn (Bson_json.to_string b)

(* answer with ONE model value, encoded through its codec *)
let model ?(status = 200) (conn : Conn.t) (c : 'a Codec.t) (v : 'a) : Conn.t = bson ~status conn (c.Codec.enc v)

(* answer with a LIST of model values *)
let models ?(status = 200) (conn : Conn.t) (c : 'a Codec.t) (vs : 'a list) : Conn.t =
  bson ~status conn (Bson.array (List.map (fun v -> c.Codec.enc v) vs))

(* the JSON error envelope — the canonical {!Form.summary} shape:
   [{ form_errors: [..], field_errors: { field: [..] } }] *)
let error_envelope (errs : Codec.error list) : Bson.t =
  let s = Form.summary errs in
  Bson.doc
    [ ("form_errors", Bson.array (List.map Bson.str s.form_errors));
      ("field_errors", Bson.doc (List.map (fun (f, ms) -> (f, Bson.array (List.map Bson.str ms))) s.field_errors)) ]

(* answer with the validation-error envelope (422 by default) *)
let errors ?(status = 422) (conn : Conn.t) (errs : Codec.error list) : Conn.t = bson ~status conn (error_envelope errs)

(* ──────────────────────────── tests ──────────────────────────── *)

module H = Fennec_core.Http

let conn_with_accept a = Conn.make (H.make_request ~meth:H.GET ~path:"/x" ~headers:[ ("accept", a) ] ())

let%test "prefers_json honors Accept ordering; defaults to HTML" =
  prefers_json (conn_with_accept "application/json")
  && prefers_json (conn_with_accept "application/json, text/html")
  && (not (prefers_json (conn_with_accept "text/html,application/json")))
  && (not (prefers_json (conn_with_accept "text/html,application/xhtml+xml,*/*")))
  && not (prefers_json (Conn.make (H.make_request ~meth:H.GET ~path:"/x" ())))

let contains hay needle =
  let nh = String.length hay and nn = String.length needle in
  let rec go i = if i + nn > nh then false else if String.sub hay i nn = needle then true else go (i + 1) in
  nn = 0 || go 0

let%test "error envelope uses the canonical form_errors/field_errors shape" =
  let b = error_envelope [ Codec.err [] "form is invalid"; Codec.err [ "email" ] "is required" ] in
  let s = Bson_json.to_string b in
  contains s "form_errors" && contains s "form is invalid" && contains s "field_errors" && contains s "email"
  && contains s "is required"
