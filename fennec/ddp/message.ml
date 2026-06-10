(* The DDP message model and its JSON codec. One variant per `msg` type from the spec
   (DATAFLOW.md §2), encoded to / decoded from Json.t (and strings). Absent optional fields are
   omitted on encode and tolerated on decode, matching the wire. EJSON fields/params go through the
   Ejson codec. Pure -> native + JS. *)

type error = {
  code : string;
  reason : string option;
  message : string option;
  error_type : string; (* "Meteor.Error" *)
}

type t =
  (* handshake *)
  | Connect of { session : string option; version : string; support : string list }
  | Connected of { session : string }
  | User of { id : string option }
      (* v2 extension: the connection's authenticated user (pushed on connect + on change) — drives
         the client's transparent cache purge on identity change; stock clients ignore it *)
  | Failed of { version : string }
  (* heartbeat *)
  | Ping of { id : string option }
  | Pong of { id : string option }
  (* pub/sub *)
  | Sub of { id : string; name : string; params : Bson.t list }
  | Unsub of { id : string }
  | Nosub of { id : string; error : error option }
  (* [sub] is our extended-mode tag (the contributing sub id); None in standard DDP. Meteor parsers
     ignore unknown fields, so emitting it stays compatible. *)
  | Added of { collection : string; id : string; fields : (string * Bson.t) list; sub : string option }
  | Changed of {
      collection : string;
      id : string;
      fields : (string * Bson.t) list;
      cleared : string list;
      sub : string option;
    }
  | Removed of { collection : string; id : string; sub : string option }
  | Ready of { subs : string list }
  | Added_before of {
      collection : string;
      id : string;
      fields : (string * Bson.t) list;
      before : string option;
    }
  | Moved_before of { collection : string; id : string; before : string option }
  (* rpc *)
  | Method of { method_ : string; params : Bson.t list; id : string; random_seed : Bson.t option }
  | Result of { id : string; error : error option; result : Bson.t option }
  | Updated of { methods : string list }
  (* protocol-level error *)
  | Error_msg of { reason : string; offending : Json.t option }

(* ---- small encode helpers ------------------------------------------------ *)

let s v = Json.String v
let strs xs = Json.List (List.map (fun x -> Json.String x) xs)
let params_json ps = Json.List (List.map Ejson.of_bson ps)

let some j = Some j
let opt_field kvs = if kvs = [] then None else some (Ejson.doc_to_json kvs)
let opt_params ps = if ps = [] then None else some (params_json ps)
let opt_strs = function [] -> None | xs -> some (strs xs)
let opt_before = function Some b -> some (Json.String b) | None -> some Json.Null

let error_to_json (e : error) : Json.t =
  let extra_of l = List.filter_map (fun (k, v) -> Option.map (fun j -> (k, j)) v) l in
  Json.Obj
    (extra_of
       [ ("error", some (s e.code));
         ("reason", Option.map s e.reason);
         ("message", Option.map s e.message);
         ("errorType", some (s e.error_type)) ])

let to_json (m : t) : Json.t =
  let extra_of l = List.filter_map (fun (k, v) -> Option.map (fun j -> (k, j)) v) l in
  let msg name extra = Json.Obj (("msg", s name) :: extra_of extra) in
  match m with
  | Connect { session; version; support } ->
      msg "connect"
        [ ("session", Option.map s session); ("version", some (s version));
          ("support", some (strs support)) ]
  | Connected { session } -> msg "connected" [ ("session", some (s session)) ]
  | User { id } -> msg "fennecUser" [ ("id", Option.map s id) ]
  | Failed { version } -> msg "failed" [ ("version", some (s version)) ]
  | Ping { id } -> msg "ping" [ ("id", Option.map s id) ]
  | Pong { id } -> msg "pong" [ ("id", Option.map s id) ]
  | Sub { id; name; params } ->
      msg "sub" [ ("id", some (s id)); ("name", some (s name)); ("params", opt_params params) ]
  | Unsub { id } -> msg "unsub" [ ("id", some (s id)) ]
  | Nosub { id; error } ->
      msg "nosub" [ ("id", some (s id)); ("error", Option.map error_to_json error) ]
  | Added { collection; id; fields; sub } ->
      msg "added"
        [ ("collection", some (s collection)); ("id", some (s id)); ("fields", opt_field fields);
          ("sub", Option.map s sub) ]
  | Changed { collection; id; fields; cleared; sub } ->
      msg "changed"
        [ ("collection", some (s collection)); ("id", some (s id));
          ("fields", opt_field fields); ("cleared", opt_strs cleared); ("sub", Option.map s sub) ]
  | Removed { collection; id; sub } ->
      msg "removed" [ ("collection", some (s collection)); ("id", some (s id)); ("sub", Option.map s sub) ]
  | Ready { subs } -> msg "ready" [ ("subs", some (strs subs)) ]
  | Added_before { collection; id; fields; before } ->
      msg "addedBefore"
        [ ("collection", some (s collection)); ("id", some (s id));
          ("fields", opt_field fields); ("before", opt_before before) ]
  | Moved_before { collection; id; before } ->
      msg "movedBefore"
        [ ("collection", some (s collection)); ("id", some (s id)); ("before", opt_before before) ]
  | Method { method_; params; id; random_seed } ->
      msg "method"
        [ ("method", some (s method_)); ("params", opt_params params); ("id", some (s id));
          ("randomSeed", Option.map Ejson.of_bson random_seed) ]
  | Result { id; error; result } ->
      msg "result"
        [ ("id", some (s id)); ("error", Option.map error_to_json error);
          ("result", Option.map Ejson.of_bson result) ]
  | Updated { methods } -> msg "updated" [ ("methods", some (strs methods)) ]
  | Error_msg { reason; offending } ->
      msg "error" [ ("reason", some (s reason)); ("offendingMessage", offending) ]

(* ---- decode -------------------------------------------------------------- *)

exception Bad_message of string

let str_of = function Some (Json.String s) -> s | _ -> ""
let str_opt = function Some (Json.String s) -> Some s | _ -> None
let strs_of = function
  | Some (Json.List xs) -> List.filter_map Json.to_string_opt xs
  | _ -> []
let fields_of = function Some (Json.Obj _ as j) -> Ejson.json_to_doc j | _ -> []
let params_of = function Some (Json.List xs) -> List.map Ejson.to_bson xs | _ -> []
let before_of = function Some (Json.String b) -> Some b | _ -> None

(* DDP v1 says the error code is a string, but real Meteor sends a number (e.g. 404). Accept either
   so a real Meteor [nosub]/[result] decodes losslessly. *)
let code_of = function
  | Some (Json.String s) -> s
  | Some (Json.Number f) ->
      if Float.is_integer f then string_of_int (int_of_float f) else string_of_float f
  | _ -> ""

let error_of_json = function
  | Json.Obj _ as j ->
      Some
        {
          code = code_of (Json.member "error" j);
          reason = str_opt (Json.member "reason" j);
          message = str_opt (Json.member "message" j);
          error_type = (match str_opt (Json.member "errorType" j) with Some t -> t | None -> "Meteor.Error");
        }
  | _ -> None

let of_json (j : Json.t) : t =
  let m k = Json.member k j in
  match str_opt (m "msg") with
  | None -> raise (Bad_message "missing msg")
  | Some msg -> (
      match msg with
      | "connect" ->
          Connect { session = str_opt (m "session"); version = str_of (m "version");
                    support = strs_of (m "support") }
      | "connected" -> Connected { session = str_of (m "session") }
      | "fennecUser" -> User { id = str_opt (m "id") }
      | "failed" -> Failed { version = str_of (m "version") }
      | "ping" -> Ping { id = str_opt (m "id") }
      | "pong" -> Pong { id = str_opt (m "id") }
      | "sub" -> Sub { id = str_of (m "id"); name = str_of (m "name"); params = params_of (m "params") }
      | "unsub" -> Unsub { id = str_of (m "id") }
      | "nosub" -> Nosub { id = str_of (m "id"); error = Option.bind (m "error") error_of_json }
      | "added" ->
          Added { collection = str_of (m "collection"); id = str_of (m "id");
                  fields = fields_of (m "fields"); sub = str_opt (m "sub") }
      | "changed" ->
          Changed { collection = str_of (m "collection"); id = str_of (m "id");
                    fields = fields_of (m "fields"); cleared = strs_of (m "cleared");
                    sub = str_opt (m "sub") }
      | "removed" ->
          Removed { collection = str_of (m "collection"); id = str_of (m "id"); sub = str_opt (m "sub") }
      | "ready" -> Ready { subs = strs_of (m "subs") }
      | "addedBefore" ->
          Added_before { collection = str_of (m "collection"); id = str_of (m "id");
                         fields = fields_of (m "fields"); before = before_of (m "before") }
      | "movedBefore" ->
          Moved_before { collection = str_of (m "collection"); id = str_of (m "id");
                         before = before_of (m "before") }
      | "method" ->
          Method { method_ = str_of (m "method"); params = params_of (m "params");
                   id = str_of (m "id"); random_seed = Option.map Ejson.to_bson (m "randomSeed") }
      | "result" ->
          Result { id = str_of (m "id"); error = Option.bind (m "error") error_of_json;
                   result = Option.map Ejson.to_bson (m "result") }
      | "updated" -> Updated { methods = strs_of (m "methods") }
      | "error" -> Error_msg { reason = str_of (m "reason"); offending = m "offendingMessage" }
      | other -> raise (Bad_message ("unknown msg: " ^ other)))

(* ---- string entry points ------------------------------------------------- *)

let encode (m : t) : string = Json.to_string (to_json m)
let decode (s : string) : t = of_json (Json.parse s)
