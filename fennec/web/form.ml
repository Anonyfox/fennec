(* Typed HTML forms over the SAME Codec model that powers collections and DDP methods — no parallel
   model, no second validation story. An HTML form (or a GET filter form) produces stringly-typed
   [(key, value)] pairs; [parse] COERCES them into the shape's BSON (driven by [Codec.view]), then
   runs the codec's full decode + validation. So a form automatically gets the model's refinements
   ([@non_empty], [@max_len], [@email], [@min], …), and failures come back as per-field errors keyed
   by wire name — exactly what re-rendering a form with inline feedback needs. *)

module Conn = Fennec_paw.Conn

(* strip refinement wrappers down to the structural kind underneath *)
let rec strip (v : Codec.view) : Codec.view =
  match v with Codec.V_check (_, inner) -> strip inner | v -> v

(* HTML checkbox / truthy-string semantics: a checked box submits "on" (absent when unchecked) *)
let truthy s =
  match String.lowercase_ascii (String.trim s) with
  | "on" | "true" | "1" | "yes" | "checked" -> true
  | _ -> false

(* coerce ONE submitted string into the BSON the codec expects for [view]; [None] = uncoercible *)
let rec coerce (view : Codec.view) (s : string) : Bson.t option =
  match view with
  | Codec.V_string | Codec.V_id | Codec.V_bson -> Some (Bson.str s)
  | Codec.V_int -> Option.map Bson.int (int_of_string_opt (String.trim s))
  | Codec.V_float -> Option.map Bson.float (float_of_string_opt (String.trim s))
  | Codec.V_bool -> Some (Bson.bool (truthy s))
  | Codec.V_date -> Option.map (fun ms -> Bson.Date ms) (Int64.of_string_opt (String.trim s))
  | Codec.V_option inner -> if String.trim s = "" then Some Bson.Null else coerce inner s
  | Codec.V_check (_, inner) -> coerce inner s
  | Codec.V_unit -> Some Bson.Null
  | Codec.V_list _ | Codec.V_map _ | Codec.V_obj _ | Codec.V_variant _ -> None

let coerce_msg (view : Codec.view) =
  match strip view with
  | Codec.V_int | Codec.V_float -> "must be a number"
  | Codec.V_date -> "must be a date"
  | _ -> "invalid value"

(* Parse a flat [(key, value)] assoc (as an HTML form or query string produces) into the codec's
   type. Repeated keys feed a list field; an absent checkbox (bool) reads as [false]; an absent
   non-bool field is left out so the codec's own required/optional/default rule decides. A coercion
   failure (e.g. "abc" for an int) is reported per-field, and the matching decode "missing" error is
   suppressed so the user sees one clear message, not two. *)
let parse_assoc (c : 'a Codec.t) (data : (string * string) list) : ('a, Codec.error list) result =
  match Codec.view c with
  | Codec.V_obj fields ->
      let coerce_errs = ref [] and failed = ref [] in
      let add_fail name msg =
        failed := name :: !failed;
        coerce_errs := { Codec.path = [ name ]; msg } :: !coerce_errs
      in
      let values_of name = List.filter_map (fun (k, v) -> if k = name then Some v else None) data in
      let bson_fields =
        List.filter_map
          (fun (name, _required, shape) ->
            let vals = values_of name in
            match strip shape with
            | Codec.V_bool -> Some (name, Bson.bool (match vals with v :: _ -> truthy v | [] -> false))
            | Codec.V_list inner ->
                let elems =
                  List.filter_map
                    (fun v ->
                      match coerce inner v with
                      | Some b -> Some b
                      | None ->
                          add_fail name (coerce_msg inner);
                          None)
                    vals
                in
                Some (name, Bson.array elems)
            | _ -> (
                match vals with
                | [] -> None (* absent: let the codec's required/optional/default rule decide *)
                | v :: _ -> (
                    match coerce shape v with
                    | Some b -> Some (name, b)
                    | None ->
                        add_fail name (coerce_msg shape);
                        None)))
          fields
      in
      (match Codec.decode c (Bson.Document bson_fields) with
      | Ok v -> if !coerce_errs = [] then Ok v else Error (List.rev !coerce_errs)
      | Error es ->
          let es' =
            List.filter
              (fun (e : Codec.error) -> match e.path with x :: _ -> not (List.mem x !failed) | [] -> true)
              es
          in
          Error (List.rev !coerce_errs @ es'))
  | _ -> Error [ { Codec.path = []; msg = "form codec must be a record" } ]

(* where to read the submitted pairs from *)
type source = Body | Query

(* [parse codec conn] parses the request body (a POSTed form) — or the query string with
   [~from:Query] — into [codec]'s type. The everyday call inside a [create]/[update] handler. *)
let parse ?(from = Body) (c : 'a Codec.t) (conn : Conn.t) : ('a, Codec.error list) result =
  let data = match from with Body -> Conn.body_params conn | Query -> Conn.query_params conn in
  parse_assoc c data

(* the error messages attached to ONE field (by wire name) — drive inline form feedback *)
let field_errors (f : _ Codec.field) (errs : Codec.error list) : string list =
  let n = Codec.field_name f in
  List.filter_map (fun (e : Codec.error) -> match e.path with x :: _ when x = n -> Some e.msg | _ -> None) errs

(* the value the user just submitted for [name] — repopulate an input when re-rendering after error *)
let submitted (conn : Conn.t) (name : string) : string = Option.value ~default:"" (Conn.body_param conn name)

(* ──────────────────────────── tests ──────────────────────────── *)

type signup = { email : string; age : int; tags : string list; subscribe : bool }

let signup_codec =
  Codec.(
    seal
      (record (fun email age tags subscribe -> { email; age; tags; subscribe })
      |> field (req "email" (email string)) (fun t -> t.email)
      |> field (req "age" (min_i 18 int)) (fun t -> t.age)
      |> field (opt_list "tags" (non_empty string)) (fun t -> t.tags)
      |> field (req "subscribe" bool) (fun t -> t.subscribe)))

let f_email = Codec.(req "email" (email string))
let f_age = Codec.(req "age" (min_i 18 int))

let%test "parse_assoc: valid form coerces strings to typed values" =
  match parse_assoc signup_codec [ ("email", "a@b.com"); ("age", "30"); ("tags", "x"); ("tags", "y"); ("subscribe", "on") ] with
  | Ok s -> s.email = "a@b.com" && s.age = 30 && s.tags = [ "x"; "y" ] && s.subscribe = true
  | Error _ -> false

let%test "parse_assoc: an absent checkbox reads as false (not an error)" =
  match parse_assoc signup_codec [ ("email", "a@b.com"); ("age", "30") ] with
  | Ok s -> s.subscribe = false && s.tags = []
  | Error _ -> false

let%test "parse_assoc: a non-numeric int is a per-field coercion error" =
  match parse_assoc signup_codec [ ("email", "a@b.com"); ("age", "abc"); ("subscribe", "on") ] with
  | Ok _ -> false
  | Error errs -> field_errors f_age errs = [ "must be a number" ] && field_errors f_email errs = []

let%test "parse_assoc: the model's refinements fire as field errors (email + min)" =
  match parse_assoc signup_codec [ ("email", "nope"); ("age", "10"); ("subscribe", "on") ] with
  | Ok _ -> false
  | Error errs -> field_errors f_email errs <> [] && field_errors f_age errs <> []

let%test "parse_assoc: a missing required field is reported once, by name" =
  match parse_assoc signup_codec [ ("age", "30"); ("subscribe", "on") ] with
  | Ok _ -> false
  | Error errs -> field_errors f_email errs <> []
