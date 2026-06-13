(* Typed HTML forms over the SAME Codec model that powers collections and DDP methods — no parallel
   model, no second validation story. An HTML form (or a GET filter form) produces stringly-typed
   [(key, value)] pairs; [parse] COERCES them into the shape's BSON (driven by [Codec.view]), then
   runs the codec's full decode + validation. So a form automatically gets the model's refinements
   ([@non_empty], [@max_len], [@email], [@min], …), and failures come back as per-field errors keyed
   by wire name — exactly what re-rendering a form with inline feedback needs. *)

module Conn = Fennec_paw.Conn
module Csrf = Fennec_server.Csrf

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

(* the structural kind of a shape with check AND option wrappers peeled (for dispatch — option
   semantics are still honored by [coerce] on the original shape in the scalar case) *)
let rec kind (v : Codec.view) : Codec.view =
  match v with Codec.V_check (_, i) | Codec.V_option i -> kind i | i -> i

(* Parse a [(key, value)] assoc (as an HTML form or query string produces) into the codec's type.
   Repeated keys feed a list field; dotted keys ([author.name]) feed a nested record; an absent
   checkbox (bool) reads as [false]; an absent non-bool field is left out so the codec's own
   required/optional/default rule decides. A coercion failure (e.g. "abc" for an int) is reported
   per-field (full path), and the matching decode "missing" error is suppressed so the user sees one
   clear message, not two. *)
let parse_assoc (c : 'a Codec.t) (data : (string * string) list) : ('a, Codec.error list) result =
  match Codec.view c with
  | Codec.V_obj top_fields ->
      let coerce_errs = ref [] and failed = ref [] in
      let add_fail path msg =
        failed := path :: !failed;
        coerce_errs := Codec.err ~code:"coerce" path msg :: !coerce_errs
      in
      let values_of key = List.filter_map (fun (k, v) -> if k = key then Some v else None) data in
      (* build the BSON document field-by-field, recursing into embedded records via dotted keys.
         [path] is the error path (record-relative); [key] is the flat form-data key (dotted). *)
      let rec build fields path key : (string * Bson.t) list =
        List.filter_map
          (fun (name, _required, shape) ->
            let k = if key = "" then name else key ^ "." ^ name in
            let p = path @ [ name ] in
            match kind shape with
            | Codec.V_bool -> Some (name, Bson.bool (match values_of k with v :: _ -> truthy v | [] -> false))
            | Codec.V_list inner ->
                let elems =
                  List.filter_map
                    (fun v -> match coerce inner v with Some b -> Some b | None -> add_fail p (coerce_msg inner); None)
                    (values_of k)
                in
                Some (name, Bson.array elems)
            | Codec.V_obj subfields ->
                let kvs = build subfields p k in
                if kvs = [] then None (* nothing submitted for this nested record → let decode decide *)
                else Some (name, Bson.Document kvs)
            | _ -> (
                match values_of k with
                | [] -> None (* absent: the codec's required/optional/default rule decides *)
                | v :: _ -> (
                    match coerce shape v with
                    | Some b -> Some (name, b)
                    | None ->
                        add_fail p (coerce_msg shape);
                        None)))
          fields
      in
      let bson_fields = build top_fields [] "" in
      (match Codec.decode c (Bson.Document bson_fields) with
      | Ok v -> if !coerce_errs = [] then Ok v else Error (List.rev !coerce_errs)
      | Error es ->
          (* drop decode errors for fields that already have a coercion error (one message, not two) *)
          let es' = List.filter (fun (e : Codec.error) -> not (List.mem e.path !failed)) es in
          Error (List.rev !coerce_errs @ es'))
  | _ -> Error [ Codec.err [] "form codec must be a record" ]

(* where to read the submitted pairs from *)
type source = Body | Query

(* [parse codec conn] parses the request body (a POSTed form) — or the query string with
   [~from:Query] — into [codec]'s type. The everyday call inside a [create]/[update] handler.

   [~inject] supplies server-controlled fields (the id, owner, timestamps) that the form must NOT
   carry — they OVERRIDE any value a client tries to submit for the same key (mass-assignment
   safety: the form "omits" them, the server fills them). [~ignore] drops named keys from the user
   input entirely (block fields a client may never set even when no server value is injected). *)
let parse ?(from = Body) ?(inject = []) ?(ignore = []) (c : 'a Codec.t) (conn : Conn.t) :
    ('a, Codec.error list) result =
  let body = match from with Body -> Conn.body_params conn | Query -> Conn.query_params conn in
  let body = List.filter (fun (k, _) -> (not (List.mem k ignore)) && not (List.mem_assoc k inject)) body in
  parse_assoc c (inject @ body)

(* the error messages attached to ONE field (by wire name) — drive inline form feedback *)
let field_errors (f : _ Codec.field) (errs : Codec.error list) : string list =
  let n = Codec.field_name f in
  List.filter_map (fun (e : Codec.error) -> match e.path with x :: _ when x = n -> Some e.msg | _ -> None) errs

(* the value the user just submitted for [name] — repopulate an input when re-rendering after error *)
let submitted (conn : Conn.t) (name : string) : string = Option.value ~default:"" (Conn.body_param conn name)

(* The canonical validation-error summary (zod's flatten / Ecto's traverse_errors shape): top-level
   (path-less) messages separated from per-field messages keyed by wire name, first-seen order
   preserved. Drives both JSON error bodies and form rendering. *)
type error_summary = { form_errors : string list; field_errors : (string * string list) list }

let summary (errs : Codec.error list) : error_summary =
  let form_errors = List.filter_map (fun (e : Codec.error) -> match e.path with [] -> Some e.msg | _ -> None) errs in
  let pairs = List.filter_map (fun (e : Codec.error) -> match e.path with f :: _ -> Some (f, e.msg) | [] -> None) errs in
  let rec group acc = function
    | [] -> acc
    | (f, m) :: rest ->
        let acc =
          if List.mem_assoc f acc then List.map (fun (k, v) -> if k = f then (k, v @ [ m ]) else (k, v)) acc
          else acc @ [ (f, [ m ]) ]
        in
        group acc rest
  in
  { form_errors; field_errors = group [] pairs }

(* ─────────────────────────── rendering ───────────────────────────
   Inputs are bound to the SAME field handles {!parse} reads, so a renamed model field is a compile
   error in BOTH directions — the form and its parser cannot drift. All emit {!Fur} vnodes (Fur
   escapes attribute values + text), so they render as a dead view or compose into a live one. *)

let attrs_of = List.map (fun (k, v) -> Fur.attr k v)

(* a hidden input — CSRF token, method override, or any carried value *)
let hidden ~name ~value : Fur.vnode =
  Fur.h "input" [ Fur.attr "type" "hidden"; Fur.attr "name" name; Fur.attr "value" value ] []

let label_for (f : _ Codec.field) (text : string) : Fur.vnode =
  Fur.h "label" [ Fur.attr "for" (Codec.field_name f) ] [ Fur.text text ]

let input ?(type_ = "text") ?(value = "") ?(attrs = []) (f : _ Codec.field) : Fur.vnode =
  let name = Codec.field_name f in
  Fur.h "input"
    (Fur.attr "type" type_ :: Fur.attr "name" name :: Fur.attr "id" name :: Fur.attr "value" value :: attrs_of attrs)
    []

let textarea ?(value = "") ?(attrs = []) (f : _ Codec.field) : Fur.vnode =
  let name = Codec.field_name f in
  Fur.h "textarea" (Fur.attr "name" name :: Fur.attr "id" name :: attrs_of attrs) [ Fur.text value ]

let checkbox ?(checked = false) ?(attrs = []) (f : _ Codec.field) : Fur.vnode =
  let name = Codec.field_name f in
  let base = [ Fur.attr "type" "checkbox"; Fur.attr "name" name; Fur.attr "id" name; Fur.attr "value" "on" ] in
  Fur.h "input" ((base @ if checked then [ Fur.attr "checked" "checked" ] else []) @ attrs_of attrs) []

(* a file-upload input. Files are NOT codec data, so this binds a plain [name] (not a field handle);
   the handler reads the upload with [Conn.file conn name]. Requires the form to be [~multipart]. *)
let file_input ?(attrs = []) ~name () : Fur.vnode =
  Fur.h "input" (Fur.attr "type" "file" :: Fur.attr "name" name :: Fur.attr "id" name :: attrs_of attrs) []

(* a <select> bound to a field; [options] is [(value, label)]; [selected] marks the current one *)
let select ?selected ?(attrs = []) ~options (f : _ Codec.field) : Fur.vnode =
  let name = Codec.field_name f in
  let opt (value, lbl) =
    let a = [ Fur.attr "value" value ] @ if selected = Some value then [ Fur.attr "selected" "selected" ] else [] in
    Fur.h "option" a [ Fur.text lbl ]
  in
  Fur.h "select" (Fur.attr "name" name :: Fur.attr "id" name :: attrs_of attrs) (List.map opt options)

let submit ?(attrs = []) (text : string) : Fur.vnode =
  Fur.h "button" (Fur.attr "type" "submit" :: attrs_of attrs) [ Fur.text text ]

(* the inline error list for a field (empty → no node) *)
let errors_block (msgs : string list) : Fur.vnode =
  match msgs with
  | [] -> Fur.frag []
  | _ -> Fur.h "ul" [ Fur.class_ "field-errors" ] (List.map (fun m -> Fur.h "li" [] [ Fur.text m ]) msgs)

(* A labeled field row: <label> + control + inline errors, in one call. [kind] picks the control;
   [value]/[errors] repopulate + annotate after a failed submit (use {!submitted}/{!field_errors}). *)
let field ?(label = "") ?(kind = `Text) ?(value = "") ?(errors = []) ?(attrs = []) (f : _ Codec.field) : Fur.vnode =
  let control =
    match kind with
    | `Text -> input ~type_:"text" ~value ~attrs f
    | `Email -> input ~type_:"email" ~value ~attrs f
    | `Password -> input ~type_:"password" ~value ~attrs f
    | `Number -> input ~type_:"number" ~value ~attrs f
    | `Date -> input ~type_:"date" ~value ~attrs f
    | `Textarea -> textarea ~value ~attrs f
    | `Checkbox -> checkbox ~checked:(truthy value) ~attrs f
  in
  let lbl = if label = "" then Fur.frag [] else label_for f label in
  Fur.h "div" [ Fur.class_ "field" ] [ lbl; control; errors_block errors ]

(* The <form> wrapper. [method_] is the HTML method; [override] simulates PUT/PATCH/DELETE via the
   [_method] field (pair with the Method_override paw); [csrf] embeds the token in [_csrf_token]
   (pair with the Csrf paw — mint it with [Csrf.token] in the handler). *)
let form ?(method_ = `POST) ~action ?csrf ?override ?(multipart = false) ?(attrs = []) (children : Fur.vnode list) :
    Fur.vnode =
  let m = match method_ with `GET -> "get" | `POST -> "post" in
  let hiddens =
    (match override with Some verb -> [ hidden ~name:"_method" ~value:verb ] | None -> [])
    @ (match csrf with Some tok -> [ hidden ~name:"_csrf_token" ~value:tok ] | None -> [])
  in
  let enc = if multipart then [ Fur.attr "enctype" "multipart/form-data" ] else [] in
  Fur.h "form" ((Fur.attr "action" action :: Fur.attr "method" m :: enc) @ attrs_of attrs) (hiddens @ children)

(* ───────────────────────── the bound form (to_form) ─────────────────────────
   A [ctx] carries the request (to repopulate values), the validation errors to show, and the form's
   chrome (action / method / CSRF / override). [bound] then renders ONE field with everything filled
   in automatically — value, per-field errors, inferred input type, and HTML5 constraint attributes
   derived from the model's refinements — so the view never threads value/error/type by hand. *)

(* render a model value as form-input strings (for prefilling an edit form) *)
let rec bson_to_form_pairs name (b : Bson.t) : (string * string) list =
  match b with
  | Bson.String s -> [ (name, s) ]
  | Bson.Int i -> [ (name, string_of_int i) ]
  | Bson.Float f -> [ (name, if Float.is_integer f then string_of_int (int_of_float f) else string_of_float f) ]
  | Bson.Bool b -> [ (name, if b then "on" else "") ]
  | Bson.Date ms -> [ (name, Int64.to_string ms) ]
  | Bson.Object_id s -> [ (name, s) ]
  | Bson.Array xs -> List.concat_map (fun x -> bson_to_form_pairs name x) xs
  | _ -> []

let values_of (c : 'a Codec.t) (v : 'a) : (string * string) list =
  match c.Codec.enc v with
  | Bson.Document kvs -> List.concat_map (fun (k, b) -> bson_to_form_pairs k b) kvs
  | _ -> []

(* strip option/check/list wrappers to the leaf view kind *)
let rec leaf_view (v : Codec.view) : Codec.view =
  match v with Codec.V_check (_, i) | Codec.V_option i | Codec.V_list i -> leaf_view i | i -> i

let rec is_optional (v : Codec.view) : bool =
  match v with Codec.V_check (_, i) -> is_optional i | Codec.V_option _ -> true | _ -> false

(* infer the input control from a leaf view (number/checkbox/date are reliable; email and password
   aren't inferable from the shape alone, so pass [~kind] for those) *)
let kind_of_view (v : Codec.view) =
  match leaf_view v with
  | Codec.V_bool -> `Checkbox
  | Codec.V_int | Codec.V_float -> `Number
  | Codec.V_date -> `Date
  | _ -> `Text

let infer_kind (f : _ Codec.field) = kind_of_view (Codec.field_view f)

let fmt_float f = if Float.is_integer f then string_of_int (int_of_float f) else string_of_float f

let hint_attrs = function
  | Codec.H_min_len n -> [ ("minlength", string_of_int n) ]
  | Codec.H_max_len n -> [ ("maxlength", string_of_int n) ]
  | Codec.H_pattern p -> [ ("pattern", p) ]
  | Codec.H_min f -> [ ("min", fmt_float f) ]
  | Codec.H_max f -> [ ("max", fmt_float f) ]
  | _ -> []

(* the HTML5 constraint attributes a view's refinements imply (client-side validation for free, from
   the same model the server validates against) *)
let constraints_of_view ~required (v : Codec.view) : (string * string) list =
  let rec collect acc = function
    | Codec.V_check (h, i) -> collect (hint_attrs h @ acc) i
    | Codec.V_option i | Codec.V_list i -> collect acc i
    | _ -> acc
  in
  let cs = collect [] v in
  if required && not (is_optional v) then ("required", "required") :: cs else cs

let constraints (f : _ Codec.field) : (string * string) list =
  constraints_of_view ~required:(Codec.field_required f) (Codec.field_view f)

type ctx = {
  conn : Conn.t;
  errors : Codec.error list;
  values : (string * string) list; (* prefill source for a fresh (edit) render *)
  submitted : bool; (* did this request carry a form body? then repopulate from it *)
  action : string;
  method_ : [ `GET | `POST ];
  override : string option;
  csrf : string option;
  multipart : bool;
}

(* [start conn ~action …] opens a bound form. [~errors] are shown inline; [~values] prefill a fresh
   render (use {!values_of} on the entity for an edit form); [~secret] auto-mints a CSRF token (or
   pass [~csrf] directly); [~override] simulates PUT/PATCH/DELETE; [~multipart] sets the enctype for
   file uploads. After a failed submit the request body repopulates every field automatically. *)
let start ?(method_ = `POST) ?(errors = []) ?(values = []) ?override ?csrf ?secret ?(multipart = false) ~action conn :
    ctx =
  let csrf = match csrf with Some t -> Some t | None -> Option.map (fun s -> Csrf.token ~secret:s conn) secret in
  { conn; errors; values; submitted = Conn.body_params conn <> []; action; method_; override; csrf; multipart }

let value_for (c : ctx) (name : string) : string =
  if c.submitted then Option.value ~default:"" (Conn.body_param c.conn name)
  else Option.value ~default:"" (List.assoc_opt name c.values)

(* render ONE field of a bound form — value, errors, input type, and constraints all filled in *)
let bound (c : ctx) ?label ?kind (f : _ Codec.field) : Fur.vnode =
  let kind = match kind with Some k -> k | None -> infer_kind f in
  field ?label ~kind ~value:(value_for c (Codec.field_name f)) ~errors:(field_errors f c.errors)
    ~attrs:(constraints f) f

(* wrap a bound form's fields in the <form> (action/method/CSRF/override/enctype all from the ctx) *)
let render (c : ctx) (children : Fur.vnode list) : Fur.vnode =
  form ~method_:c.method_ ~action:c.action ?csrf:c.csrf ?override:c.override ~multipart:c.multipart children

(* "first_name" → "First name" — a default label for an auto-generated field *)
let humanize name =
  String.capitalize_ascii (String.map (fun c -> if c = '_' then ' ' else c) name)

(* Auto-generate a field row PER top-level field straight from the codec's view — no field handles,
   no per-field code (Rails scaffold / DRF browsable-API). Label is the humanized name; input type +
   constraints are inferred; [values]/[errors] prefill + annotate. The typed {!bound} is the explicit
   counterpart when you want compile-time field names + custom labels/kinds. *)
let auto ?(values = []) ?(errors = []) (c : 'a Codec.t) : Fur.vnode list =
  match Codec.view c with
  | Codec.V_obj fields ->
      List.map
        (fun (name, required, shape) ->
          let value = Option.value ~default:"" (List.assoc_opt name values) in
          let errs =
            List.filter_map (fun (e : Codec.error) -> match e.path with x :: _ when x = name -> Some e.msg | _ -> None) errors
          in
          let attrs = constraints_of_view ~required shape in
          let id_name = [ Fur.attr "name" name; Fur.attr "id" name ] in
          let control =
            match kind_of_view shape with
            | `Checkbox ->
                Fur.h "input"
                  (((Fur.attr "type" "checkbox" :: id_name) @ [ Fur.attr "value" "on" ]
                   @ if truthy value then [ Fur.attr "checked" "checked" ] else [])
                  @ attrs_of attrs)
                  []
            | k ->
                let t = match k with `Number -> "number" | `Date -> "date" | _ -> "text" in
                Fur.h "input" (((Fur.attr "type" t :: id_name) @ [ Fur.attr "value" value ]) @ attrs_of attrs) []
          in
          Fur.h "div" [ Fur.class_ "field" ]
            [ Fur.h "label" [ Fur.attr "for" name ] [ Fur.text (humanize name) ]; control; errors_block errs ])
        fields
  | _ -> []

(* ──────────────────────────── tests ──────────────────────────── *)

module H = Fennec_core.Http

let contains hay needle =
  let nh = String.length hay and nn = String.length needle in
  let rec go i = if i + nn > nh then false else if String.sub hay i nn = needle then true else go (i + 1) in
  nn = 0 || go 0

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

(* a model with an embedded record — exercises dotted-key nested parsing *)
type addr = { city : string; zip : string }
type person = { who : string; home : addr }

let addr_codec =
  Codec.(seal (record (fun city zip -> { city; zip }) |> field (req "city" (non_empty string)) (fun a -> a.city) |> field (req "zip" string) (fun a -> a.zip)))

let person_codec =
  Codec.(seal (record (fun who home -> { who; home }) |> field (req "who" (non_empty string)) (fun p -> p.who) |> field (req "home" addr_codec) (fun p -> p.home)))

let%test "parse_assoc: dotted keys populate an embedded record" =
  match parse_assoc person_codec [ ("who", "Ada"); ("home.city", "London"); ("home.zip", "NW1") ] with
  | Ok p -> p.who = "Ada" && p.home.city = "London" && p.home.zip = "NW1"
  | Error _ -> false

let%test "parse_assoc: a missing nested required field errors at its dotted path" =
  match parse_assoc person_codec [ ("who", "Ada"); ("home.zip", "NW1") ] with
  | Ok _ -> false
  | Error errs -> List.exists (fun (e : Codec.error) -> e.path = [ "home"; "city" ]) errs

let%test "rendered input binds the field's wire name (render and parse share the handle)" =
  let html = Fur.to_html (input ~value:"a@b.com" f_email) in
  contains html "name=\"email\"" && contains html "value=\"a@b.com\"" && contains html "type=\"text\""

let%test "field row renders label + control + inline errors" =
  let html = Fur.to_html (field ~label:"Email" ~value:"bad" ~errors:[ "is invalid" ] f_email) in
  contains html "<label for=\"email\">Email</label>" && contains html "value=\"bad\""
  && contains html "field-errors" && contains html "is invalid"

let%test "form embeds CSRF + method-override hidden fields" =
  let html = Fur.to_html (form ~action:"/posts/1" ~override:"PUT" ~csrf:"tok123" [ submit "Save" ]) in
  contains html "method=\"post\"" && contains html "action=\"/posts/1\""
  && contains html "name=\"_method\"" && contains html "value=\"PUT\""
  && contains html "name=\"_csrf_token\"" && contains html "value=\"tok123\""
  && contains html "type=\"submit\""

let%test "checkbox reflects checked state and submits \"on\"" =
  let f_sub = Codec.(req "subscribe" bool) in
  let on = Fur.to_html (checkbox ~checked:true f_sub) and off = Fur.to_html (checkbox ~checked:false f_sub) in
  contains on "checked=\"checked\"" && contains on "value=\"on\"" && not (contains off "checked")

let%test "infer_kind reads the field's leaf type" =
  infer_kind f_age = `Number
  && infer_kind Codec.(req "ok" bool) = `Checkbox
  && infer_kind Codec.(req "when" date) = `Date
  && infer_kind f_email = `Text

let%test "constraints derive HTML5 attributes from refinements + requiredness" =
  let cs = constraints Codec.(req "title" (non_empty (max_len 80 string))) in
  List.mem ("required", "required") cs && List.mem ("maxlength", "80") cs

let%test "an optional field is not marked required" =
  not (List.mem ("required", "required") (constraints Codec.(opt "nick" string)))

let%test "bound: a fresh form prefills from ~values; input type + constraints are inferred" =
  let conn = Conn.make (H.make_request ~meth:H.GET ~path:"/x" ()) in
  let f = start conn ~action:"/signup" ~values:[ ("age", "30") ] in
  let html = Fur.to_html (bound f f_age ~label:"Age") in
  contains html "type=\"number\"" && contains html "value=\"30\"" && contains html {|<label for="age">Age</label>|}

let%test "bound: after a failed submit the body repopulates and errors show" =
  let conn =
    Conn.make
      (H.make_request ~meth:H.POST ~path:"/x"
         ~headers:[ ("content-type", "application/x-www-form-urlencoded") ]
         ~body:"email=bad&age=10" ())
  in
  let errs = match parse_assoc signup_codec [ ("email", "bad"); ("age", "10"); ("subscribe", "on") ] with Error e -> e | Ok _ -> [] in
  let f = start conn ~action:"/signup" ~errors:errs in
  let html = Fur.to_html (bound f f_email ~label:"Email") in
  contains html "value=\"bad\"" (* repopulated *) && contains html "field-errors"

let%test "render wraps fields with the ctx's action/method/csrf/override" =
  let conn = Conn.make (H.make_request ~meth:H.GET ~path:"/x" ()) in
  let f = start conn ~action:"/posts/1" ~override:"PUT" ~csrf:"tok" in
  let html = Fur.to_html (render f [ submit "Save" ]) in
  contains html "action=\"/posts/1\"" && contains html {|name="_method"|} && contains html "value=\"PUT\""
  && contains html {|name="_csrf_token"|} && contains html "value=\"tok\""

let%test "values_of encodes a model into form-input strings" =
  let v = values_of signup_codec { email = "a@b"; age = 7; tags = [ "x"; "y" ]; subscribe = true } in
  List.assoc "email" v = "a@b" && List.assoc "age" v = "7" && List.assoc "subscribe" v = "on"
  && List.filter (fun (k, _) -> k = "tags") v = [ ("tags", "x"); ("tags", "y") ]

let%test "auto generates a labeled, typed, constrained input per field from the codec view" =
  let html = Fur.to_html (Fur.frag (auto signup_codec ~values:[ ("age", "30") ])) in
  contains html {|<label for="email">Email</label>|}
  && contains html {|name="age"|} && contains html "type=\"number\"" && contains html "value=\"30\""
  && contains html {|name="subscribe"|} && contains html "type=\"checkbox\""
  && contains html "required" (* email is required → required attr *)

let%test "file_input + multipart form set the right type and enctype" =
  let html = Fur.to_html (form ~action:"/up" ~multipart:true [ file_input ~name:"avatar" (); submit "Go" ]) in
  contains html "enctype=\"multipart/form-data\"" && contains html "type=\"file\"" && contains html {|name="avatar"|}

let%test "summary groups errors by field and separates form-level ones" =
  let s =
    summary
      [ Codec.err [] "form bad";
        Codec.err [ "email" ] "is required";
        Codec.err [ "email" ] "is invalid";
        Codec.err [ "age" ] "too small" ]
  in
  s.form_errors = [ "form bad" ]
  && s.field_errors = [ ("email", [ "is required"; "is invalid" ]); ("age", [ "too small" ]) ]

let%test "parse ~inject overrides a client-submitted value (mass-assignment safety)" =
  let body = "email=a@b.com&age=30&subscribe=on&owner=attacker" in
  let conn =
    Conn.make
      (H.make_request ~meth:H.POST ~path:"/x"
         ~headers:[ ("content-type", "application/x-www-form-urlencoded") ]
         ~body:(body ^ "&age=999") ())
  in
  (* the client also tries to set age=999; the injected age wins *)
  match parse signup_codec conn ~inject:[ ("age", "21") ] ~ignore:[ "owner" ] with
  | Ok s -> s.age = 21
  | Error _ -> false
