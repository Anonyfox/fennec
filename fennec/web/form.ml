(* Typed form INPUT over the Codec model — the input half of a handler. The stringly request a form
   (or query string) submits is COERCED into the shape's BSON (driven by [Codec.view]) and run through
   the codec's full decode + validation, so a handler gets a typed value with the model's refinements
   ([@non_empty]/[@max_len]/[@email]/[@min]…) enforced and per-field errors for re-rendering. No HTML
   building: forms are authored as plain .mlx markup; this module only READS the request. *)

module Conn = Fennec_paw.Conn

(* strip refinement wrappers to the structural kind (for the coercion message) *)
let rec strip (v : Codec.view) : Codec.view = match v with Codec.V_check (_, inner) -> strip inner | v -> v

(* HTML checkbox / truthy-string semantics: a checked box submits "on" (absent when unchecked) *)
let truthy s =
  match String.lowercase_ascii (String.trim s) with "on" | "true" | "1" | "yes" | "checked" -> true | _ -> false

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
  match strip view with Codec.V_int | Codec.V_float -> "must be a number" | Codec.V_date -> "must be a date" | _ -> "invalid value"

(* the structural kind with check AND option wrappers peeled (for dispatch) *)
let rec kind (v : Codec.view) : Codec.view = match v with Codec.V_check (_, i) | Codec.V_option i -> kind i | i -> i

(* Parse a [(key, value)] assoc (as a form/query produces) into the codec's type. Repeated keys feed
   a list field; dotted keys ([author.name]) feed a nested record; an absent checkbox reads [false];
   an absent non-bool field defers to the codec's required/optional/default rule. A coercion failure
   is reported per-field (full path); its redundant "missing" decode error is suppressed. *)
let parse_assoc (c : 'a Codec.t) (data : (string * string) list) : ('a, Codec.error list) result =
  match Codec.view c with
  | Codec.V_obj top_fields ->
      let coerce_errs = ref [] and failed = ref [] in
      let add_fail path msg =
        failed := path :: !failed;
        coerce_errs := Codec.err ~code:"coerce" path msg :: !coerce_errs
      in
      let values_of key = List.filter_map (fun (k, v) -> if k = key then Some v else None) data in
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
                if kvs = [] then None else Some (name, Bson.Document kvs)
            | _ -> (
                match values_of k with
                | [] -> None
                | v :: _ -> ( match coerce shape v with Some b -> Some (name, b) | None -> add_fail p (coerce_msg shape); None)))
          fields
      in
      let bson_fields = build top_fields [] "" in
      (match Codec.decode c (Bson.Document bson_fields) with
      | Ok v -> if !coerce_errs = [] then Ok v else Error (List.rev !coerce_errs)
      | Error es ->
          let es' = List.filter (fun (e : Codec.error) -> not (List.mem e.path !failed)) es in
          Error (List.rev !coerce_errs @ es'))
  | _ -> Error [ Codec.err [] "form codec must be a record" ]

(* where to read the submitted pairs from *)
type source = Body | Query

(* [read codec conn] coerces the request body (a POSTed form) — or the query string with [~from:Query]
   — into [codec]'s type and validates it. [~inject] supplies server-controlled fields (id, owner,
   timestamps) that OVERRIDE any client-submitted value for the same key (mass-assignment safety);
   [~ignore] drops named keys from the user input entirely. *)
let read ?(from = Body) ?(inject = []) ?(ignore = []) (c : 'a Codec.t) (conn : Conn.t) : ('a, Codec.error list) result =
  let body = match from with Body -> Conn.body_params conn | Query -> Conn.query_params conn in
  let body = List.filter (fun (k, _) -> (not (List.mem k ignore)) && not (List.mem_assoc k inject)) body in
  parse_assoc c (inject @ body)

(* the error messages attached to ONE field (by wire name) — for inline feedback when re-rendering *)
let field_errors (f : _ Codec.field) (errs : Codec.error list) : string list =
  let n = Codec.field_name f in
  List.filter_map (fun (e : Codec.error) -> match e.path with x :: _ when x = n -> Some e.msg | _ -> None) errs

(* the raw value the user just submitted for [name] — repopulate an input when re-rendering *)
let submitted (conn : Conn.t) (name : string) : string = Option.value ~default:"" (Conn.body_param conn name)

(* The canonical validation-error summary (zod's flatten / Ecto's traverse_errors shape): top-level
   (path-less) messages separated from per-field messages keyed by wire name, first-seen order kept. *)
type error_summary = { form_errors : string list; field_errors : (string * string list) list }

let summary (errs : Codec.error list) : error_summary =
  let form_errors = List.filter_map (fun (e : Codec.error) -> match e.path with [] -> Some e.msg | _ -> None) errs in
  let pairs = List.filter_map (fun (e : Codec.error) -> match e.path with f :: _ -> Some (f, e.msg) | [] -> None) errs in
  let rec group acc = function
    | [] -> acc
    | (f, m) :: rest ->
        let acc = if List.mem_assoc f acc then List.map (fun (k, v) -> if k = f then (k, v @ [ m ]) else (k, v)) acc else acc @ [ (f, [ m ]) ] in
        group acc rest
  in
  { form_errors; field_errors = group [] pairs }

(* ─────────────── form-handler view-state + outcome (server-rendered handler vocabulary) ─────────────── *)

(* The per-render context threaded into a form handler's [view]: the live conn (CSRF token + the raw
   submitted values, to repopulate inputs), the flash from a prior redirect, and the validation errors
   from a failed [read]. [view] only READS it — a plain value, so it is concurrency-safe (no globals,
   unlike the still-racy Head). The ppx builds it in the generated get/post; userland never does. *)
type ctx = { conn : Conn.t; flash : string option; errors : Codec.error list }

let ctx ~conn ~flash ~errors = { conn; flash; errors }

(* the flash banner (empty without a message — harmless to always place near the form top) *)
let flash (c : ctx) : Fur.vnode =
  match c.flash with Some m -> Fur.h "p" [ Fur.attr "class" "flash" ] [ Fur.text m ] | None -> Fur.frag []

(* this form's CSRF hidden field (token from the conn; empty without the Csrf paw) *)
let csrf (c : ctx) : Fur.vnode = Handler.csrf_field c.conn

(* repopulate an input with what the user just submitted for [name] (empty on the first GET) *)
let value (c : ctx) (name : string) : string = submitted c.conn name

(* the inline validation error(s) for ONE field by wire name (empty when it validated) — same path-head
   match as {!field_errors}/{!summary}, but keyed by the plain wire string the [view] writes. *)
let error (c : ctx) (name : string) : Fur.vnode =
  match
    List.filter_map (fun (e : Codec.error) -> match e.path with x :: _ when x = name -> Some e.msg | _ -> None) c.errors
  with
  | [] -> Fur.frag []
  | msgs -> Fur.h "ul" [ Fur.attr "class" "errors" ] (List.map (fun m -> Fur.h "li" [] [ Fur.text m ]) msgs)

(* What a form handler's [submit] returns — the framework renders it. The author chooses per case, so
   re-rendering HTML (error states, awaiting data corrections) is first-class, not a fixed branch:
   re-show the form with errors (input preserved), post-redirect-get, or an arbitrary result page. *)
type outcome =
  | Again of Codec.error list  (** re-render [view] with these field errors, 422 — codec-invalid input auto-does this *)
  | Redirect of string * string option  (** post-redirect-get: (url, optional flash) *)
  | Page of Fur.vnode  (** render an arbitrary result page (200) *)

let again (fields : (string * string) list) : outcome = Again (List.map (fun (f, m) -> Codec.err [ f ] m) fields)
let redirect ?flash (url : string) : outcome = Redirect (url, flash)
let page (v : Fur.vnode) : outcome = Page v

(* ──────────────────────────── tests ──────────────────────────── *)

module H = Fennec_core.Http

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

let%test "read/parse_assoc: valid form coerces strings to typed values" =
  match parse_assoc signup_codec [ ("email", "a@b.com"); ("age", "30"); ("tags", "x"); ("tags", "y"); ("subscribe", "on") ] with
  | Ok s -> s.email = "a@b.com" && s.age = 30 && s.tags = [ "x"; "y" ] && s.subscribe = true
  | Error _ -> false

let%test "read: an absent checkbox reads as false; missing required reports by name" =
  (match parse_assoc signup_codec [ ("email", "a@b.com"); ("age", "30") ] with Ok s -> not s.subscribe && s.tags = [] | Error _ -> false)
  && (match parse_assoc signup_codec [ ("age", "30"); ("subscribe", "on") ] with Error errs -> field_errors f_email errs <> [] | Ok _ -> false)

let%test "read: a non-numeric int is a per-field coercion error; refinements fire" =
  (match parse_assoc signup_codec [ ("email", "a@b.com"); ("age", "abc"); ("subscribe", "on") ] with
   | Error errs -> field_errors f_age errs = [ "must be a number" ]
   | Ok _ -> false)
  && (match parse_assoc signup_codec [ ("email", "nope"); ("age", "10"); ("subscribe", "on") ] with
     | Error errs -> field_errors f_email errs <> [] && field_errors f_age errs <> []
     | Ok _ -> false)

let%test "read ~inject overrides a client-submitted value (mass-assignment safety)" =
  let conn =
    Conn.make (H.make_request ~meth:H.POST ~path:"/x" ~headers:[ ("content-type", "application/x-www-form-urlencoded") ] ~body:"email=a@b.com&age=30&subscribe=on&age=999" ())
  in
  match read signup_codec conn ~inject:[ ("age", "21") ] with Ok s -> s.age = 21 | Error _ -> false

let%test "summary groups errors by field and separates form-level ones" =
  let s = summary [ Codec.err [] "form bad"; Codec.err [ "email" ] "is required"; Codec.err [ "email" ] "is invalid"; Codec.err [ "age" ] "too small" ] in
  s.form_errors = [ "form bad" ] && s.field_errors = [ ("email", [ "is required"; "is invalid" ]); ("age", [ "too small" ]) ]

type addr = { city : string; zip : string }
type person = { who : string; home : addr }

let addr_codec = Codec.(seal (record (fun city zip -> { city; zip }) |> field (req "city" (non_empty string)) (fun a -> a.city) |> field (req "zip" string) (fun a -> a.zip)))
let person_codec = Codec.(seal (record (fun who home -> { who; home }) |> field (req "who" (non_empty string)) (fun p -> p.who) |> field (req "home" addr_codec) (fun p -> p.home)))

let%test "read: dotted keys populate an embedded record; a missing nested field errors at its path" =
  (match parse_assoc person_codec [ ("who", "Ada"); ("home.city", "London"); ("home.zip", "NW1") ] with
   | Ok p -> p.who = "Ada" && p.home.city = "London" && p.home.zip = "NW1"
   | Error _ -> false)
  && match parse_assoc person_codec [ ("who", "Ada"); ("home.zip", "NW1") ] with
     | Error errs -> List.exists (fun (e : Codec.error) -> e.path = [ "home"; "city" ]) errs
     | Ok _ -> false

(* ── form-handler view-state + outcome ── *)

let has hay needle =
  let nh = String.length hay and nn = String.length needle in
  let rec go i = if i + nn > nh then false else if String.sub hay i nn = needle then true else go (i + 1) in
  nn = 0 || go 0

let%test "flash renders a banner when set, empty when not; error keys by field name" =
  let c0 = Conn.make (H.make_request ~meth:H.GET ~path:"/" ()) in
  let fa = ctx ~conn:c0 ~flash:(Some "saved!") ~errors:[] in
  let fe = ctx ~conn:c0 ~flash:None ~errors:[ Codec.err [ "name" ] "must not be empty" ] in
  has (Fur.to_html (flash fa)) "saved!"
  && Fur.to_html (flash fe) = ""
  && has (Fur.to_html (error fe "name")) "must not be empty"
  && Fur.to_html (error fe "other") = ""

let%test "again/redirect/page carry their payload at the right path" =
  (match again [ ("name", "taken") ] with Again [ e ] -> e.Codec.path = [ "name" ] && e.Codec.msg = "taken" | _ -> false)
  && (match redirect "/x" ~flash:"hi" with Redirect ("/x", Some "hi") -> true | _ -> false)
  && (match page (Fur.h "p" [] [ Fur.text "ok" ]) with Page _ -> true | _ -> false)

let%test "value repopulates an input from the submitted body (awaiting corrections)" =
  let c =
    Conn.make
      (H.make_request ~meth:H.POST ~path:"/x" ~headers:[ ("content-type", "application/x-www-form-urlencoded") ]
         ~body:"name=Ada" ())
  in
  value (ctx ~conn:c ~flash:None ~errors:[]) "name" = "Ada"
