(* The CLIENT form primitive — a reactive form bound to a {!Sift} codec, so a browser SPA form gets
   the SAME refinement validation the server enforces ([@trim] [@non_empty] [@max_len] [@email] …),
   recomputed reactively, offline, with no round-trip. The draft lives in ONE Fur signal; the errors
   are a derived {!Fur.memo} recomputed via {!Sift.validate} on every change. Per-field feedback is keyed
   by the TYPED field handle (the model's generated [Fields]) — a renamed field is a compile error, the
   same discipline as {!Filter}/[%q]. Pure (Fur signals + Sift), so it runs native (tests/SSR) and in the
   js_of_ocaml bundle unchanged.

   It UNIFIES the three form stories on one codec + one {!Sift.error} shape: the server [Form.read]
   (decode + validate of a POST) and this client [Form.signal] both run the SAME [Sift.error] codes/paths,
   because they validate the SAME ['a Sift.t]. The HTML5 constraint attributes ({!input_attrs}) come off
   that same codec too — so the browser, the client form, and the server all enforce one declaration. *)

(* a reactive form: the typed draft in a signal, plus a memoized error list (re-run on draft change).
   [errors_memo] reads [draft] inside {!Fur.memo}, so it recomputes lazily once per change and is
   glitch-free; reading it subscribes the caller. *)
type 'a t = { codec : 'a Sift.t; draft : 'a Fur.signal; errors_memo : Sift.error list Fur.signal }

(* [signal codec init] — a fresh reactive form over [codec], seeded with [init] (use [Sift.default codec]
   for an empty form). The error memo validates [init] up front, then on every {!set}/{!update}. *)
let signal (codec : 'a Sift.t) (init : 'a) : 'a t =
  let draft = Fur.signal init in
  let errors_memo =
    Fur.memo (fun () -> match Sift.validate codec (Fur.get draft) with Ok () -> [] | Error es -> es)
  in
  { codec; draft; errors_memo }

(* reactive read of the current draft (subscribes the caller — re-renders on every edit) *)
let value (f : 'a t) : 'a = Fur.get f.draft

(* read the draft WITHOUT subscribing — for an event handler that must not re-run on its own write *)
let peek (f : 'a t) : 'a = Fur.peek f.draft

(* replace / transform the whole draft (the error memo recomputes on the next read) *)
let set (f : 'a t) (v : 'a) : unit = Fur.set f.draft v
let update (f : 'a t) (g : 'a -> 'a) : unit = Fur.update f.draft g

(* the live validation errors of the current draft — reactive; [[]] when valid. SAME [Sift.error]
   list (codes/paths) the server's [Form.read] produces for the same codec. *)
let errors (f : 'a t) : Sift.error list = Fur.get f.errors_memo

(* reactive: the draft satisfies every refinement (no errors) — drive a submit button's [disabled] *)
let valid (f : 'a t) : bool = errors f = []

(* the first error message for ONE field, by its typed handle ([Model.Fields.x]) — reactive inline
   feedback. Matches on the error PATH HEAD (the field's wire name), exactly like the server
   [Form.field_errors]/[Form.error], so client and server surface the same per-field message. *)
let field_error (f : 'a t) (fld : _ Sift.field) : string option =
  let n = Sift.field_name fld in
  List.find_map (fun (e : Sift.error) -> match e.Sift.path with x :: _ when x = n -> Some e.Sift.msg | _ -> None) (errors f)

(* all error messages for ONE field (the field can violate several refinements at once) — reactive *)
let field_errors (f : 'a t) (fld : _ Sift.field) : string list =
  let n = Sift.field_name fld in
  List.filter_map (fun (e : Sift.error) -> match e.Sift.path with x :: _ when x = n -> Some e.Sift.msg | _ -> None) (errors f)

(* the submit gate: the typed value when the draft is valid, else its errors. A NON-reactive peek (call
   it from an onClick), so it never subscribes the handler. The [Ok] value is exactly what a DDP method
   / [Sift.encode_json] would send — the same boundary the server re-validates. *)
let submit (f : 'a t) : ('a, Sift.error list) result = Sift.validate f.codec (Fur.peek f.draft) |> Result.map (fun () -> Fur.peek f.draft)

(* ─────────────────── codec-driven HTML5 constraint attributes (D2) ───────────────────
   Walk a field's {!Sift.field_view} + {!Sift.field_required} (the same reflection [json_schema.ml]
   renders to $jsonSchema) and emit the browser's native form attributes — [required] / [maxlength] /
   [minlength] / [pattern] / [type=email|url|number|…] — so the constraints come off the ONE codec, with
   zero hand-duplication. The browser then enforces them client-side before any JS runs. *)

(* the exact patterns {!Sift.email}/{!Sift.url} stamp (combinators.ml) — recognised so they map to the
   native [type=email]/[type=url] inputs (which enforce the format) instead of a raw [pattern] attr. *)
let email_pat = "^[a-zA-Z0-9._%+-]+@[a-zA-Z0-9.-]+\\.[a-zA-Z][a-zA-Z]+$"
let url_pat = "^https?://[^ ]+$"

(* the leaf input type, read off the structural view (refinements ride as hints): a numeric leaf is a
   number input, a date a date input, the email/url pattern their semantic inputs, everything else text. *)
let rec input_type (v : Sift.view) : string =
  match v with
  | Sift.V_int | Sift.V_float -> "number"
  | Sift.V_date -> "date"
  | Sift.V_check (Sift.H_pattern p, _) when p = email_pat -> "email"
  | Sift.V_check (Sift.H_pattern p, _) when p = url_pat -> "url"
  | Sift.V_check (_, inner) -> input_type inner
  | Sift.V_option inner -> input_type inner
  | _ -> "text"

(* render a bound float as HTML wants it: an integral bound ([min_i 18] -> 18.0) emits "18", not "18." *)
let num x = if Float.is_integer x then string_of_int (int_of_float x) else string_of_float x

(* collect the constraint attrs from the refinement hints stacked on the view. The email/url patterns
   are skipped (their [type=] already enforces the format); any OTHER [pattern] (slug, custom) is emitted. *)
let rec constraints (v : Sift.view) (acc : Fur.attr list) : Fur.attr list =
  match v with
  | Sift.V_check (h, inner) ->
      let acc =
        match h with
        | Sift.H_max_len n -> Fur.attr "maxlength" (string_of_int n) :: acc
        | Sift.H_min_len n -> Fur.attr "minlength" (string_of_int n) :: acc
        | Sift.H_pattern p when p <> email_pat && p <> url_pat -> Fur.attr "pattern" p :: acc
        | Sift.H_min x -> Fur.attr "min" (num x) :: acc
        | Sift.H_max x -> Fur.attr "max" (num x) :: acc
        | _ -> acc
      in
      constraints inner acc
  | Sift.V_option inner -> constraints inner acc
  | _ -> acc

(* [field_attrs view required] — the pure core: the HTML5 attrs for a field of structural [view] that
   is [required] or not. The leaf kind picks [type=]; the hints add [required]/[maxlength]/[pattern]/… *)
let field_attrs (view : Sift.view) (required : bool) : Fur.attr list =
  let ty = input_type view in
  let base = if ty = "text" then [] else [ Fur.attr "type" ty ] in
  (* a required field gets the boolean attribute (Fur renders k="v"; [required="required"] is valid HTML5) *)
  let base = if required then Fur.attr "required" "required" :: base else base in
  constraints view base

(* [input_attrs field] — the HTML5 constraint attrs for ONE typed field handle ([Model.Fields.x]):
   drop them straight onto an [<input>] so the codec's [@non_empty]/[@max_len]/[@email]/… become the
   browser's [required]/[maxlength]/[type=email]/… No name= (the caller writes that). *)
let input_attrs (fld : _ Sift.field) : Fur.attr list = field_attrs (Sift.field_view fld) (Sift.field_required fld)

(* ──────────────────────────── tests ──────────────────────────── *)

(* a model shaped like a real signup form — exercised by BOTH the reactive form and the attr builder *)
type signup = { name : string; email : string; age : int; bio : string }

let f_name = Sift.(req "name" (trim (non_empty (max_len 40 string))))
let f_email = Sift.(req "email" (email string))
let f_age = Sift.(req "age" (min_i 18 (max_i 120 int)))
let f_bio = Sift.(opt "bio" (max_len 200 string))

(* a hand-written codec here (the lib has no ppx) — [@@deriving model] emits exactly this Fields+codec *)
let codec =
  Sift.(
    seal
      (record (fun name email age bio -> { name; email; age; bio = Option.value bio ~default:"" })
      |> field f_name (fun t -> t.name)
      |> field f_email (fun t -> t.email)
      |> field f_age (fun t -> t.age)
      |> field f_bio (fun t -> if t.bio = "" then None else Some t.bio)))

let empty = { name = ""; email = ""; age = 0; bio = "" }

let%test "signal: a valid draft has no errors and submit yields the typed value" =
  let f = signal codec { name = "Ada"; email = "ada@x.io"; age = 30; bio = "" } in
  valid f && errors f = [] && (match submit f with Ok v -> v.name = "Ada" && v.age = 30 | Error _ -> false)

let%test "signal: an empty draft is invalid; field_error reports per typed handle, NO network" =
  let f = signal codec empty in
  (not (valid f))
  (* name is empty -> non_empty fires; age 0 -> below min_i 18 *)
  && field_error f f_name <> None
  && field_error f f_age <> None
  (* email empty is also invalid; bio is optional+empty -> no error *)
  && field_error f f_email <> None
  && field_error f f_bio = None
  && (match submit f with Error _ -> true | Ok _ -> false)

let%test "signal: an edit re-validates reactively (set/update flips validity, clears the field error)" =
  let f = signal codec empty in
  let bad = not (valid f) in
  set f { name = "Grace"; email = "grace@x.io"; age = 45; bio = "" };
  let good = valid f && field_error f f_name = None in
  update f (fun d -> { d with name = "" }); (* clear name again *)
  let bad_again = (not (valid f)) && field_error f f_name <> None in
  bad && good && bad_again

let%test "signal: too-long name trips max_len with the SAME code the server decode produces" =
  let long = String.make 50 'x' in
  let f = signal codec { empty with name = long; email = "a@b.io"; age = 20 } in
  (* the live client error for this field carries code "max_len" — identical to a server-side decode *)
  let client_code =
    List.find_map (fun (e : Sift.error) -> if e.Sift.path = [ "name" ] then Some e.Sift.code else None) (errors f)
  in
  let server_code =
    match Sift.decode codec (Sift.to_bson codec { empty with name = long; email = "a@b.io"; age = 20 }) with
    | Error es -> List.find_map (fun (e : Sift.error) -> if e.Sift.path = [ "name" ] then Some e.Sift.code else None) es
    | Ok _ -> None
  in
  client_code = Some "max_len" && client_code = server_code

let%test "field_attrs: non_empty + max_len string field -> required + maxlength + text" =
  let attrs = input_attrs f_name in
  let has k v = List.exists (fun a -> a = Fur.attr k v) attrs in
  has "required" "required" && has "maxlength" "40" && not (List.exists (fun a -> a = Fur.attr "type" "text") attrs)

let%test "field_attrs: email field -> type=email + required, and NO redundant pattern attr" =
  let attrs = input_attrs f_email in
  let has k v = List.exists (fun a -> a = Fur.attr k v) attrs in
  has "type" "email" && has "required" "required"
  (* the email hint becomes type=email; we don't ALSO dump the raw regex as a pattern *)
  && not (List.exists (fun a -> a = Fur.attr "pattern" email_pat) attrs)

let%test "field_attrs: numeric field -> type=number with min/max bounds" =
  let attrs = input_attrs f_age in
  let has k v = List.exists (fun a -> a = Fur.attr k v) attrs in
  has "type" "number" && has "min" "18" && has "max" "120" && has "required" "required"

let%test "field_attrs: an OPTIONAL field is not marked required" =
  let attrs = input_attrs f_bio in
  (not (List.exists (fun a -> a = Fur.attr "required" "required") attrs))
  && List.exists (fun a -> a = Fur.attr "maxlength" "200") attrs
