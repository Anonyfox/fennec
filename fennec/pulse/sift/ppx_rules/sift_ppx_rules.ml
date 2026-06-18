(* Sift's REUSABLE ppx rules as a plain library (NOT a ppx_rewriter) — the neutral codec/Fields
   deriver, shippable with the Sift package and composable into ANY ppx pipeline (a downstream user
   links this into their single driver; fennec links it into fur.ppx alongside its own rules, so a
   file still pays ONE ppx process). Owns NO semantics beyond "the hand-written builder form".

   [@@deriving sift] (alias [@@deriving model]) — generates, for a plain record type t:
     module Fields = struct let <f> = Sift.req/opt/opt_list/doc_id … end
     let codec      = Sift.(seal (record (fun … -> {…}) |> field Fields.f (fun x -> x.f) |> …))
   Conventions (annotation-free common case): a field named id/_id is the _id (doc_id); a TRAILING
   UNDERSCORE is an OCaml keyword escape, stripped for the wire key (done_ -> "done"); 'a option ->
   opt (absent/null = None, None omits the key); 'a list -> opt_list (absent = []). Deviations:
   [@key "wire"] overrides the wire key; [@check fn] wraps the field codec (stackable). Plus the inline
   validation attributes ([@non_empty] [@max_len 200] [@one_of …] …). The exported {!model_core} is
   what fennec's [collection] deriver reuses before adding its Mongo/store tail. *)

open Ppxlib

let strip_keyword_escape s =
  if String.length s > 1 && s.[String.length s - 1] = '_' then String.sub s 0 (String.length s - 1) else s

let key_attr =
  Attribute.declare "key" Attribute.Context.label_declaration
    Ast_pattern.(single_expr_payload (estring __))
    (fun s -> s)

let check_attr =
  Attribute.declare "check" Attribute.Context.label_declaration
    Ast_pattern.(single_expr_payload __)
    (fun e -> e)

(* ---- validation attributes: the catalog inline on the record, so there is ONE model form -------
   Each present attribute wraps the field's base codec with its combinator. No-payload flags
   ([@non_empty] [@email] …), int payloads ([@max_len 140]), and expression payloads ([@one_of …]
   [@min 1]). Numeric bounds pick min_i/min_f by the field's base type. *)
let lc = Attribute.Context.label_declaration

let flag name = Attribute.declare name lc Ast_pattern.(pstr nil) ()
let inta name = Attribute.declare name lc Ast_pattern.(single_expr_payload (eint __)) (fun n -> n)
let expra name = Attribute.declare name lc Ast_pattern.(single_expr_payload __) (fun e -> e)

let a_non_empty = flag "non_empty"
let a_email = flag "email"
let a_url = flag "url"
let a_slug = flag "slug"
let a_trim = flag "trim"
let a_lowercase = flag "lowercase"
let a_positive = flag "positive"
let a_min_len = inta "min_len"
let a_max_len = inta "max_len"
let a_one_of = expra "one_of"
let a_matches = expra "matches"
let a_min = expra "min"
let a_max = expra "max"

(* unwrap list/option to the leaf scalar kind, for numeric-bound combinator choice *)
let rec base_kind (ct : core_type) =
  match ct.ptyp_desc with
  | Ptyp_constr ({ txt = Lident "string"; _ }, []) -> `Str
  | Ptyp_constr ({ txt = Lident "int"; _ }, []) -> `Int
  | Ptyp_constr ({ txt = Lident "float"; _ }, []) -> `Float
  | Ptyp_constr ({ txt = Lident ("list" | "option"); _ }, [ el ]) -> base_kind el
  | _ -> `Other

(* the ordered wrapper chain a field's validation attributes contribute (innermost first) *)
let validators ~loc (ld : label_declaration) : (expression -> expression) list =
  let kind = base_kind ld.pld_type in
  let opt a f = match Attribute.get a ld with Some v -> [ f v ] | None -> [] in
  let num_bound which e =
    match kind with
    | `Int -> [%expr [%e if which = `Min then [%expr Sift.min_i] else [%expr Sift.max_i]] [%e e]]
    | _ -> [%expr [%e if which = `Min then [%expr Sift.min_f] else [%expr Sift.max_f]] [%e e]]
  in
  List.concat
    [ opt a_trim (fun () c -> [%expr Sift.trim [%e c]]);
      opt a_lowercase (fun () c -> [%expr Sift.lowercase [%e c]]);
      opt a_non_empty (fun () c -> [%expr Sift.non_empty [%e c]]);
      opt a_min_len (fun n c -> [%expr Sift.min_len [%e Ast_builder.Default.eint ~loc n] [%e c]]);
      opt a_max_len (fun n c -> [%expr Sift.max_len [%e Ast_builder.Default.eint ~loc n] [%e c]]);
      opt a_email (fun () c -> [%expr Sift.email [%e c]]);
      opt a_url (fun () c -> [%expr Sift.url [%e c]]);
      opt a_slug (fun () c -> [%expr Sift.slug [%e c]]);
      opt a_one_of (fun e c -> [%expr Sift.one_of [%e e] [%e c]]);
      opt a_matches (fun e c -> [%expr Sift.pattern [%e e] [%e c]]);
      opt a_positive (fun () c -> match kind with `Int -> [%expr Sift.positive_i [%e c]] | _ -> [%expr Sift.positive [%e c]]);
      opt a_min (fun e c -> [%expr [%e num_bound `Min e] [%e c]]);
      opt a_max (fun e c -> [%expr [%e num_bound `Max e] [%e c]]);
      opt check_attr (fun e c -> [%expr Sift.check [%e e] [%e c]]) ]

(* the base codec expression for a core type (primitives + list/option of primitives) *)
let rec base_codec ~loc (ct : core_type) : expression option =
  match ct.ptyp_desc with
  | Ptyp_constr ({ txt = Lident "string"; _ }, []) -> Some [%expr Sift.string]
  | Ptyp_constr ({ txt = Lident "int"; _ }, []) -> Some [%expr Sift.int]
  | Ptyp_constr ({ txt = Lident "float"; _ }, []) -> Some [%expr Sift.float]
  | Ptyp_constr ({ txt = Lident "bool"; _ }, []) -> Some [%expr Sift.bool]
  | Ptyp_constr ({ txt = Lident "int64"; _ }, []) -> Some [%expr Sift.date]
  | Ptyp_constr ({ txt = Ldot (Lident "Bson", "t"); _ }, []) -> Some [%expr Sift.bson]
  | Ptyp_constr ({ txt = Lident ("list" | "option"); _ }, [ el ]) -> base_codec ~loc el
  (* an embedded record: a field typed [M.t] (M a derived model) → its codec is [M.codec] *)
  | Ptyp_constr ({ txt = Ldot (m, "t"); _ }, []) when m <> Lident "Bson" ->
      Some (Ast_builder.Default.pexp_ident ~loc { txt = Ldot (m, "codec"); loc })
  | _ -> None

(* the embedded model's module (for [M.t], [M.t list], [M.t option]) — drives Fields re-export
   so dotted-path projections can navigate into it *)
let rec embedded_of (ct : core_type) : longident option =
  match ct.ptyp_desc with
  | Ptyp_constr ({ txt = Ldot (m, "t"); _ }, []) when m <> Lident "Bson" -> Some m
  | Ptyp_constr ({ txt = Lident ("list" | "option"); _ }, [ el ]) -> embedded_of el
  | _ -> None

let field_spec ~loc (ld : label_declaration) : expression =
  let fname = ld.pld_name.txt in
  let wire = match Attribute.get key_attr ld with Some k -> k | None -> strip_keyword_escape fname in
  let is_list = match ld.pld_type.ptyp_desc with Ptyp_constr ({ txt = Lident "list"; _ }, _) -> true | _ -> false in
  let is_option = match ld.pld_type.ptyp_desc with Ptyp_constr ({ txt = Lident "option"; _ }, _) -> true | _ -> false in
  let base =
    match base_codec ~loc ld.pld_type with
    | Some c -> c
    | None ->
        Location.raise_errorf ~loc:ld.pld_loc
          "fennec.collection: unsupported field type for %s (primitives, list/option of those, or an \
           embedded M.t whose module M also derives fennec_collection — else use the hand-written builder)"
          fname
  in
  (* wrap the base codec with the field's validation attributes (innermost-first chain) *)
  let base = List.fold_left (fun acc wrap -> wrap acc) base (validators ~loc ld) in
  if (fname = "id" || fname = "_id") && not (is_list || is_option) then [%expr Sift.doc_id]
  else
    let w = Ast_builder.Default.estring ~loc wire in
    if is_list then [%expr Sift.opt_list [%e w] [%e base]]
    else if is_option then [%expr Sift.opt [%e w] [%e base]]
    else [%expr Sift.req [%e w] [%e base]]

(* the SHARED core both derivers emit: [module Fields = …] (typed handles + embedded re-exports) and
   [let codec = …] (the validating builder). DB-agnostic — references only [Sift]. *)
let model_core ~loc (labels : label_declaration list) : structure =
  let module B = Ast_builder.Default in
  (* module Fields = struct
       let f = <spec> …                         (* one handle per field *)
       module <Cap g> = M.Fields …              (* per EMBEDDED field g : M.t — path navigation *)
     end
     The re-export submodule is named after the FIELD (capitalised), not the type, so two fields of
     the same embedded type get distinct paths (e.g. [author] / [reviewer] both [Person.t]). *)
  let field_items =
    List.concat_map
      (fun ld ->
        let handle = [%stri let [%p B.pvar ~loc ld.pld_name.txt] = [%e field_spec ~loc ld]] in
        match embedded_of ld.pld_type with
        | None -> [ handle ]
        | Some m ->
            let sub =
              B.pstr_module ~loc
                (B.module_binding ~loc
                   ~name:{ txt = Some (String.capitalize_ascii ld.pld_name.txt); loc }
                   ~expr:(B.pmod_ident ~loc { txt = Ldot (m, "Fields"); loc }))
            in
            [ handle; sub ])
      labels
  in
  let fields_mod =
    B.pstr_module ~loc
      (B.module_binding ~loc ~name:{ txt = Some "Fields"; loc } ~expr:(B.pmod_structure ~loc field_items))
  in
  (* record (fun a b … -> { a; b; … }) *)
  let make =
    List.fold_right
      (fun ld acc -> B.pexp_fun ~loc Nolabel None (B.pvar ~loc ld.pld_name.txt) acc)
      labels
      (B.pexp_record ~loc
         (List.map (fun ld -> ({ txt = Lident ld.pld_name.txt; loc }, B.evar ~loc ld.pld_name.txt)) labels)
         None)
  in
  let builder =
    List.fold_left
      (fun acc ld ->
        let get =
          B.pexp_fun ~loc Nolabel None (B.pvar ~loc "x")
            (B.pexp_field ~loc (B.evar ~loc "x") { txt = Lident ld.pld_name.txt; loc })
        in
        let f = B.pexp_ident ~loc { txt = Ldot (Lident "Fields", ld.pld_name.txt); loc } in
        [%expr [%e acc] |> Sift.field [%e f] [%e get]])
      [%expr Sift.record [%e make]] labels
  in
  (* K4 STAGING: for arity 1..8 emit [Sift.objN Fields.f1 … ~make ~split] — a DIRECT decoder (read all
     fields then construct in one application, no applicative currying), byte-identical to the builder.
     A wider record falls back to [Sift.seal builder]. Same one ppx pass — this is just better output
     from the deriver that already runs. *)
  let n = List.length labels in
  let codec_expr =
    if n >= 1 && n <= 8 then (
      let objn = B.pexp_ident ~loc { txt = Ldot (Lident "Sift", Printf.sprintf "obj%d" n); loc } in
      let field_args = List.map (fun ld -> (Nolabel, B.pexp_ident ~loc { txt = Ldot (Lident "Fields", ld.pld_name.txt); loc })) labels in
      let proj ld = B.pexp_field ~loc (B.evar ~loc "x") { txt = Lident ld.pld_name.txt; loc } in
      let split_body = match labels with [ ld ] -> proj ld | _ -> B.pexp_tuple ~loc (List.map proj labels) in
      let split = B.pexp_fun ~loc Nolabel None (B.pvar ~loc "x") split_body in
      B.pexp_apply ~loc objn (field_args @ [ (Labelled "make", make); (Labelled "split", split) ]))
    else [%expr Sift.seal [%e builder]]
  in
  let codec = [%stri let codec = [%e codec_expr]] in
  [ fields_mod; codec ]

(* [@@deriving model]: the DB-agnostic model — just [Fields] + [codec], for forms / JSON APIs / any
   shape that isn't a Mongo collection. The collection deriver below is this plus the Mongo tail. *)
let expand_model ~ctxt (_rec : rec_flag) (tds : type_declaration list) : structure =
  let loc = Expansion_context.Deriver.derived_item_loc ctxt in
  match tds with
  | [ { ptype_kind = Ptype_record labels; ptype_name; _ } ] when ptype_name.txt = "t" -> model_core ~loc labels
  | _ -> Location.raise_errorf ~loc "fennec.model: expects a single record type named t"

(* [@@deriving model] — generates [Fields] + [codec] from a record: the DB-agnostic typed model (forms,
   JSON APIs, method args). Fennec's [collection] deriver ({!Collection_deriver}) reuses {!model_core}
   and adds the Mongo/store tail. Registered globally on module load. *)
let deriver_model =
  Deriving.add "model" ~str_type_decl:(Deriving.Generator.V2.make Deriving.Args.empty (fun ~ctxt (rf, tds) -> expand_model ~ctxt rf tds))

let () = ignore deriver_model
