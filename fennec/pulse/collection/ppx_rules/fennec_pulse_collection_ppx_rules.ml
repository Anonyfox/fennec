(* The fennec.pulse.collection ppx RULES as a plain library (NOT a ppx_rewriter): both the
   standalone fennec.pulse.collection.ppx AND fur.ppx import this and register the same rules in
   their own driver, so downstream pays ONE ppx process. Exposes [deriver] (global) + [rules].

   [@@fennec.collection "name"] — generates, for a plain record type t:
     module Fields = struct let <f> = Codec.req/opt/opt_list/doc_id … end
     let codec      = Codec.(seal (record (fun … -> {…}) |> field Fields.f (fun x -> x.f) |> …))
     let collection = Def.v "name" codec
   Conventions (annotation-free common case): a field named id/_id is the _id (doc_id); a TRAILING
   UNDERSCORE is an OCaml keyword escape, stripped for the wire key (done_ -> "done"); 'a option ->
   opt (absent/null = None, None omits the key); 'a list -> opt_list (absent = []). Deviations:
   [@key "wire"] overrides the wire key; [@check fn] wraps the field codec (stackable). The output
   is exactly the hand-written builder form — this ppx owns NO semantics. *)

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
    | `Int -> [%expr [%e if which = `Min then [%expr Codec.min_i] else [%expr Codec.max_i]] [%e e]]
    | _ -> [%expr [%e if which = `Min then [%expr Codec.min_f] else [%expr Codec.max_f]] [%e e]]
  in
  List.concat
    [ opt a_trim (fun () c -> [%expr Codec.trim [%e c]]);
      opt a_lowercase (fun () c -> [%expr Codec.lowercase [%e c]]);
      opt a_non_empty (fun () c -> [%expr Codec.non_empty [%e c]]);
      opt a_min_len (fun n c -> [%expr Codec.min_len [%e Ast_builder.Default.eint ~loc n] [%e c]]);
      opt a_max_len (fun n c -> [%expr Codec.max_len [%e Ast_builder.Default.eint ~loc n] [%e c]]);
      opt a_email (fun () c -> [%expr Codec.email [%e c]]);
      opt a_url (fun () c -> [%expr Codec.url [%e c]]);
      opt a_slug (fun () c -> [%expr Codec.slug [%e c]]);
      opt a_one_of (fun e c -> [%expr Codec.one_of [%e e] [%e c]]);
      opt a_matches (fun e c -> [%expr Codec.pattern [%e e] [%e c]]);
      opt a_positive (fun () c -> match kind with `Int -> [%expr Codec.positive_i [%e c]] | _ -> [%expr Codec.positive [%e c]]);
      opt a_min (fun e c -> [%expr [%e num_bound `Min e] [%e c]]);
      opt a_max (fun e c -> [%expr [%e num_bound `Max e] [%e c]]);
      opt check_attr (fun e c -> [%expr Codec.check [%e e] [%e c]]) ]

(* the base codec expression for a core type (primitives + list/option of primitives) *)
let rec base_codec ~loc (ct : core_type) : expression option =
  match ct.ptyp_desc with
  | Ptyp_constr ({ txt = Lident "string"; _ }, []) -> Some [%expr Codec.string]
  | Ptyp_constr ({ txt = Lident "int"; _ }, []) -> Some [%expr Codec.int]
  | Ptyp_constr ({ txt = Lident "float"; _ }, []) -> Some [%expr Codec.float]
  | Ptyp_constr ({ txt = Lident "bool"; _ }, []) -> Some [%expr Codec.bool]
  | Ptyp_constr ({ txt = Lident "int64"; _ }, []) -> Some [%expr Codec.date]
  | Ptyp_constr ({ txt = Ldot (Lident "Bson", "t"); _ }, []) -> Some [%expr Codec.bson]
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
  if (fname = "id" || fname = "_id") && not (is_list || is_option) then [%expr Codec.doc_id]
  else
    let w = Ast_builder.Default.estring ~loc wire in
    if is_list then [%expr Codec.opt_list [%e w] [%e base]]
    else if is_option then [%expr Codec.opt [%e w] [%e base]]
    else [%expr Codec.req [%e w] [%e base]]

let expand ~ctxt (_rec : rec_flag) (tds : type_declaration list) (cname : string) : structure =
  let loc = Expansion_context.Deriver.derived_item_loc ctxt in
  match tds with
  | [ { ptype_kind = Ptype_record labels; ptype_name; _ } ] when ptype_name.txt = "t" ->
      let module B = Ast_builder.Default in
      (* module Fields = struct
           let f = <spec> …                         (* one handle per field *)
           module <Cap g> = M.Fields …              (* per EMBEDDED field g : M.t — path navigation *)
         end
         The re-export submodule is named after the FIELD (capitalised), not the type, so two fields
         of the same embedded type get distinct paths (e.g. [author] / [reviewer] both [Person.t]). *)
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
            [%expr [%e acc] |> Codec.field [%e f] [%e get]])
          [%expr Codec.record [%e make]] labels
      in
      let codec = [%stri let codec = Codec.seal [%e builder]] in
      let coll = [%stri let collection = Def.v [%e B.estring ~loc cname] codec] in
      [ fields_mod; codec; coll ]
  | _ ->
      Location.raise_errorf ~loc "fennec.collection: expects a single record type named t"

(* the deriver registers GLOBALLY on module load (forced when fur.ppx / the standalone references
   this module), so [@@deriving fennec_collection] works in whichever single driver links us *)
let deriver =
  Deriving.add "collection"
    ~str_type_decl:
      (Deriving.Generator.V2.make
         Deriving.Args.(empty +> arg "name" (Ast_pattern.estring __))
         (fun ~ctxt (rf, tds) name ->
           match name with
           | Some n -> expand ~ctxt rf tds n
           | None ->
               Location.raise_errorf
                 ~loc:(Expansion_context.Deriver.derived_item_loc ctxt)
                 "fennec.collection: the collection name is required — [@@deriving collection ~name:\"tasks\"]"))


(* ---- the [%fields a; b; …] projection extension ---------------------------------------------
   Expands, under the model's scope (so [Fields] resolves), to a Proj.t whose decoder builds an
   OBJECT from the model's field handles — existence + type pulled from the handles (a non-model
   field is an unbound [Fields.x] error right here). Meteor's [{ a: 1, b: 1 }].

   - [a; b]                top-level inclusion → [< a : _; b : _ >]
   - [slice tags 3]        $slice on an array field (list type unchanged)
   - [author / name]       DOTTED path into an embedded record → nested object
                           [< author : < name : _ > >]; the wire ships "author.name", navigation
                           goes through the embedded model's re-exported [Fields] submodule. *)

(* one projected entry: the field PATH (OCaml idents) + the leaf wire VALUE (1 or {$slice: …}) *)
let rec specs_of (e : expression) : (string list * expression) list =
  let loc = e.pexp_loc in
  let rec path_of e =
    match e.pexp_desc with
    | Pexp_ident { txt = Lident n; _ } -> [ n ]
    | Pexp_apply ({ pexp_desc = Pexp_ident { txt = Lident "/"; _ }; _ }, [ (_, l); (_, r) ]) ->
        path_of l @ path_of r
    | _ -> Location.raise_errorf ~loc:e.pexp_loc "%%fields: a path is field names joined by / (e.g. author / name)"
  in
  match e.pexp_desc with
  | Pexp_sequence (a, b) -> specs_of a @ specs_of b
  | Pexp_construct ({ txt = Lident "()"; _ }, None) -> []
  | Pexp_apply ({ pexp_desc = Pexp_ident { txt = Lident "slice"; _ }; _ }, args) -> (
      match List.map snd args with
      | [ { pexp_desc = Pexp_ident { txt = Lident n; _ }; _ }; count ] ->
          [ ([ n ], [%expr Bson.doc [ ("$slice", Bson.Int [%e count]) ]]) ]
      | [ { pexp_desc = Pexp_ident { txt = Lident n; _ }; _ }; skip; count ] ->
          [ ([ n ], [%expr Bson.doc [ ("$slice", Bson.array [ Bson.Int [%e skip]; Bson.Int [%e count] ]) ]]) ]
      | _ -> Location.raise_errorf ~loc "%%fields: slice expects `slice field n` or `slice field skip n`")
  | Pexp_apply ({ pexp_desc = Pexp_ident { txt = Lident "/"; _ }; _ }, _) -> [ (path_of e, [%expr Bson.Int 1]) ]
  | Pexp_ident { txt = Lident n; _ } -> [ ([ n ], [%expr Bson.Int 1]) ]
  | _ -> Location.raise_errorf ~loc "%%fields: expected field names, `a / b` paths, or `slice field n`"

let fields_expander =
  Extension.declare "fields" Extension.Context.expression
    Ast_pattern.(single_expr_payload __)
    (fun ~loc ~path:_ payload ->
      let module B = Ast_builder.Default in
      let specs = specs_of payload in
      if specs = [] then Location.raise_errorf ~loc "%%fields: at least one field is required";
      (* the Fields module path for a prefix of segments: Fields, Fields.<Cap s0>, … *)
      let mod_of prefix =
        List.fold_left (fun acc s -> Ldot (acc, String.capitalize_ascii s)) (Lident "Fields") prefix
      in
      let handle prefix seg = B.pexp_ident ~loc { txt = Ldot (mod_of prefix, seg); loc } in
      (* the dotted wire key for a path: field_name of each segment's handle, joined by "." *)
      let rec wire_key acc_prefix = function
        | [] -> [%expr ""]
        | [ seg ] -> [%expr Codec.field_name [%e handle acc_prefix seg]]
        | seg :: rest ->
            [%expr Codec.field_name [%e handle acc_prefix seg] ^ "." ^ [%e wire_key (acc_prefix @ [ seg ]) rest]]
      in
      let includes = List.map (fun (p, v) -> [%expr ([%e wire_key [] p], [%e v])]) specs in
      (* _id is shipped by default; suppress it unless a TOP-LEVEL field maps to wire "_id" *)
      let any_is_id =
        List.fold_right
          (fun (p, _) acc -> match p with [ seg ] -> [%expr Codec.field_name [%e handle [] seg] = "_id" || [%e acc]] | _ -> acc)
          specs [%expr false]
      in
      let fields_list =
        [%expr
          let incs = [%e B.elist ~loc includes] in
          if [%e any_is_id] then incs else ("_id", Bson.Int 0) :: incs]
      in
      (* the result object + recursive decode: group paths by head; a leaf head decodes its handle,
         a branch head extracts its subdoc and recurses through the embedded Fields submodule *)
      let paths = List.map fst specs in
      let rec build (prefix : string list) (doc : expression) (depth : int) (items : string list list) : expression =
        (* ordered-unique heads *)
        let heads =
          List.fold_left (fun acc p -> match p with h :: _ when not (List.mem h acc) -> acc @ [ h ] | _ -> acc) [] items
        in
        let classify h =
          let leaf = List.exists (function [ x ] -> x = h | _ -> false) items in
          let tails = List.filter_map (function x :: (_ :: _ as t) when x = h -> Some t | _ -> None) items in
          if leaf && tails <> [] then
            Location.raise_errorf ~loc "%%fields: %s is taken both whole and by sub-path" h;
          if leaf then `Leaf else `Branch tails
        in
        let var h = "__p" ^ string_of_int depth ^ "_" ^ h in
        let obj =
          B.pexp_object ~loc
            (B.class_structure ~self:(B.ppat_any ~loc)
               ~fields:(List.map (fun h -> B.pcf_method ~loc ({ txt = h; loc }, Public, Cfk_concrete (Fresh, B.evar ~loc (var h)))) heads))
        in
        List.fold_right
          (fun h acc ->
            let decode_h =
              match classify h with
              | `Leaf -> [%expr Codec.field_get [%e handle prefix h] [%e doc]]
              | `Branch tails ->
                  let sub = "__sub" ^ string_of_int depth ^ "_" ^ h in
                  [%expr
                    match Bson.get [%e doc] (Codec.field_name [%e handle prefix h]) with
                    | None -> Error [ { Codec.path = [ [%e B.estring ~loc h] ]; msg = "missing field" } ]
                    | Some [%p B.pvar ~loc sub] ->
                        [%e build (prefix @ [ h ]) (B.evar ~loc sub) (depth + 1) tails]]
            in
            [%expr match [%e decode_h] with Error __e -> Error __e | Ok [%p B.pvar ~loc (var h)] -> [%e acc]])
          heads [%expr Ok [%e obj]]
      in
      let decode_body = build [] [%expr __d] 0 paths
      in
      [%expr Proj.v ~fields:[%e fields_list] ~decode:(fun __d -> [%e decode_body])])

(* ---- [%q …] / [%sort …] / [%set …] — write queries as expressions, not API calls -------------
   Resolved against the model's [Fields] in scope (open Task, or Task.(…)). A bare ident is a field
   (Fields.x); record-access [a.b] is a dotted path (Codec.dot, navigating the embedded Fields).
   Everything expands to the typed Q/Sort/M calls, so a wrong field/value is still a compile error —
   only the noise is gone. Richer matchers/modifiers (regex/has/inc/push) use Q/M directly. *)

(* a field path expression (bare ident or a.b.c record-access) → the handle, dotted via Codec.dot *)
let segs_of_field e =
  let rec go e =
    match e.pexp_desc with
    | Pexp_ident { txt = Lident n; _ } -> [ n ]
    | Pexp_field (inner, { txt = Lident n; _ }) -> go inner @ [ n ]
    | _ -> Location.raise_errorf ~loc:e.pexp_loc "expected a field name or a.b path"
  in
  go e

let field_handle ~loc segs =
  let module B = Ast_builder.Default in
  let rec chain modp = function
    | [ s ] -> B.pexp_ident ~loc { txt = Ldot (modp, s); loc }
    | s :: rest -> [%expr Codec.dot [%e B.pexp_ident ~loc { txt = Ldot (modp, s); loc }] [%e chain (Ldot (modp, String.capitalize_ascii s)) rest]]
    | [] -> assert false
  in
  chain (Lident "Fields") segs

let q_expander =
  Extension.declare "q" Extension.Context.expression Ast_pattern.(single_expr_payload __)
    (fun ~loc ~path:_ e ->
      let fld e = field_handle ~loc (segs_of_field e) in
      let cmp = function
        | "=" -> Some [%expr Q.eq] | "<>" -> Some [%expr Q.ne] | "<" -> Some [%expr Q.lt]
        | "<=" -> Some [%expr Q.lte] | ">" -> Some [%expr Q.gt] | ">=" -> Some [%expr Q.gte] | _ -> None
      in
      let rec q_of e =
        match e.pexp_desc with
        | Pexp_construct ({ txt = Lident "true"; _ }, None) -> [%expr Q.all []]
        | Pexp_apply ({ pexp_desc = Pexp_ident { txt = Lident "&&"; _ }; _ }, [ (_, l); (_, r) ]) ->
            [%expr Q.all [ [%e q_of l]; [%e q_of r] ]]
        | Pexp_apply ({ pexp_desc = Pexp_ident { txt = Lident "||"; _ }; _ }, [ (_, l); (_, r) ]) ->
            [%expr Q.any [ [%e q_of l]; [%e q_of r] ]]
        | Pexp_apply ({ pexp_desc = Pexp_ident { txt = Lident op; _ }; _ }, [ (_, l); (_, r) ]) when cmp op <> None ->
            [%expr [%e Option.get (cmp op)] [%e fld l] [%e r]]
        | _ ->
            Location.raise_errorf ~loc:e.pexp_loc
              "%%q: use comparisons (= <> < <= > >=) and && / ||; for in/has/regex use Q directly"
      in
      [%expr [ [%e q_of e] ]])

let sort_expander =
  Extension.declare "sort" Extension.Context.expression Ast_pattern.(single_expr_payload __)
    (fun ~loc ~path:_ e ->
      let key e =
        match e.pexp_desc with
        | Pexp_apply (f, [ (_, { pexp_desc = Pexp_ident { txt = Lident dir; _ }; _ }) ]) ->
            let h = field_handle ~loc (segs_of_field f) in
            (match dir with
             | "asc" -> [%expr Sort.asc [%e h]] | "desc" -> [%expr Sort.desc [%e h]]
             | _ -> Location.raise_errorf ~loc:e.pexp_loc "%%sort: direction must be asc or desc")
        | _ -> [%expr Sort.asc [%e field_handle ~loc (segs_of_field e)]]
      in
      let keys = match e.pexp_desc with Pexp_tuple es -> List.map key es | _ -> [ key e ] in
      [%expr Sort.by [%e Ast_builder.Default.elist ~loc keys]])

let set_expander =
  Extension.declare "set" Extension.Context.expression Ast_pattern.(single_expr_payload __)
    (fun ~loc ~path:_ e ->
      let one e =
        match e.pexp_desc with
        | Pexp_apply ({ pexp_desc = Pexp_ident { txt = Lident "="; _ }; _ }, [ (_, l); (_, r) ]) ->
            [%expr M.set [%e field_handle ~loc (segs_of_field l)] [%e r]]
        | _ -> Location.raise_errorf ~loc:e.pexp_loc "%%set: each clause is `field = value`; for inc/push/… use M"
      in
      let rec assigns e = match e.pexp_desc with Pexp_sequence (a, b) -> assigns a @ assigns b | _ -> [ one e ] in
      [%expr M.all [%e Ast_builder.Default.elist ~loc (assigns e)]])

(* exposed for composition into a SINGLE driver: fur.ppx (mlx components) and the thin standalone
   both fold these into their own [register_transformation ~rules], so a file pays ONE ppx process
   for mlx + tests + the collection deriver + projections + query/sort/set DSLs. *)
let rules =
  List.map Context_free.Rule.extension [ fields_expander; q_expander; sort_expander; set_expander ]
