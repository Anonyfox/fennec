(* [@@fennec.collection "name"] — generates, for a plain record type t:
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
          "fennec.collection: unsupported field type for %s (primitives, list/option of primitives — use the hand-written builder for richer shapes)"
          fname
  in
  let base = List.fold_left (fun acc chk -> [%expr Codec.check [%e chk] [%e acc]]) base
      (match Attribute.get check_attr ld with Some e -> [ e ] | None -> []) in
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
      (* module Fields = struct let f = <spec> … end *)
      let field_items =
        List.map (fun ld -> [%stri let [%p B.pvar ~loc ld.pld_name.txt] = [%e field_spec ~loc ld]]) labels
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

let deriver =
  Deriving.add "fennec_collection"
    ~str_type_decl:
      (Deriving.Generator.V2.make
         Deriving.Args.(empty +> arg "name" (Ast_pattern.estring __))
         (fun ~ctxt (rf, tds) name ->
           match name with
           | Some n -> expand ~ctxt rf tds n
           | None ->
               Location.raise_errorf
                 ~loc:(Expansion_context.Deriver.derived_item_loc ctxt)
                 "fennec.collection: the collection name is required — [@@deriving fennec_collection ~name:\"tasks\"]"))


(* ---- the [%fields a; b; …] projection extension ---------------------------------------------
   Expands, under the model's scope (so [Fields] resolves), to a Proj.t whose decoder builds an
   object [< a : _; b : _ >] from [Fields.a]/[Fields.b] — existence + type pulled from the handles
   (a non-model field is an unbound [Fields.x] error right here). Meteor's [{ a: 1, b: 1 }]. An
   array field can carry a $slice: [slice tags 3] keeps the first 3, [slice tags 2 5] skips 2 then
   keeps 5 — the field's list TYPE is unchanged, only the wire/array is trimmed. *)

(* a projected field spec: the OCaml field name + the wire VALUE expression (1, or {$slice: …}) *)
let rec specs_of (e : expression) : (string * expression) list =
  let loc = e.pexp_loc in
  match e.pexp_desc with
  | Pexp_ident { txt = Lident n; _ } -> [ (n, [%expr Bson.Int 1]) ]
  | Pexp_sequence (a, b) -> specs_of a @ specs_of b
  | Pexp_construct ({ txt = Lident "()"; _ }, None) -> []
  | Pexp_apply ({ pexp_desc = Pexp_ident { txt = Lident "slice"; _ }; _ }, args) -> (
      match List.map snd args with
      | [ { pexp_desc = Pexp_ident { txt = Lident n; _ }; _ }; count ] ->
          [ (n, [%expr Bson.doc [ ("$slice", Bson.Int [%e count]) ]]) ]
      | [ { pexp_desc = Pexp_ident { txt = Lident n; _ }; _ }; skip; count ] ->
          [ (n, [%expr Bson.doc [ ("$slice", Bson.array [ Bson.Int [%e skip]; Bson.Int [%e count] ]) ]]) ]
      | _ -> Location.raise_errorf ~loc "%%fields: slice expects `slice field n` or `slice field skip n`")
  | _ ->
      Location.raise_errorf ~loc
        "%%fields: expected field names (e.g. [%%fields title; done_]) or `slice field n`"

let fields_expander =
  Extension.declare "fields" Extension.Context.expression
    Ast_pattern.(single_expr_payload __)
    (fun ~loc ~path:_ payload ->
      let module B = Ast_builder.Default in
      let specs = specs_of payload in
      if specs = [] then Location.raise_errorf ~loc "%%fields: at least one field is required";
      let names = List.map fst specs in
      let fld n = B.pexp_ident ~loc { txt = Ldot (Lident "Fields", n); loc } in
      (* the wire projection doc: [(field_name Fields.a, <value>); …]. Mongo includes _id by default,
         so unless a projected field's wire name is "_id" (i.e. the model's id), suppress it with
         _id:0 — we ship EXACTLY what was asked for, not the id nobody requested. *)
      let includes = List.map (fun (n, v) -> [%expr (Codec.field_name [%e fld n], [%e v])]) specs in
      let any_is_id =
        List.fold_right
          (fun n acc -> [%expr Codec.field_name [%e fld n] = "_id" || [%e acc]])
          names [%expr false]
      in
      let fields_list =
        [%expr
          let incs = [%e B.elist ~loc includes] in
          if [%e any_is_id] then incs else ("_id", Bson.Int 0) :: incs]
      in
      (* the object: object method a = a method b = b end *)
      let obj =
        let meths =
          List.map
            (fun n ->
              B.pcf_method ~loc
                ({ txt = n; loc }, Public, Cfk_concrete (Fresh, B.evar ~loc n)))
            names
        in
        B.pexp_object ~loc (B.class_structure ~self:(B.ppat_any ~loc) ~fields:meths)
      in
      (* nest: match Codec.field_get Fields.a __d with Error e -> Error e | Ok a -> … Ok obj *)
      let decode_body =
        List.fold_right
          (fun n acc ->
            [%expr
              match Codec.field_get [%e fld n] __d with
              | Error __e -> Error __e
              | Ok [%p B.pvar ~loc n] -> [%e acc]])
          names
          [%expr Ok [%e obj]]
      in
      [%expr Proj.v ~fields:[%e fields_list] ~decode:(fun __d -> [%e decode_body])])

let () = Driver.register_transformation "fennec_fields" ~extensions:[ fields_expander ]
