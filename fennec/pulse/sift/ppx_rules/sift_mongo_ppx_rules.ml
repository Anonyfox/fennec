(* The Mongo query-DSL ppx extensions — the pleasant Mongo sugar; one of the three modules of
   fennec.pulse.sift.ppx.rules. Each expands, under a model's scope (so its [Fields] resolve), to a
   sift.mongo artifact:
     [%q status = "doing" && age > 18]   -> Filter.t  (a Bson selector)
     [%fields a; b; author / name]        -> Projection.t    (a typed projection)
     [%sort age desc; name]               -> Sort.t
     [%set status = "done"]               -> Update.t  (an update modifier)
     [%index unique [ asc email ]]        -> declares an index
   Pure AST -> sift.mongo references, resolved at the consumer (which links fennec.pulse.sift.mongo).
   fur.ppx and the standalone sift.ppx link {!rules} into their single ppx pass. *)

open Ppxlib

(* ---- the [%fields a; b; …] projection extension ---------------------------------------------
   Expands, under the model's scope (so [Fields] resolves), to a Projection.t whose decoder builds an
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
        | [ seg ] -> [%expr Sift.field_name [%e handle acc_prefix seg]]
        | seg :: rest ->
            [%expr Sift.field_name [%e handle acc_prefix seg] ^ "." ^ [%e wire_key (acc_prefix @ [ seg ]) rest]]
      in
      let includes = List.map (fun (p, v) -> [%expr ([%e wire_key [] p], [%e v])]) specs in
      (* _id is shipped by default; suppress it unless a TOP-LEVEL field maps to wire "_id" *)
      let any_is_id =
        List.fold_right
          (fun (p, _) acc -> match p with [ seg ] -> [%expr Sift.field_name [%e handle [] seg] = "_id" || [%e acc]] | _ -> acc)
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
              | `Leaf -> [%expr Sift.field_get [%e handle prefix h] [%e doc]]
              | `Branch tails ->
                  let sub = "__sub" ^ string_of_int depth ^ "_" ^ h in
                  [%expr
                    match Bson.get [%e doc] (Sift.field_name [%e handle prefix h]) with
                    | None -> Error [ Sift.err ~code:"required" [ [%e B.estring ~loc h] ] "is required" ]
                    | Some [%p B.pvar ~loc sub] ->
                        [%e build (prefix @ [ h ]) (B.evar ~loc sub) (depth + 1) tails]]
            in
            [%expr match [%e decode_h] with Error __e -> Error __e | Ok [%p B.pvar ~loc (var h)] -> [%e acc]])
          heads [%expr Ok [%e obj]]
      in
      let decode_body = build [] [%expr __d] 0 paths
      in
      [%expr Projection.v ~fields:[%e fields_list] ~decode:(fun __d -> [%e decode_body])])

(* ---- [%q …] / [%sort …] / [%set …] — write queries as expressions, not API calls -------------
   Resolved against the model's [Fields] in scope (open Task, or Task.(…)). A bare ident is a field
   (Fields.x); record-access [a.b] is a dotted path (Sift.dot, navigating the embedded Fields).
   Everything expands to the typed Filter/Sort/M calls, so a wrong field/value is still a compile error —
   only the noise is gone. Richer matchers/modifiers (regex/has/inc/push) use Filter/M directly. *)

(* a field path expression (bare ident or a.b.c record-access) → the handle, dotted via Sift.dot *)
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
    | s :: rest -> [%expr Sift.dot [%e B.pexp_ident ~loc { txt = Ldot (modp, s); loc }] [%e chain (Ldot (modp, String.capitalize_ascii s)) rest]]
    | [] -> assert false
  in
  chain (Lident "Fields") segs

let q_expander =
  Extension.declare "q" Extension.Context.expression Ast_pattern.(single_expr_payload __)
    (fun ~loc ~path:_ e ->
      let fld e = field_handle ~loc (segs_of_field e) in
      let cmp = function
        | "=" -> Some [%expr Filter.eq] | "<>" -> Some [%expr Filter.ne] | "<" -> Some [%expr Filter.lt]
        | "<=" -> Some [%expr Filter.lte] | ">" -> Some [%expr Filter.gt] | ">=" -> Some [%expr Filter.gte] | _ -> None
      in
      let rec q_of e =
        match e.pexp_desc with
        | Pexp_construct ({ txt = Lident "true"; _ }, None) -> [%expr Filter.all []]
        | Pexp_apply ({ pexp_desc = Pexp_ident { txt = Lident "&&"; _ }; _ }, [ (_, l); (_, r) ]) ->
            [%expr Filter.all [ [%e q_of l]; [%e q_of r] ]]
        | Pexp_apply ({ pexp_desc = Pexp_ident { txt = Lident "||"; _ }; _ }, [ (_, l); (_, r) ]) ->
            [%expr Filter.any [ [%e q_of l]; [%e q_of r] ]]
        | Pexp_apply ({ pexp_desc = Pexp_ident { txt = Lident op; _ }; _ }, [ (_, l); (_, r) ]) when cmp op <> None ->
            [%expr [%e Option.get (cmp op)] [%e fld l] [%e r]]
        | _ ->
            Location.raise_errorf ~loc:e.pexp_loc
              "%%q: use comparisons (= <> < <= > >=) and && / ||; for in/has/regex use Filter directly"
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

(* [%index unique email; title; desc created; unique (team, slug)] — declares the model's indexes,
   reading bare field names (resolved against Fields in scope, like the other DSLs). A bare field is
   an ascending single-key index; [unique x] marks it unique; [desc x] descends; a TUPLE is a
   compound key. Expands to [Def.index collection [ … ]] — no Def/Index/asc/Fields tokens visible. *)
let index_expander =
  Extension.declare "index" Extension.Context.expression Ast_pattern.(single_expr_payload __)
    (fun ~loc ~path:_ e ->
      let module B = Ast_builder.Default in
      let key e =
        (* one key of an index → Index.asc/desc Fields.f *)
        match e.pexp_desc with
        | Pexp_ident { txt = Lident _; _ } -> [%expr Index.asc [%e field_handle ~loc (segs_of_field e)]]
        | Pexp_apply ({ pexp_desc = Pexp_ident { txt = Lident "desc"; _ }; _ }, [ (_, f) ]) ->
            [%expr Index.desc [%e field_handle ~loc (segs_of_field f)]]
        | _ -> Location.raise_errorf ~loc:e.pexp_loc "%%index: a key is a field name or `desc field`"
      in
      let body e =
        (* the index's key(s): a tuple is a compound key, otherwise a single key *)
        match e.pexp_desc with
        | Pexp_tuple es -> [%expr Index.compound [%e B.elist ~loc (List.map key es)]]
        | _ -> key e
      in
      let one e =
        match e.pexp_desc with
        | Pexp_apply ({ pexp_desc = Pexp_ident { txt = Lident "unique"; _ }; _ }, [ (_, inner) ]) ->
            [%expr Index.unique [%e body inner]]
        | _ -> body e
      in
      let rec items e = match e.pexp_desc with Pexp_sequence (a, b) -> items a @ items b | _ -> [ one e ] in
      [%expr Def.index collection [%e B.elist ~loc (items e)]])

let set_expander =
  Extension.declare "set" Extension.Context.expression Ast_pattern.(single_expr_payload __)
    (fun ~loc ~path:_ e ->
      let one e =
        match e.pexp_desc with
        | Pexp_apply ({ pexp_desc = Pexp_ident { txt = Lident "="; _ }; _ }, [ (_, l); (_, r) ]) ->
            [%expr Update.set [%e field_handle ~loc (segs_of_field l)] [%e r]]
        | _ -> Location.raise_errorf ~loc:e.pexp_loc "%%set: each clause is `field = value`; for inc/push/… use Update"
      in
      let rec assigns e = match e.pexp_desc with Pexp_sequence (a, b) -> assigns a @ assigns b | _ -> [ one e ] in
      [%expr Update.all [%e Ast_builder.Default.elist ~loc (assigns e)]])

(* exposed for composition into a SINGLE driver: fur.ppx (mlx components) and the thin standalone
   both fold these into their own [register_transformation ~rules], so a file pays ONE ppx process
   for mlx + tests + the collection deriver + projections + query/sort/set DSLs. *)
let rules =
  List.map Context_free.Rule.extension
    [ fields_expander; q_expander; sort_expander; set_expander; index_expander ]
