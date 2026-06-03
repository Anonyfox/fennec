open Ppxlib
let scope_of css = "fur-" ^ String.sub (Digest.to_hex (Digest.string css)) 0 6
let module_scope = ref None
let starts_with p s = String.length s >= String.length p && String.sub s 0 (String.length p) = p
let dash s = String.map (fun c -> if c = '_' then '-' else c) s
let lid_str f = match f.pexp_desc with
  | Pexp_ident { txt; _ } -> Some (String.concat "." (Longident.flatten_exn txt)) | _ -> None
let head c = match c.pexp_desc with Pexp_apply (f, _) -> lid_str f | _ -> None
let list_heads = ["List.map";"List.mapi";"List.filter_map";"List.concat_map";"Array.map";"each";"Fur.each"]
(* an event attribute (onClick, onInput, …): on + UpperCase *)
let is_event name = String.length name > 2 && name.[0]='o' && name.[1]='n' && name.[2] >= 'A' && name.[2] <= 'Z'
(* handler sugar: wrap a unit statement in `fun () ->`, but leave real functions /
   function references untouched (onClick=(count += 1) vs onClick=(fun () -> …) / onClick=h) *)
let wrap_handler ~loc arg = match arg.pexp_desc with
  | Pexp_function _ | Pexp_ident _ -> arg   (* already a function / function reference *)
  | _ -> [%expr fun () -> [%e arg]]
let is_vnode_head h =
  starts_with "Fur_html." h || starts_with "Fur." h
  || (let n = String.length h in n >= 5 && String.sub h (n-5) 5 = ".make")
let wrap_child ~loc c =
  match head c with
  | Some h when List.mem h list_heads -> [%expr Fur.frag [%e c]]
  | Some h when is_vnode_head h -> c
  | _ -> [%expr Fur.node [%e c]]
let rec list_elements e = match e.pexp_desc with
  | Pexp_construct ({ txt = Lident "[]"; _ }, None) -> Some []
  | Pexp_construct ({ txt = Lident "::"; _ }, Some { pexp_desc = Pexp_tuple [hd; tl]; _ }) ->
    (match list_elements tl with Some r -> Some (hd :: r) | None -> None)
  | _ -> None

let expand ~loc e =
  let open Ast_builder.Default in
  match e.pexp_desc with
  | Pexp_apply (f, args) ->
    let is_comp = (match f.pexp_desc with Pexp_ident { txt = Ldot (_, "createElement"); _ } -> true | _ -> false) in
    let children = ref None and labeled = ref [] and extra = ref [] and key = ref None in
    List.iter (fun (label, arg) -> match label with
      | (Labelled "children" | Optional "children") -> children := Some arg
      | (Labelled "key" | Optional "key") ->
        let k = [%expr Fur.skey [%e arg]] in           (* auto-coerce int/string keys *)
        if is_comp then key := Some k else labeled := (Labelled "key", k) :: !labeled
      | Labelled name when (not is_comp) && (starts_with "data_" name || starts_with "aria_" name) ->
        extra := [%expr Fur.attr [%e estring ~loc (dash name)] [%e arg]] :: !extra
      | Labelled name when (not is_comp) && is_event name ->
        labeled := (Labelled name, wrap_handler ~loc arg) :: !labeled  (* fun () -> sugar *)
      | (Labelled _ | Optional _) -> labeled := (label, arg) :: !labeled
      | Nolabel -> ()) args;
    let children0 = (match !children with Some c -> c | None -> [%expr []]) in
    let elems = list_elements children0 in
    let children_e = (match elems with Some es -> elist ~loc (List.map (wrap_child ~loc) es) | None -> children0) in
    (match f.pexp_desc with
     | Pexp_ident { txt = Ldot (modpath, "createElement"); _ } ->
       let make = pexp_ident ~loc { txt = Ldot (modpath, "make"); loc } in
       let child_arg = (match elems with Some [] -> [] | _ -> [ (Labelled "children", children_e) ]) in
       let call = pexp_apply ~loc make (List.rev !labeled @ child_arg @ [ (Nolabel, [%expr ()]) ]) in
       let cid = estring ~loc (String.concat "." (Longident.flatten_exn modpath)) in
       let setup = [%expr fun () -> [%e call]] in
       (match !key with
        | Some k -> [%expr Fur.comp ~cid:[%e cid] ~key:[%e k] [%e setup]]
        | None -> [%expr Fur.comp ~cid:[%e cid] [%e setup]])
     | Pexp_ident { txt = Lident tag; _ } ->
       (match !module_scope with Some sc -> extra := [%expr Fur.attr "data-fur" [%e estring ~loc sc]] :: !extra | None -> ());
       pexp_apply ~loc (evar ~loc ("Fur_html." ^ tag))
         (List.rev !labeled @ [ (Labelled "attrs", elist ~loc (List.rev !extra)); (Nolabel, children_e) ])
     | _ -> e)
  | _ -> [%expr ()]

let mapper = object
  inherit Ast_traverse.map as super
  method! expression e =
    let e = super#expression e in
    if List.exists (fun a -> a.attr_name.txt = "JSX") e.pexp_attributes
    then expand ~loc:e.pexp_loc e else e
end
(* ---- <script setup> transform ----
   A component file may be written as top-level setup bindings + `let view = <jsx>`,
   with no `make`. This folds them into the real contract:
       let make () = <setup let-ins, in source order> in fun () -> view
   Setup runs once per instance; `view` is the reactive render. A file that defines
   `make` explicitly is left ALONE — the full-power escape hatch (typed props, custom
   args, server-only shells like document.mlx). A file with no `view` is untouched. *)
let pat_name p = match p.ppat_desc with
  | Ppat_var { txt; _ } -> Some txt
  | Ppat_constraint ({ ppat_desc = Ppat_var { txt; _ }; _ }, _) -> Some txt
  | _ -> None
let item_defines name item = match item.pstr_desc with
  | Pstr_value (_, vbs) -> List.exists (fun vb -> pat_name vb.pvb_pat = Some name) vbs
  | _ -> false
let rec componentize str =
  let open Ast_builder.Default in
  (* recurse into nested module structures first (so generated route files, which
     hold one page per nested module, get each page's `view` turned into `make`) *)
  let str = List.map (fun item -> match item.pstr_desc with
    | Pstr_module ({ pmb_expr = { pmod_desc = Pmod_structure s; _ } as me; _ } as mb) ->
      { item with pstr_desc =
          Pstr_module { mb with pmb_expr = { me with pmod_desc = Pmod_structure (componentize s) } } }
    | _ -> item) str in
  if (not (List.exists (item_defines "view") str)) || List.exists (item_defines "make") str
  then str
  else begin
    let setup = ref [] and view_expr = ref None and others = ref [] in
    List.iter (fun item -> match item.pstr_desc with
      | Pstr_value (_, vbs) when List.exists (fun vb -> pat_name vb.pvb_pat = Some "view") vbs ->
        (match List.find_opt (fun vb -> pat_name vb.pvb_pat = Some "view") vbs with
         | Some vb -> view_expr := Some vb.pvb_expr | None -> ())
      | Pstr_value (rf, vbs) -> setup := (rf, vbs) :: !setup
      | _ -> others := item :: !others) str;
    match !view_expr with
    | None -> str
    | Some ve ->
      let loc = ve.pexp_loc in
      let render = [%expr fun () -> [%e ve]] in
      let body = List.fold_left (fun body (rf, vbs) -> pexp_let ~loc rf vbs body) render !setup in
      List.rev !others @ [ [%stri let make () = [%e body]] ]
  end

(* <template>…</template> block: a top-level JSX <template> expression. Rewrite it to
   `let view = <its children>` (single child as-is, multiple wrapped in a fragment),
   BEFORE the JSX mapper runs — then componentize folds it into `make` as usual. *)
let is_jsx e = List.exists (fun a -> a.attr_name.txt = "JSX") e.pexp_attributes
let desugar_blocks str =
  let open Ast_builder.Default in
  List.map (fun item -> match item.pstr_desc with
    | Pstr_eval (e, _) when is_jsx e ->
      (match e.pexp_desc with
       | Pexp_apply ({ pexp_desc = Pexp_ident { txt = Lident "template"; _ }; _ }, args) ->
         let loc = e.pexp_loc in
         let children = List.fold_left (fun acc (l, a) -> match l with
           | Labelled "children" | Optional "children" -> Some a | _ -> acc) None args in
         let view = (match children with
           | None -> [%expr Fur.frag []]
           | Some c -> (match list_elements c with
               | Some [ single ] -> single        (* one root *)
               | _ -> [%expr Fur.frag [%e c]]))    (* many roots -> fragment *)
         in
         [%stri let view = [%e view]]
       | _ -> item)
    | _ -> item) str

(* Auto-bind route params from the FILENAME. A file whose path has segments marked
   `name_` (param) or `name__` (catch-all) — e.g. products/id_.mlx — gets a typed
   `let <name> = Fur.param_or "<key>" ""` injected, folded by componentize into the
   per-instance setup (so it reads the live route param each render). The binding is a
   plain `string`; carry a warning-suppression attr so an unused param never errors.
   Only injected when the file will be componentized (has `view`, no explicit `make`)
   so the binding always lands in setup, never at module-init. *)
let route_params fname =
  String.split_on_char '/' (try Filename.chop_extension fname with _ -> fname)
  |> List.filter_map (fun c ->
       let n = String.length c in
       if n >= 3 && c.[n-1] = '_' && c.[n-2] = '_' then Some (String.sub c 0 (n-2), "*")
       else if n >= 2 && c.[n-1] = '_' && c.[n-2] <> '_' then Some (String.sub c 0 (n-1), String.sub c 0 (n-1))
       else None)
let input_fname str =
  List.fold_left (fun acc item ->
    match acc with Some _ -> acc | None ->
      let p = item.pstr_loc.loc_start.Lexing.pos_fname in
      if p = "" || p = "_none_" then None else Some p) None str
  |> Option.value ~default:""
let inject_params str =
  let open Ast_builder.Default in
  if (not (List.exists (item_defines "view") str)) || List.exists (item_defines "make") str then str
  else
    match route_params (input_fname str) with
    | [] -> str
    | params ->
      let loc = Location.none in
      let no_unused = attribute ~loc ~name:{ txt = "warning"; loc }
                        ~payload:(PStr [ pstr_eval ~loc (estring ~loc "-26-27") [] ]) in
      let bindings = List.map (fun (name, key) ->
        let vb = { (value_binding ~loc ~pat:(ppat_var ~loc { txt = name; loc })
                      ~expr:[%expr Fur.param_or [%e estring ~loc key] ""])
                   with pvb_attributes = [ no_unused ] } in
        pstr_value ~loc Nonrecursive [ vb ]) params in
      bindings @ str

let scan_scope str = List.iter (fun item -> match item.pstr_desc with
  | Pstr_extension (({ txt = "style"; _ },
      PStr [ { pstr_desc = Pstr_eval ({ pexp_desc = Pexp_constant (Pconst_string (css,_,_)); _ }, _); _ } ]), _) ->
    module_scope := Some (scope_of css)
  | _ -> ()) str
let impl str =
  module_scope := None; scan_scope str;
  let str = List.filter (fun item -> match item.pstr_desc with
    | Pstr_extension (({ txt = "style"; _ }, _), _) -> false | _ -> true) str in
  componentize (inject_params (mapper#structure (desugar_blocks str)))
let () = Driver.register_transformation "iso_jsx" ~impl
