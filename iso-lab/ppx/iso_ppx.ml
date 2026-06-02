open Ppxlib
let scope_of css = "iso-" ^ String.sub (Digest.to_hex (Digest.string css)) 0 6
let module_scope = ref None
let starts_with p s = String.length s >= String.length p && String.sub s 0 (String.length p) = p
let dash s = String.map (fun c -> if c = '_' then '-' else c) s
let lid_str f = match f.pexp_desc with
  | Pexp_ident { txt; _ } -> Some (String.concat "." (Longident.flatten_exn txt)) | _ -> None
let head c = match c.pexp_desc with Pexp_apply (f, _) -> lid_str f | _ -> None
let list_heads = ["List.map";"List.mapi";"List.filter_map";"List.concat_map";"Array.map"]
let is_vnode_head h =
  starts_with "Html." h || starts_with "Iso." h
  || (let n = String.length h in n >= 5 && String.sub h (n-5) 5 = ".make")
let wrap_child ~loc c =
  match head c with
  | Some h when List.mem h list_heads -> [%expr Iso.frag [%e c]]
  | Some h when is_vnode_head h -> c
  | _ -> [%expr Iso.node [%e c]]
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
      | (Labelled "key" | Optional "key") when is_comp -> key := Some arg
      | Labelled name when (not is_comp) && (starts_with "data_" name || starts_with "aria_" name) ->
        extra := [%expr Iso.attr [%e estring ~loc (dash name)] [%e arg]] :: !extra
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
        | Some k -> [%expr Iso.comp ~cid:[%e cid] ~key:[%e k] [%e setup]]
        | None -> [%expr Iso.comp ~cid:[%e cid] [%e setup]])
     | Pexp_ident { txt = Lident tag; _ } ->
       (match !module_scope with Some sc -> extra := [%expr Iso.attr "data-iso" [%e estring ~loc sc]] :: !extra | None -> ());
       pexp_apply ~loc (evar ~loc ("Html." ^ tag))
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
let componentize str =
  let open Ast_builder.Default in
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

let scan_scope str = List.iter (fun item -> match item.pstr_desc with
  | Pstr_extension (({ txt = "style"; _ },
      PStr [ { pstr_desc = Pstr_eval ({ pexp_desc = Pexp_constant (Pconst_string (css,_,_)); _ }, _); _ } ]), _) ->
    module_scope := Some (scope_of css)
  | _ -> ()) str
let impl str =
  module_scope := None; scan_scope str;
  let str = List.filter (fun item -> match item.pstr_desc with
    | Pstr_extension (({ txt = "style"; _ }, _), _) -> false | _ -> true) str in
  componentize (mapper#structure str)
let () = Driver.register_transformation "iso_jsx" ~impl
