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

(* ---- handler files (frontend/handlers/<name>.mlx) ----
   A HANDLER is ONE .mlx: an isomorphic `view : payload -> vnode` + a server `load : conn -> outcome`,
   over a `payload` type (which derives its `codec`). The fur ppx (-handler) derives key/bundle from
   the FILENAME and fuses them, so the file holds zero plumbing:
     - server (-handler):              wrap `load` with the facade opened (so Conn/render/redirect
                                       resolve) and append `serve` (run load -> seed+SSR via render_doc,
                                       or redirect/error/not_found);
     - client (-handler -conn-client): STRIP `load`, append `boot` (start_page over the seeded view),
                                       so the jsoo bundle never sees Conn.
   The ONLY Conn-aware code is the generated server `serve`. This is Eliom's server/client section
   split via one driver flag — no second source file, no userland ceremony. *)
let handler_mode = ref false
let () = Driver.add_arg "-handler" (Stdlib.Arg.Set handler_mode)
    ~doc:"compile a frontend/handlers/*.mlx file (payload/load/view -> serve+boot)"
let conn_client = ref false
let () = Driver.add_arg "-conn-client" (Stdlib.Arg.Set conn_client)
    ~doc:"strip the server `load` (the client/jsoo build of a handler)"

let handler_name str =
  let f = input_fname str in
  let base = (try Filename.basename f with _ -> f) in
  (try Filename.chop_extension base with _ -> base)

let desugar_handler str =
  let open Ast_builder.Default in
  let loc = Location.none in
  let name = handler_name str in
  let key = estring ~loc ("handler:" ^ name) in
  let bundle = estring ~loc ("/_handlers/" ^ name ^ "/main.js") in
  (* keep+wrap `load` (server), or strip it (client). The facade open stays LOCAL to load, so it never
     leaks into the client compilation — and on the client load is gone entirely. *)
  let body = List.concat_map (fun item -> match item.pstr_desc with
    | Pstr_value (rf, [ vb ]) when pat_name vb.pvb_pat = Some "load" ->
      if !conn_client then []
      else
        let e = [%expr let open Fennec in let open Fennec_fur_handler.Handler in [%e vb.pvb_expr]] in
        [ { item with pstr_desc = Pstr_value (rf, [ { vb with pvb_expr = e } ]) } ]
    | _ -> [ item ]) str
  in
  let extra =
    if !conn_client then
      [ [%stri let boot () =
          Fur_csr.start_page (fun () -> view (Fennec_fur_handler.Handler.payload codec ~key:[%e key])) ] ]
    else
      [ [%stri let serve conn =
          Fennec.Fur.Data.with_context @@ fun () ->   (* per-request seed + Head isolation for this render *)
          match load conn with
          | Fennec_fur_handler.Handler.Render p ->
              Fennec.Conn.html conn
                (Fennec_fur_handler.Handler.render_doc ~key:[%e key] ~codec ~bundle:[%e bundle] p view)
          | Fennec_fur_handler.Handler.Html p ->
              Fennec.Conn.html conn (Fennec_fur_handler.Handler.render_static (view p))
          | Fennec_fur_handler.Handler.Json s -> Fennec.Conn.json conn s
          | Fennec_fur_handler.Handler.Text s -> Fennec.Conn.text conn s
          | Fennec_fur_handler.Handler.Redirect u -> Fennec.Conn.redirect conn u
          | Fennec_fur_handler.Handler.Not_found -> Fennec.Conn.text ~status:404 conn "Not found"
          | Fennec_fur_handler.Handler.Error s -> Fennec.Conn.text ~status:s conn ("Error " ^ string_of_int s) ] ]
  in
  body @ extra

(* FORM handler (server-rendered, no bundle): payload `type t` + `view : Form.ctx -> vnode` +
   `submit : conn -> t -> Form.outcome`. We generate get/post/serve. Re-rendering HTML is first-class:
   the author returns `again`/`redirect`/`page`, and codec-invalid input auto re-renders the form with
   the per-field errors AND the submitted values preserved (Form.ctx carries the conn). The client build
   (-conn-client) strips it to `type t` (no Form/Handler/Conn client-side; route_gen never bundles it). *)
let desugar_form_handler str =
  let open Ast_builder.Default in
  let loc = Location.none in
  if !conn_client then
    (* a form handler has no client half — keep only the payload type (codec-only, dead), drop the rest *)
    List.filter (fun item -> match item.pstr_desc with Pstr_type _ -> true | _ -> false) str
  else
    let body =
      List.concat_map
        (fun item ->
          match item.pstr_desc with
          | Pstr_value (rf, [ vb ]) when pat_name vb.pvb_pat = Some "submit" ->
            (* open the facade LOCALLY in `submit` so it reads `again …`/`redirect …`/`page …` bare and
               can use Pulse/Conn for real work — never leaking into the client (load-less, stripped) *)
            let e = [%expr let open Fennec in let open Fennec.Fur.Form in [%e vb.pvb_expr]] in
            [ { item with pstr_desc = Pstr_value (rf, [ { vb with pvb_expr = e } ]) } ]
          | _ -> [ item ])
        str
    in
    let extra =
      [ [%stri
          let get conn =
            let conn, flash = Fennec.Fur.Handler.flash conn in
            Fennec.Fur.Handler.html conn (view (Fennec.Fur.Form.ctx ~conn ~flash ~errors:[]))];
        [%stri
          let post conn =
            let rerender errs =
              Fennec.Fur.Handler.html ~status:422 conn (view (Fennec.Fur.Form.ctx ~conn ~flash:None ~errors:errs))
            in
            match Fennec.Fur.Form.read codec conn with
            | Error errs -> rerender errs
            | Ok t -> (
              match submit conn t with
              | Fennec.Fur.Form.Again errs -> rerender errs
              | Fennec.Fur.Form.Redirect (u, f) -> Fennec.Fur.Handler.redirect conn u ?flash:f
              | Fennec.Fur.Form.Page v -> Fennec.Fur.Handler.html conn v)];
        [%stri
          let serve conn =
            Fennec.Fur.Data.with_context @@ fun () ->   (* per-request seed + Head isolation (view + render) *)
            match Fennec.Conn.meth conn with Fennec.Http.POST -> post conn | _ -> get conn] ]
    in
    (* alias Form so the view's `Form.ctx`/`Form.flash`/`Form.error`/… resolve: the handler lib opens
       CORE Fur (which has no Form — the client mirror needs core Fur, as Fennec.Fur.Form is native-only).
       Dropped in the client build (the strip above keeps only `type t`). *)
    [%stri module Form = Fennec.Fur.Form] :: (body @ extra)

let scan_scope str = List.iter (fun item -> match item.pstr_desc with
  | Pstr_extension (({ txt = "style"; _ },
      PStr [ { pstr_desc = Pstr_eval ({ pexp_desc = Pexp_constant (Pconst_string (css,_,_)); _ }, _); _ } ]), _) ->
    module_scope := Some (scope_of css)
  | _ -> ()) str
let impl str =
  module_scope := None; scan_scope str;
  let str = List.filter (fun item -> match item.pstr_desc with
    | Pstr_extension (({ txt = "style"; _ }, _), _) -> false | _ -> true) str in
  (* MLX desugaring first (turn JSX into plain OCaml), THEN append executable doc-block tests. Handler
     files take a SEPARATE path and SKIP componentize/inject_params (a handler's `view` is a plain
     function, not a component). We dispatch by SHAPE: a `load` is an SPA handler (fuse load/view ->
     serve+boot); a `submit` is a server-rendered FORM handler (view/submit -> get/post/serve). Sibling
     modules in the same lib (e.g. the route_gen-generated routes.ml) define neither -> the normal path. *)
  if !handler_mode && List.exists (item_defines "load") str then
    Fennec_hunt_ppx_rules.expand_doctests (mapper#structure (desugar_blocks (desugar_handler str)))
  else if !handler_mode && List.exists (item_defines "submit") str then
    Fennec_hunt_ppx_rules.expand_doctests (mapper#structure (desugar_blocks (desugar_form_handler str)))
  else
    Fennec_hunt_ppx_rules.expand_doctests (componentize (inject_params (mapper#structure (desugar_blocks str))))
(* register the MLX transform (whole-structure) + the inline test rules (context-free) in
   ONE driver, so a library using (pps fennec.fur.ppx) pays ONE ppx process for both *)
(* ONE driver for the whole component pipeline: MLX/JSX sugar (impl) + inline tests (hunt rules) +
   the typed-collection deriver & [%fields] projections (collection rules). [(pps fennec.fur.ppx)]
   alone gives all of it — no extra pps entry, no extra ppx subprocess. *)
(* force the [collection] deriver to register (it auto-registers on load; referencing it also loads
   Sift_ppx_rules for the [sift]/[model] derivers, which the collection rules reuse via model_core) *)
let () = ignore Fennec_pulse_collection_ppx_rules.deriver

let () = Driver.register_transformation "iso_jsx" ~impl
    ~rules:(Fennec_hunt_ppx_rules.rules @ Sift_mongo_ppx_rules.rules)
