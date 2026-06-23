open Ppxlib
let scope_of css = "fur-" ^ String.sub (Digest.to_hex (Digest.string css)) 0 6
let module_scope = ref None
(* TRUE only while transforming a [.mlx] file. The fur.ppx is also attached to plain [.ml]
   libraries (fennec.fur core, the sift ppx_rules, …) for inline tests — and THOSE files use
   real OCaml refs ([!current], [!batch_depth], …). The [!s] read sugar must therefore fire
   ONLY in [.mlx] source, where signals (not refs) are the idiom; on [.ml] [!] keeps its deref
   meaning. The JSX/`<script setup>`/param sugars are already self-gating (they key off [@JSX]
   nodes / a `view` binding, which only [.mlx] produces), so only the bang rewrite needs this. *)
let in_mlx = ref false
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
(* a child whose head is one of our own combinators is ALREADY a vnode: pass it through
   verbatim. This makes the desugar IDEMPOTENT — an explicit (node …) / (frag …) is not
   re-wrapped into Fur.node (node …). So the canonical migrated child `(expr)` and a
   hand-written `(node expr)` BOTH lower to a SINGLE `Fur.node expr` — and a stray
   hand-written `node`/`frag`/`text`/`raw` still compiles. *)
let is_passthrough_head h =
  is_vnode_head h || List.mem h [ "node"; "frag"; "text"; "raw"; "Fur.text"; "Fur.raw" ]
(* JSX child positions accept three shapes, mapped UNIFORMLY to the runtime:
   - a list-producing expression (each / List.map / …) -> wrapped in {!Fur.frag} (a
     `vnode list` becomes one fragment child), so userland writes a bare `each` — never
     `frag (each …)`;
   - an expression that already evaluates to a vnode (a nested <jsx> element/component, an
     `outlet ()`, or an explicit node/frag/text) -> passed through untouched (the one runtime
     coercion, never doubled);
   - any other VALUE expression -> wrapped in {!Fur.node}, the total int|float|string|vnode
     coercion, so userland writes `(expr)` and never spells `node` itself. *)
let wrap_child ~loc c =
  match head c with
  | Some h when List.mem h list_heads -> [%expr Fur.frag [%e c]]
  | Some h when is_passthrough_head h -> c
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
    let children = ref None and labeled = ref [] and extra = ref [] and key = ref None and spread = ref [] in
    List.iter (fun (label, arg) -> match label with
      | (Labelled "children" | Optional "children") -> children := Some arg
      | (Labelled "key" | Optional "key") ->
        let k = [%expr Fur.skey [%e arg]] in           (* auto-coerce int/string keys *)
        if is_comp then key := Some k else labeled := (Labelled "key", k) :: !labeled
      (* attr-list SPREAD: [attrs=(expr)] on a plain element merges an [attr list] (e.g.
         [Form.input_attrs Fields.x] — codec-driven HTML5 attrs) into the element's attributes, like
         React's [{...spread}]. Last-wins is the DOM's own attribute semantics; we append AFTER the
         element's labeled attrs + the data-/aria-/scope extras, so an explicit [name=]/[value=] still
         takes precedence in source order via {!Fur_html}'s [?attrs] tail. On a component it stays a
         normal [Labelled "attrs"] prop (a component may legitimately take an [attrs] argument). *)
      | Labelled "attrs" when not is_comp -> spread := arg :: !spread
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
       (* the element's static attrs (data-/aria-/scope) followed by every [attrs=] spread, in source
          order — so [Fur_html]'s [?attrs] tail carries [scope ++ input_attrs ++ …] after the labeled
          ones. With nothing spread this is byte-identical to the old [elist extra]. *)
       let attrs_e =
         List.fold_left (fun acc s -> [%expr [%e acc] @ [%e s]]) (elist ~loc (List.rev !extra)) (List.rev !spread)
       in
       pexp_apply ~loc (evar ~loc ("Fur_html." ^ tag))
         (List.rev !labeled @ [ (Labelled "attrs", attrs_e); (Nolabel, children_e) ])
     | _ -> e)
  | _ -> [%expr ()]

(* ---- [!s] read sugar (the fur.mli-promised alias for {!Fur.get}) ----
   In a Fur component a signal is read with [get s]; [!s] is the JSX/Svelte-flavoured
   shorthand. We rewrite ONLY the exact prefix application [! e] (one unlabeled argument) to
   the bare [get e] — bare, NOT [Fur.get], so it is byte-for-byte the SAME expansion as an
   explicit [(get s)] child (every [.mlx] consumer compiles with [-open Fur], the same open
   that already lets userland write [signal]/[get]/[set] unqualified). Emitting bare [get]
   also keeps the result a plain VALUE so {!wrap_child} still wraps it in [Fur.node]; a
   qualified [Fur.get] would be misread as an already-vnode head and dropped through unwrapped.

   The clash with OCaml's deref is type-SAFE, never silent: signals carry [-open Fur]'s
   [signal] type, so [!] on a real [ref] becomes [get (r : _ ref)] — a compile error, not a
   wrong read. Writes stay explicit ([set]/[update]/[+=]/[-=]); the MEMORY rule "never [:=] a
   signal" holds, and [.mlx] uses signals, not refs (see fur.mli). Any operator merely NAMED
   [!…] (a custom [!=]-style op) or [!] applied with labels is left untouched. Runs over every
   expression node, so it fires in a JSX child [(!count)], an attribute [value=(!draft)], and
   an ordinary expression alike. *)
let is_bang_get e = match e.pexp_desc with
  | Pexp_apply ({ pexp_desc = Pexp_ident { txt = Lident "!"; _ }; _ }, [ (Nolabel, _) ]) -> true
  | _ -> false
let bang_get ~loc e = match e.pexp_desc with
  | Pexp_apply ({ pexp_desc = Pexp_ident { txt = Lident "!"; _ }; _ }, [ (Nolabel, arg) ]) ->
    [%expr get [%e arg]]
  | _ -> e

let mapper = object
  inherit Ast_traverse.map as super
  method! expression e =
    let e = super#expression e in
    let e =
      if List.exists (fun a -> a.attr_name.txt = "JSX") e.pexp_attributes
      then expand ~loc:e.pexp_loc e else e
    in
    if !in_mlx && is_bang_get e then bang_get ~loc:e.pexp_loc e else e
end

(* ---- CO-LOCATED data: strip the inline SERVER fetcher from the CLIENT (jsoo) build ----
   A component declares its server fetcher inline as [Data.local key ~fallback (Server_only.fn body)]
   (or [Data.model_local]). The fetcher BODY is server-only — it may read secrets, hit the DB, call
   [compute_*]. Components compile ONCE into a shared lib that is ALSO linked into the jsoo bundle, so
   to keep server logic out of the bundle the CLIENT build of that lib runs with [-data-client], and
   this pass rewrites the WHOLE co-located call to its CLIENT LOWERING:

     Data.local       NAME [~path P] ~fallback FB (Server_only.fn body)  ->  Data.local_client       NAME [~path P] ~fallback FB
     Data.model_local C NAME [~path P] ~fallback FB (Server_only.fn body)  ->  Data.model_local_client C NAME [~path P] ~fallback FB

   It DROPS the trailing [Server_only.fn body] argument entirely — so [body] (and everything it
   references) never enters the client AST — and redirects to the [_client] variant, which is exactly the
   bare seeded + refetchable {!Data.string}/{!Data.model} over the derived path. Result: zero client
   growth (the co-located form compiles identically to the un-co-located one) AND zero server logic. It
   is the component mirror of a handler's [-conn-client] stripping [load].

   We key on the call HEAD [local]/[model_local] (any prefix: bare / [Data.] / [Fur.Data.]) and drop the
   single trailing positional argument (the fetcher), keeping every labeled arg ([~path], [~fallback]) and
   the leading positional ones (the name, and for [model_local] the codec) untouched. *)
let data_client = ref false
let () = Driver.add_arg "-data-client" (Stdlib.Arg.Set data_client)
    ~doc:"strip inline Data.local/model_local server fetchers (the client/jsoo build of a component lib)"

(* is this longident a co-located [local]/[model_local] head (any module prefix)? return the client
   lowering's longident (same prefix, [_client] suffix). *)
let colocated_client_head (lid : Longident.t) : Longident.t option =
  let lower base = function
    | Lident b when b = base -> Some (Lident (b ^ "_client"))
    | Ldot (p, b) when b = base -> Some (Ldot (p, b ^ "_client"))
    | _ -> None
  in
  match lower "local" lid with
  | Some _ as r -> r
  | None -> lower "model_local" lid

(* is [e] the trailing fetcher arg we drop — a [Server_only.fn …] application (any prefix)? Restricting
   the drop to exactly this shape keeps the rewrite from mangling an unrelated [local]-named function. *)
let is_server_only_fn_arg e =
  match e.pexp_desc with
  | Pexp_apply ({ pexp_desc = Pexp_ident { txt = (Ldot (Ldot (_, "Server_only"), "fn") | Ldot (Lident "Server_only", "fn")); _ }; _ }, _) -> true
  | _ -> false

let strip_server_only = object
  inherit Ast_traverse.map as super
  method! expression e =
    let e = super#expression e in
    match e.pexp_desc with
    | Pexp_apply (({ pexp_desc = Pexp_ident ({ txt; _ } as id); _ } as f), args) -> (
      match colocated_client_head txt with
      | Some client_txt ->
        (* drop the LAST positional arg iff it is the [Server_only.fn …] fetcher; redirect to [_client]
           and append a trailing [()] (so the [_client] variant's optional [?path] erases). *)
        (match List.rev args with
         | (Nolabel, last) :: rest_rev when is_server_only_fn_arg last ->
           let loc = e.pexp_loc in
           let f' = { f with pexp_desc = Pexp_ident { id with txt = client_txt } } in
           { e with pexp_desc = Pexp_apply (f', List.rev rest_rev @ [ (Nolabel, [%expr ()]) ]) }
         | _ -> e (* not a co-located fetcher call — leave it *))
      | None -> e)
    | _ -> e
end
(* ---- THE component shape: ONE function body, no manual render thunk ----

   A Fur component is, at the runtime level, a SETUP thunk that returns a RENDER thunk
   ([Fur.comp]'s argument is [unit -> (unit -> vnode)]): setup runs ONCE per mounted
   instance (create signals, subscribe, [on_mount], derive-once values), and the render
   thunk RE-RUNS whenever a signal it reads changes. Two userland shapes both lower to it,
   and a beginner never writes the [fun () ->] split:

   1. No local state — module-level [let view = <jsx>]. Folded here into
        let make () = <module-level setup let-bindings, in source order> in fun () -> view
      (the [let () = Head.title …]-style top-level bindings become the per-instance setup).

   2. Local state / props — [let make <params> () = <setup let-ins> in <trailing jsx>].
      The author writes the render as the TRAILING EXPRESSION of the body; this transform
      emits the [fun () ->] wrapper around it. Everything BEFORE the trailing expression —
      every [let … in], [let open … in], [let () = … in] side effect — is SETUP (runs once
      when [make ~… ()] is called, i.e. once per mount). Only the trailing JSX is the render
      thunk (re-runs on signal change). This is Solid/Svelte's "component body runs once,
      reactivity lives in the markup" model.

   The boundary is keyed to the component-instance CONTRACT: a component [make]'s parameter
   list ENDS IN [()] (the JSX desugar always calls [Module.make ~props () ] — see {!expand}),
   and the render is the body after that final [()]. The explicit [fun () ->] shape STILL
   compiles unchanged (it is detected as already-thunked and left alone — backward compatible;
   migration is opt-in). A [make] whose last parameter is NOT [()] (a server-only document
   shell like [let make ctx = <html>…</html>], consumed as a plain [ctx -> vnode]) is NEVER
   wrapped — it is the full-power escape hatch.

   The ONE footgun this shape introduces, and the SAME one Solid has: a setup
   [let x = get s in …] computes [x] ONCE (at mount), so it is NOT reactive — the reactive
   forms are reading the signal IN the markup ([(get s)] / [(!s)] in the JSX) or a [memo]. We
   emit a non-fatal ppx warning on exactly that pattern (a setup binding whose RHS is a bare
   tracking read [get s] / [!s]); [peek] is the deliberate snapshot and never warns. *)
let pat_name p = match p.ppat_desc with
  | Ppat_var { txt; _ } -> Some txt
  | Ppat_constraint ({ ppat_desc = Ppat_var { txt; _ }; _ }, _) -> Some txt
  | _ -> None
let item_defines name item = match item.pstr_desc with
  | Pstr_value (_, vbs) -> List.exists (fun vb -> pat_name vb.pvb_pat = Some name) vbs
  | _ -> false

(* the unit pattern [()] — the final parameter of every component [make] (the JSX desugar
   always applies [make ~props ()]). Distinguishes a component [make ~props ()] from a shell
   [make ctx]. A [(() : unit)] constraint counts too. *)
let rec is_unit_pat p = match p.ppat_desc with
  | Ppat_construct ({ txt = Lident "()"; _ }, None) -> true
  | Ppat_constraint (p, _) -> is_unit_pat p
  | _ -> false
(* already a function (a render thunk): [fun .. -> ..] or [function | ..] — in the merged
   OCaml-5.2 AST both are [Pexp_function]. *)
let is_fun_expr e = match e.pexp_desc with Pexp_function _ -> true | _ -> false
(* descend the SETUP prelude (lets / opens / local modules / sequenced side effects, and a
   type-annotated body) to the single TRAILING expression — what the component renders. *)
let rec trailing e = match e.pexp_desc with
  | Pexp_let (_, _, body) | Pexp_open (_, body) | Pexp_letmodule (_, _, body)
  | Pexp_sequence (_, body) -> trailing body
  | Pexp_constraint (inner, _) -> trailing inner
  | _ -> e

(* ---- setup-let lint: a binding computed ONCE that looks like it wants to be reactive ----
   [get s] / [!s] (the TRACKING reads) on the RHS of a setup [let] is the footgun: the value
   is snapshotted at mount and never updates. [peek s] is the explicit non-tracking read, so
   it is left alone (a deliberate snapshot). Attached as [ocaml.ppwarning] (compiler warning,
   never an error). *)
let rhs_is_tracking_read e = match e.pexp_desc with
  | Pexp_apply ({ pexp_desc = Pexp_ident { txt = (Lident "get" | Ldot (_, "get")); _ }; _ },
                [ (Nolabel, _) ]) -> true
  | Pexp_apply ({ pexp_desc = Pexp_ident { txt = Lident "!"; _ }; _ }, [ (Nolabel, _) ]) -> true
  | _ -> false
let lint_setup_binding vb =
  let open Ast_builder.Default in
  if rhs_is_tracking_read vb.pvb_expr then
    let loc = vb.pvb_expr.pexp_loc in
    let name = match pat_name vb.pvb_pat with Some n -> n | None -> "this binding" in
    let msg =
      Printf.sprintf
        "Fur: setup binding %S reads a signal once (at mount) — it is NOT reactive. For a value \
         that updates, read the signal in the markup ((get s) / !s) or use a memo." name
    in
    let attr = attribute ~loc ~name:{ txt = "ocaml.ppwarning"; loc }
                 ~payload:(PStr [ pstr_eval ~loc (estring ~loc msg) [] ]) in
    { vb with pvb_attributes = attr :: vb.pvb_attributes }
  else vb
(* lint every setup [let] in a prelude (descend the same constructs as {!trailing}). *)
let rec lint_prelude e =
  match e.pexp_desc with
  | Pexp_let (rf, vbs, body) ->
    { e with pexp_desc = Pexp_let (rf, List.map lint_setup_binding vbs, lint_prelude body) }
  | Pexp_open (o, body) -> { e with pexp_desc = Pexp_open (o, lint_prelude body) }
  | Pexp_letmodule (n, m, body) -> { e with pexp_desc = Pexp_letmodule (n, m, lint_prelude body) }
  | Pexp_sequence (a, body) -> { e with pexp_desc = Pexp_sequence (a, lint_prelude body) }
  | Pexp_constraint (inner, t) -> { e with pexp_desc = Pexp_constraint (lint_prelude inner, t) }
  | _ -> e

(* wrap the trailing expression of a render BODY in [fun () ->] (the render thunk), keeping the
   setup prelude as-is, and lint the setup bindings. The body is everything after the final
   [()] parameter. *)
let wrap_render_body body =
  let loc = body.pexp_loc in
  let rec rebuild e = match e.pexp_desc with
    | Pexp_let (rf, vbs, b) ->
      { e with pexp_desc = Pexp_let (rf, List.map lint_setup_binding vbs, rebuild b) }
    | Pexp_open (o, b) -> { e with pexp_desc = Pexp_open (o, rebuild b) }
    | Pexp_letmodule (n, m, b) -> { e with pexp_desc = Pexp_letmodule (n, m, rebuild b) }
    | Pexp_sequence (a, b) -> { e with pexp_desc = Pexp_sequence (a, rebuild b) }
    | Pexp_constraint (inner, t) -> { e with pexp_desc = Pexp_constraint (rebuild inner, t) }
    | _ -> [%expr fun () -> [%e e]]   (* the trailing render expression *)
  in
  rebuild body

let param_is_unit = function
  | { pparam_desc = Pparam_val (_, _, pat); _ } -> is_unit_pat pat
  | _ -> false

(* Transform B — unify a component [make]'s body to ONE function returning JSX.
   In the merged OCaml-5.2 AST a whole curried lambda is ONE [Pexp_function] with all params in
   [params] and the body separate — and crucially the parser FOLDS an explicit trailing thunk
   into the param list: [fun ~a () -> fun () -> jsx] parses as [params = a, (), ()] + [body = jsx]
   (the extra [()]), exactly like the unified [params = a, ()] + [body = jsx] but with one more
   unit param. So we wrap iff this is the component-instance shape AND not already thunked:
     - the FINAL param is [()] (the component-instance contract — the JSX desugar always calls
       [make ~props ()]); and
     - we are NOT already returning a render thunk, detected two ways:
         · the body's trailing expr (past its setup prelude) is a [fun]/[function] — the
           explicit-thunk-WITH-setup shape [make () = let .. in fun () -> jsx] (the [let] keeps
           the thunk OUT of the merged param list); OR
         · the last TWO params are BOTH [()] — the explicit-thunk-WITHOUT-setup shape
           [make ~props () = fun () -> jsx] (the parser merged the thunk's [()] in).
   When neither tripwire fires, the body IS the render (a vnode, or a [match]/[if] over vnodes) ⇒
   wrap its trailing expr as [fun () -> …], keeping the setup prelude once. A non-unit final
   param ([make ctx = <html>…]) or a [Pfunction_cases] body falls through unwrapped — a
   server-only shell / explicit escape hatch, not a component instance. *)
let wrap_make_expr e =
  match e.pexp_desc with
  | Pexp_function (params, constr, Pfunction_body body) ->
    let last_two_unit = match List.rev params with
      | a :: b :: _ -> param_is_unit a && param_is_unit b | _ -> false in
    let final_unit = match List.rev params with p :: _ -> param_is_unit p | [] -> false in
    if final_unit && not last_two_unit && not (is_fun_expr (trailing body)) then
      { e with pexp_desc = Pexp_function (params, constr, Pfunction_body (wrap_render_body body)) }
    else e
  | _ -> e
let wrap_make_binding vb =
  if pat_name vb.pvb_pat = Some "make"
  then { vb with pvb_expr = wrap_make_expr vb.pvb_expr }
  else vb

let rec componentize str =
  let open Ast_builder.Default in
  (* recurse into nested module structures first (so generated route files, which
     hold one page per nested module, get each page's `view` turned into `make`) *)
  let str = List.map (fun item -> match item.pstr_desc with
    | Pstr_module ({ pmb_expr = { pmod_desc = Pmod_structure s; _ } as me; _ } as mb) ->
      { item with pstr_desc =
          Pstr_module { mb with pmb_expr = { me with pmod_desc = Pmod_structure (componentize s) } } }
    | _ -> item) str in
  if List.exists (item_defines "make") str then
    (* Transform B: a component [make] is unified (trailing-JSX → render thunk) but otherwise
       kept verbatim — the escape hatch. Non-[make] bindings are untouched. Gated to [.mlx]
       component files: the fur.ppx is ALSO attached to plain [.ml] libs (fennec.fur core,
       sift, …) for inline tests, and THOSE define ordinary [make]s ([Router.make], …) that
       return records/values, not render thunks — those must never be wrapped. ([.mlx] is the
       same reliable gate the [!s] read sugar uses; the generated route files are [.mlx].) *)
    if !in_mlx then
      List.map (fun item -> match item.pstr_desc with
        | Pstr_value (rf, vbs) -> { item with pstr_desc = Pstr_value (rf, List.map wrap_make_binding vbs) }
        | _ -> item) str
    else str
  else if not (List.exists (item_defines "view") str) then str
  else begin
    (* Transform A: no [make], a [view] — fold the module-level setup [let]s that PRECEDE [view] into
       [make]'s body (run once per mount) and emit [make] AT [view]'s position. Every OTHER item —
       [%%style], nested modules, types, and trailing inline [let%test]s — passes through unchanged in
       source order. Emitting [make] in place (not appended last) is what lets a [let%test] written
       BELOW the component reference the generated [make] (forward references don't resolve). *)
    let setup = ref [] and emitted = ref false in
    List.concat_map (fun item -> match item.pstr_desc with
      | Pstr_value (_, vbs) when List.exists (fun vb -> pat_name vb.pvb_pat = Some "view") vbs ->
        (match List.find_opt (fun vb -> pat_name vb.pvb_pat = Some "view") vbs with
         | Some vb ->
           let loc = vb.pvb_expr.pexp_loc in
           let render = [%expr fun () -> [%e vb.pvb_expr]] in
           (* lint the setup bindings ([let x = get s] above [view] is the same once-only footgun),
              then fold them in source order in front of the render thunk. *)
           let body = List.fold_left
               (fun body (rf, vbs) -> pexp_let ~loc rf (List.map lint_setup_binding vbs) body)
               render !setup in
           emitted := true;
           [ [%stri let make () = [%e body]] ]
         | None -> [ item ])
      | Pstr_value (rf, vbs) when not !emitted -> setup := (rf, vbs) :: !setup; []
      | _ -> [ item ])
      str
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
        (* open Fennec (Accounts/Pulse/Sift…) + the Paw HTTP carrier as MODULES (so [Conn]/[Http]/[Cookie]
           resolve bare) + Handler LAST (so [render]/[html]/[json]/[redirect] are its outcome verbs). We
           bind Conn/Http/Cookie as modules rather than [open Paw] on purpose: opening Paw would bring its
           [type t] (= the paw fn type) into scope and shadow the payload [type t] a [load]/[view] annotates
           ([let p : t = …]). The opens stay LOCAL to [load] (never reach the client build). *)
        let e = [%expr
          let open Fennec in
          let module Conn = Paw.Conn in
          let module Http = Paw.Http in
          let module Cookie = Paw.Cookie in
          let open Fennec_fur_handler.Handler in
          [%e vb.pvb_expr]] in
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
              Paw.Conn.html conn
                (Fennec_fur_handler.Handler.render_doc ~key:[%e key] ~codec ~bundle:[%e bundle] p view)
          | Fennec_fur_handler.Handler.Html p ->
              Paw.Conn.html conn (Fennec_fur_handler.Handler.render_static (view p))
          | Fennec_fur_handler.Handler.Json s -> Paw.Conn.json conn s
          | Fennec_fur_handler.Handler.Text s -> Paw.Conn.text conn s
          | Fennec_fur_handler.Handler.Redirect u -> Paw.Conn.redirect conn u
          | Fennec_fur_handler.Handler.Not_found -> Paw.Conn.text ~status:404 conn "Not found"
          | Fennec_fur_handler.Handler.Error s -> Paw.Conn.text ~status:s conn ("Error " ^ string_of_int s) ] ]
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
               can use Pulse/Conn for real work — never leaking into the client (load-less, stripped).
               Conn/Http/Cookie come in as MODULES (not [open Paw]) so Paw's [type t] never shadows the
               form payload [type t] that `submit (t : t)` annotates; Form is opened last for its verbs. *)
            let e = [%expr
              let open Fennec in
              let module Conn = Paw.Conn in
              let module Http = Paw.Http in
              let module Cookie = Paw.Cookie in
              let open Fennec.Fur.Form in
              [%e vb.pvb_expr]] in
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
            match Paw.Conn.meth conn with Paw.Http.POST -> post conn | _ -> get conn] ]
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
  module_scope := None;
  (* per-file gate for the [!s] read sugar (see [in_mlx]): the original source path is preserved in
     locations even after mlx-pp, so a [.mlx] extension reliably distinguishes a Fur component file
     (signals; [!s] = read) from a plain [.ml] in the same lib (refs; [!] = deref). *)
  in_mlx := (let f = input_fname str in Filename.check_suffix f ".mlx");
  scan_scope str;
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
    (* the CLIENT (jsoo) build of a component lib (-data-client): strip the inline server fetcher body
       from every [Data.local]/[model_local] so no server logic reaches the bundle (the component mirror
       of a handler's [load] strip). The server build leaves it untouched. *)
    let str = if !data_client then strip_server_only#structure str else str in
    Fennec_hunt_ppx_rules.expand_doctests (componentize (inject_params (mapper#structure (desugar_blocks str))))
(* register the MLX transform (whole-structure) + the inline test rules (context-free) in
   ONE driver, so a library using (pps fennec.fur.ppx) pays ONE ppx process for both *)
(* ONE driver for the whole component pipeline: MLX/JSX sugar (impl) + inline tests (hunt rules) +
   the typed-collection deriver & [%fields] projections (collection rules). [(pps fennec.fur.ppx)]
   alone gives all of it — no extra pps entry, no extra ppx subprocess. *)
(* force the [model] + [collection] derivers to register (they auto-register on load) — all three sift
   DX modules live in fennec.pulse.sift.ppx.rules now *)
let () = ignore Sift_ppx_rules.deriver_model
let () = ignore Collection_deriver.deriver

let () = Driver.register_transformation "iso_jsx" ~impl
    ~rules:(Fennec_hunt_ppx_rules.rules @ Sift_mongo_ppx_rules.rules)
