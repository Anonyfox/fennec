(* The reactions ppx — makes a [@workflow] function read like an ordinary function while the
   transaction + reactions stay transparent. It is pure sugar over {!Fennec_pulse_workflow}.

     let[@workflow] f arg = body          ⇒   f runs in one transaction, with its before/after hooks
     let[@after  f] hook x   = …          ⇒   let () = __after__f  hook   (hook fires post-commit)
     let[@before f] guard x  = …          ⇒   let () = __before__f guard  (guard runs in the tx; raise = veto)
     let[@cron "expr"] job () = …         ⇒   let () = Schedule.cron  "expr" ~name:"job" job
     let[@every secs]  job () = …         ⇒   let () = Schedule.every  secs   ~name:"job" job

   THE TYPED COMPANION TRICK. For each [@workflow] f, the ppx emits two hook lists and a wrapped
   function that USES them — so OCaml *infers* the lists' element types from f's argument and result.
   It also emits typed registrars [__after__f] / [__before__f]. So [@after f] hook becomes a normal,
   type-checked call [__after__f hook] (hook must match f's result type) — no Obj, no stringly registry.
   And because that call references f's companion, a hook's module DEPENDS ON its target's module, so a
   cross-module reaction cycle is a module dependency cycle dune rejects; an intra-module cycle is caught
   here. [@cron]/[@every] type-force a [unit -> unit] job. *)

open Ppxlib

let workflow_attr = Attribute.declare "workflow" Attribute.Context.value_binding Ast_pattern.(pstr nil) ()
let after_attr = Attribute.declare "after" Attribute.Context.value_binding Ast_pattern.(single_expr_payload __) (fun e -> e)
let before_attr = Attribute.declare "before" Attribute.Context.value_binding Ast_pattern.(single_expr_payload __) (fun e -> e)
let cron_attr = Attribute.declare "cron" Attribute.Context.value_binding Ast_pattern.(single_expr_payload (estring __)) (fun s -> s)
let every_attr = Attribute.declare "every" Attribute.Context.value_binding Ast_pattern.(single_expr_payload __) (fun e -> e)

let consumed = [ "workflow"; "after"; "before"; "cron"; "every" ]

module B = Ast_builder.Default

(* the bound name of a binding ([let foo …] / [let (foo : t) …]), if it is a simple variable *)
let rec binding_name vb =
  match vb.pvb_pat.ppat_desc with
  | Ppat_var { txt; _ } -> Some txt
  | Ppat_constraint (p, _) -> binding_name { vb with pvb_pat = p }
  | _ -> None

(* the argument expression to hand the before-guards: the first parameter if it is a plain name,
   else [()] (a tuple/wildcard parameter just means the guards receive unit) *)
let rec arg_of_pat (p : pattern) : expression =
  let loc = p.ppat_loc in
  match p.ppat_desc with
  | Ppat_var { txt; _ } -> B.evar ~loc txt
  | Ppat_constraint (p', _) -> arg_of_pat p'
  | _ -> [%expr ()]

let arg_of_params loc = function
  | { pparam_desc = Pparam_val (_, _, pat); _ } :: _ -> arg_of_pat pat
  | _ -> [%expr ()]

(* a target written as a bare name ([@after f]) is local; [@after M.f] is external (its cycle, if any,
   is caught by the module-dependency-cycle rule, not here) *)
let local_target (e : expression) = match e.pexp_desc with Pexp_ident { txt = Lident n; _ } -> Some n | _ -> None

(* turn the target reference in [@after f] / [@after M.f] into its registrar [__after__f] / [M.__after__f] *)
let registrar_ident ~prefix loc (e : expression) : expression =
  let rename = function Lident n -> Lident (prefix ^ n) | Ldot (p, n) -> Ldot (p, prefix ^ n) | l -> l in
  match e.pexp_desc with
  | Pexp_ident { txt; _ } -> B.pexp_ident ~loc { txt = rename txt; loc }
  | _ -> Location.raise_errorf ~loc "fennec reactions: @after/@before expects a workflow name (e.g. @after place)"

let warn_off_32 ~loc =
  { attr_name = { txt = "warning"; loc }; attr_payload = PStr [ B.pstr_eval ~loc (B.estring ~loc "-32") [] ]; attr_loc = loc }

let strip_vb vb =
  { vb with pvb_attributes = List.filter (fun a -> not (List.mem a.attr_name.txt consumed)) vb.pvb_attributes }

(* lower a [@workflow] binding: returns the wrapped binding + the companion items (the two hook-list
   refs to emit BEFORE it, and the two registrars to emit AFTER it) *)
let make_workflow vb : value_binding * structure * structure =
  let loc = vb.pvb_loc in
  let name =
    match binding_name vb with
    | Some n -> n
    | None -> Location.raise_errorf ~loc "fennec reactions: [@workflow] must be on a named function"
  in
  let befores_ref = "__" ^ name ^ "_befores" and afters_ref = "__" ^ name ^ "_afters" in
  match vb.pvb_expr.pexp_desc with
  | Pexp_function (params, constr, Pfunction_body body) ->
      let arg = arg_of_params loc params in
      let new_body =
        [%expr
          Fennec_pulse_workflow.Workflow.exec ~name:[%e B.estring ~loc name]
            ~befores:![%e B.evar ~loc befores_ref] ~afters:![%e B.evar ~loc afters_ref] [%e arg]
            (fun () -> [%e body])]
      in
      let wrapped = { vb with pvb_expr = { vb.pvb_expr with pexp_desc = Pexp_function (params, constr, Pfunction_body new_body) } } in
      let mk_ref n = B.pstr_value ~loc Nonrecursive [ B.value_binding ~loc ~pat:(B.pvar ~loc n) ~expr:[%expr ref []] ] in
      let mk_registrar fn list_ref =
        let expr = [%expr fun h -> [%e B.evar ~loc list_ref] := ![%e B.evar ~loc list_ref] @ [ h ]] in
        B.pstr_value ~loc Nonrecursive
          [ { (B.value_binding ~loc ~pat:(B.pvar ~loc fn) ~expr) with pvb_attributes = [ warn_off_32 ~loc ] } ]
      in
      ( wrapped,
        [ mk_ref befores_ref; mk_ref afters_ref ],
        [ mk_registrar ("__after__" ^ name) afters_ref; mk_registrar ("__before__" ^ name) befores_ref ] )
  | _ -> Location.raise_errorf ~loc:vb.pvb_expr.pexp_loc "fennec reactions: [@workflow] must be on a function (fun arg -> …)"

(* the registration emitted for a reaction / schedule binding (the binding itself is kept as-is) *)
let registration_for vb : structure_item option =
  let loc = vb.pvb_loc in
  let hook () =
    match binding_name vb with
    | Some n -> B.evar ~loc n
    | None -> Location.raise_errorf ~loc "fennec reactions: this annotation must be on a named binding"
  in
  let job_name () =
    match binding_name vb with
    | Some n -> B.estring ~loc n
    | None -> Location.raise_errorf ~loc "fennec reactions: @cron/@every must be on a named function"
  in
  match Attribute.get after_attr vb with
  | Some target -> Some [%stri let () = [%e registrar_ident ~prefix:"__after__" loc target] [%e hook ()]]
  | None -> (
      match Attribute.get before_attr vb with
      | Some target -> Some [%stri let () = [%e registrar_ident ~prefix:"__before__" loc target] [%e hook ()]]
      | None -> (
          match Attribute.get cron_attr vb with
          | Some expr -> Some [%stri let () = Fennec_pulse_workflow.Schedule.cron [%e B.estring ~loc expr] ~name:[%e job_name ()] [%e hook ()]]
          | None -> (
              match Attribute.get every_attr vb with
              | Some dur -> Some [%stri let () = Fennec_pulse_workflow.Schedule.every [%e dur] ~name:[%e job_name ()] [%e hook ()]]
              | None -> None)))

(* compile-time intra-module circuit-breaker: reject a cycle in the local @after/@before edge graph *)
let check_acyclic edges =
  let succ n = List.filter_map (fun (a, h, _) -> if String.equal a n then Some h else None) edges in
  let loc_of n = match List.find_opt (fun (_, h, _) -> String.equal h n) edges with Some (_, _, l) -> l | None -> Location.none in
  let nodes = List.sort_uniq String.compare (List.concat_map (fun (a, h, _) -> [ a; h ]) edges) in
  let rec dfs path n =
    if List.mem n path then
      Location.raise_errorf ~loc:(loc_of n)
        "fennec reactions: cyclic reaction chain %s — @after/@before edges must be acyclic"
        (String.concat " -> " (List.rev (n :: path)))
    else List.iter (dfs (n :: path)) (succ n)
  in
  List.iter (dfs []) nodes

let impl (str : structure) : structure =
  let edges = ref [] in
  let note vb target =
    match (local_target target, binding_name vb) with Some t, Some h -> edges := (t, h, vb.pvb_loc) :: !edges | _ -> ()
  in
  let result =
    List.concat_map
      (fun item ->
        match item.pstr_desc with
        | Pstr_value (rf, vbs) ->
            let pre = ref [] and post = ref [] in
            let vbs' =
              List.map
                (fun vb ->
                  (match Attribute.get ~mark_as_seen:false after_attr vb with Some t -> note vb t | None -> ());
                  (match Attribute.get ~mark_as_seen:false before_attr vb with Some t -> note vb t | None -> ());
                  let vb =
                    match Attribute.get workflow_attr vb with
                    | Some () ->
                        let wrapped, refs, registrars = make_workflow vb in
                        pre := !pre @ refs;
                        post := !post @ registrars;
                        wrapped
                    | None -> vb
                  in
                  (match registration_for vb with Some it -> post := !post @ [ it ] | None -> ());
                  strip_vb vb)
                vbs
            in
            !pre @ [ { item with pstr_desc = Pstr_value (rf, vbs') } ] @ !post
        | _ -> [ item ])
      str
  in
  check_acyclic !edges;
  result

let () = Driver.register_transformation "fennec_reactions" ~impl
