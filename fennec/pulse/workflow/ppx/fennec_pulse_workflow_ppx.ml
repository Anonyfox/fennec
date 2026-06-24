(* The reactions ppx — desugars the four lifecycle annotations on top-level let bindings into the
   plain runtime calls of {!Fennec_pulse_workflow} (Phase 2). It is pure sugar; everything it emits
   you could write by hand.

     let[@after  w] hook  x   = …   ⇒   let () = Workflow.after  w "…" hook   (after the binding)
     let[@before w] guard x   = …   ⇒   let () = Workflow.before w guard
     let[@cron "expr"] job () = …   ⇒   let () = Schedule.cron  "expr" ~name:"job" job
     let[@every secs]  job () = …   ⇒   let () = Schedule.every  secs   ~name:"job" job

   THE CIRCUIT-BREAKER. A reaction graph must be acyclic. The cross-module case is enforced for free:
   [@after w] emits a reference to [w], so a hook's module DEPENDS ON its target's module — a reaction
   cycle that crosses module boundaries is therefore a module dependency cycle, which dune/ocaml reject
   at build time. This ppx closes the remaining gap: an INTRA-module cycle (a hook and its target in the
   same file) is caught here as a compile error from the local @after/@before edge graph. So every cycle
   is a compile error, never a runtime infinite loop.

   [@cron]/[@every] type-force a no-input function: the emitted [Schedule.cron expr ~name job] requires
   [job : unit -> unit], so a job that takes an argument (or returns non-unit) is a type error. *)

open Ppxlib

(* the four annotations live on the value binding ([let[@after w] x = …]) *)
let after_attr = Attribute.declare "after" Attribute.Context.value_binding Ast_pattern.(single_expr_payload __) (fun e -> e)
let before_attr = Attribute.declare "before" Attribute.Context.value_binding Ast_pattern.(single_expr_payload __) (fun e -> e)
let cron_attr = Attribute.declare "cron" Attribute.Context.value_binding Ast_pattern.(single_expr_payload (estring __)) (fun s -> s)
let every_attr = Attribute.declare "every" Attribute.Context.value_binding Ast_pattern.(single_expr_payload __) (fun e -> e)

let consumed = [ "after"; "before"; "cron"; "every" ]

(* the bound name of a binding ([let foo …] / [let (foo : t) …]), if it is a simple variable *)
let rec binding_name vb =
  match vb.pvb_pat.ppat_desc with
  | Ppat_var { txt; _ } -> Some txt
  | Ppat_constraint (p, _) -> binding_name { vb with pvb_pat = p }
  | _ -> None

(* a target written as a BARE name ([@after place]) is local to this module; [@after Orders.place] is
   external (its cycle, if any, is caught by the module-dependency-cycle rule, not here) *)
let local_target (e : expression) = match e.pexp_desc with Pexp_ident { txt = Lident n; _ } -> Some n | _ -> None

(* strip the four attributes from a binding we have lowered (so nothing re-processes / warns on them) *)
let strip_vb vb =
  { vb with pvb_attributes = List.filter (fun a -> not (List.mem a.attr_name.txt consumed)) vb.pvb_attributes }

(* the registration emitted for one binding, if it carries one of the four annotations *)
let registration_for vb : structure_item option =
  let loc = vb.pvb_loc in
  let name_e () =
    match binding_name vb with
    | Some n -> Ast_builder.Default.evar ~loc n
    | None -> Location.raise_errorf ~loc "fennec reactions: this annotation must be on a named let binding"
  in
  let name_s () =
    match binding_name vb with
    | Some n -> Ast_builder.Default.estring ~loc n
    | None -> Location.raise_errorf ~loc "fennec reactions: @cron/@every must be on a named function"
  in
  match Attribute.get after_attr vb with
  | Some target -> Some [%stri let () = Fennec_pulse_workflow.Workflow.after [%e target] [%e name_e ()]]
  | None -> (
      match Attribute.get before_attr vb with
      | Some target -> Some [%stri let () = Fennec_pulse_workflow.Workflow.before [%e target] [%e name_e ()]]
      | None -> (
          match Attribute.get cron_attr vb with
          | Some expr ->
              let s = Ast_builder.Default.estring ~loc expr in
              Some [%stri let () = Fennec_pulse_workflow.Schedule.cron [%e s] ~name:[%e name_s ()] [%e name_e ()]]
          | None -> (
              match Attribute.get every_attr vb with
              | Some dur -> Some [%stri let () = Fennec_pulse_workflow.Schedule.every [%e dur] ~name:[%e name_s ()] [%e name_e ()]]
              | None -> None)))

(* compile-time intra-module circuit-breaker: reject a cycle in the local @after/@before edge graph *)
let check_acyclic edges =
  let succ n = List.filter_map (fun (a, b, _) -> if String.equal a n then Some b else None) edges in
  let loc_of n = match List.find_opt (fun (_, b, _) -> String.equal b n) edges with Some (_, _, l) -> l | None -> Location.none in
  let nodes = List.sort_uniq String.compare (List.concat_map (fun (a, b, _) -> [ a; b ]) edges) in
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
    match (local_target target, binding_name vb) with
    | Some t, Some h -> edges := (t, h, vb.pvb_loc) :: !edges
    | _ -> ()
  in
  let result =
    List.concat_map
      (fun item ->
        match item.pstr_desc with
        | Pstr_value (rf, vbs) ->
            let regs =
              List.filter_map
                (fun vb ->
                  (match Attribute.get ~mark_as_seen:false after_attr vb with Some t -> note vb t | None -> ());
                  (match Attribute.get ~mark_as_seen:false before_attr vb with Some t -> note vb t | None -> ());
                  registration_for vb)
                vbs
            in
            { item with pstr_desc = Pstr_value (rf, List.map strip_vb vbs) } :: regs
        | _ -> [ item ])
      str
  in
  check_acyclic !edges;
  result

let () = Driver.register_transformation "fennec_reactions" ~impl
