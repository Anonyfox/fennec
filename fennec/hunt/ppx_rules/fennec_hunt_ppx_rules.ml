(* The test ppx rewriter rules — a plain ppxlib library, NOT a driver. Both the standalone
   [fennec-hunt.ppx] and the fur ppx ([fennec.fur.ppx]) import this and register the same rules
   in their own [Driver.register_transformation], so downstream pays ONE ppx process no
   matter which ppx they list in [(pps ...)].

   Supports:
     let%test "name" = <bool expr>          — fails if false
     let%test_unit "name" = <unit expr>     — fails if raises

   The bodies expand to [Fennec_hunt.Fennec_hunt_unit.test_loc] / [test_unit_loc] calls (registration
   as a module-init side effect, exactly like Http's [hunt] or Live's [test]).

   PRODUCTION STRIP: when the ppx argument [-fennec-drop-tests] is present (passed by the
   build system for the normal library variant, NOT for the test runner variant), every
   [let%test] / [let%test_unit] body is dropped to [let () = ()]. Zero cost in production:
   no closures, no strings, no registration calls, no [Fennec_hunt.Unit] symbols linked. *)

open Ppxlib

(* ══════════════════════════════════════════════════════════════════════════════════════ *)
(*  Strip gate                                                                           *)
(* ══════════════════════════════════════════════════════════════════════════════════════ *)

let drop_tests = ref false
let () = Driver.add_arg "-fennec-drop-tests"
  (Arg.Set drop_tests) ~doc:" Strip inline test bodies (zero cost in production)"

(* ══════════════════════════════════════════════════════════════════════════════════════ *)
(*  Helpers                                                                              *)
(* ══════════════════════════════════════════════════════════════════════════════════════ *)

let noop ~loc = [%stri let () = ()]

let loc_file (loc : Location.t) = loc.loc_start.Lexing.pos_fname
let loc_line (loc : Location.t) = loc.loc_start.Lexing.pos_lnum

(* extract the test name from the pattern: let%test "name" = ... → the string constant *)
let extract_name (vb : value_binding) : string option =
  match vb.pvb_pat.ppat_desc with
  | Ppat_constant (Pconst_string (name, _, _)) -> Some name
  | _ -> None

(* ══════════════════════════════════════════════════════════════════════════════════════ *)
(*  let%test "name" = <bool>                                                             *)
(* ══════════════════════════════════════════════════════════════════════════════════════ *)

let expand_test ~ctxt payload =
  let loc = Expansion_context.Extension.extension_point_loc ctxt in
  if !drop_tests then noop ~loc
  else
    match payload with
    | PStr [ { pstr_desc = Pstr_value (_, [ vb ]); _ } ] ->
      (match extract_name vb with
       | Some name ->
         let expr = vb.pvb_expr in
         let file = Ast_builder.Default.estring ~loc (loc_file loc) in
         let line = Ast_builder.Default.eint ~loc (loc_line loc) in
         let ename = Ast_builder.Default.estring ~loc name in
         (* emit the SHORT path (Fennec_hunt_unit.test_loc) so the expansion works both INSIDE the
            fennec-hunt library (where Unit is a sibling module) AND outside (where the
            user has `open Fennec_hunt` or the library is unwrapped). The fully-qualified
            Fennec_hunt.Fennec_hunt_unit.test_loc would create a cycle inside the library. *)
         [%stri let () =
           Fennec_hunt_unit.test_loc ~name:[%e ename] ~file:[%e file] ~line:[%e line]
             (fun () -> [%e expr])]
       | None ->
         Location.raise_errorf ~loc "let%%test requires a string literal name: let%%test \"name\" = ...")
    | _ ->
      Location.raise_errorf ~loc "let%%test requires: let%%test \"name\" = <bool expression>"

let ext_test =
  Extension.V3.declare_inline "test"
    Extension.Context.structure_item
    Ast_pattern.(__)
    (fun ~ctxt payload -> [ expand_test ~ctxt payload ])

(* ══════════════════════════════════════════════════════════════════════════════════════ *)
(*  let%test_unit "name" = <unit>                                                        *)
(* ══════════════════════════════════════════════════════════════════════════════════════ *)

let expand_test_unit ~ctxt payload =
  let loc = Expansion_context.Extension.extension_point_loc ctxt in
  if !drop_tests then noop ~loc
  else
    match payload with
    | PStr [ { pstr_desc = Pstr_value (_, [ vb ]); _ } ] ->
      (match extract_name vb with
       | Some name ->
         let expr = vb.pvb_expr in
         let file = Ast_builder.Default.estring ~loc (loc_file loc) in
         let line = Ast_builder.Default.eint ~loc (loc_line loc) in
         let ename = Ast_builder.Default.estring ~loc name in
         [%stri let () =
           Fennec_hunt_unit.test_unit_loc ~name:[%e ename] ~file:[%e file] ~line:[%e line]
             (fun () -> [%e expr])]
       | None ->
         Location.raise_errorf ~loc "let%%test_unit requires a string literal name: let%%test_unit \"name\" = ...")
    | _ ->
      Location.raise_errorf ~loc "let%%test_unit requires: let%%test_unit \"name\" = <unit expression>"

let ext_test_unit =
  Extension.V3.declare_inline "test_unit"
    Extension.Context.structure_item
    Ast_pattern.(__)
    (fun ~ctxt payload -> [ expand_test_unit ~ctxt payload ])

(* ══════════════════════════════════════════════════════════════════════════════════════ *)
(*  let%system "name" = <fun sandbox -> unit>   (System cut; _manual variant = opt-in)      *)
(* ══════════════════════════════════════════════════════════════════════════════════════ *)

(* The RHS is the scenario function itself ([fun sb -> …]); we register it with its source
   location. Fully qualified [Fennec_hunt.System] so it works under any opens (suites are
   always downstream of fennec-hunt). Stripped to [()] in production like every test. *)
let expand_system ~manual ~ctxt payload =
  let loc = Expansion_context.Extension.extension_point_loc ctxt in
  if !drop_tests then noop ~loc
  else
    match payload with
    | PStr [ { pstr_desc = Pstr_value (_, [ vb ]); _ } ] ->
      (match extract_name vb with
       | Some name ->
         let expr = vb.pvb_expr in
         let file = Ast_builder.Default.estring ~loc (loc_file loc) in
         let line = Ast_builder.Default.eint ~loc (loc_line loc) in
         let ename = Ast_builder.Default.estring ~loc name in
         let fn = if manual then [%expr Fennec_hunt.System.test_manual_loc] else [%expr Fennec_hunt.System.test_loc] in
         [%stri let () = [%e fn] ~name:[%e ename] ~file:[%e file] ~line:[%e line] [%e expr]]
       | None ->
         Location.raise_errorf ~loc "let%%system requires a string literal name: let%%system \"name\" = fun sb -> ...")
    | _ ->
      Location.raise_errorf ~loc "let%%system requires: let%%system \"name\" = fun sb -> <body>"

let ext_system =
  Extension.V3.declare_inline "system"
    Extension.Context.structure_item Ast_pattern.(__)
    (fun ~ctxt payload -> [ expand_system ~manual:false ~ctxt payload ])

let ext_system_manual =
  Extension.V3.declare_inline "system_manual"
    Extension.Context.structure_item Ast_pattern.(__)
    (fun ~ctxt payload -> [ expand_system ~manual:true ~ctxt payload ])

(* ══════════════════════════════════════════════════════════════════════════════════════ *)
(*  Exported rules (for any driver to register)                                          *)
(* ══════════════════════════════════════════════════════════════════════════════════════ *)

let rules = [
  Context_free.Rule.extension ext_test;
  Context_free.Rule.extension ext_test_unit;
  Context_free.Rule.extension ext_system;
  Context_free.Rule.extension ext_system_manual;
]
