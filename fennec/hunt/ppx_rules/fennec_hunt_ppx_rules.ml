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
(*  let%browser "name" = <fun page -> unit>   (Browser cut)                                 *)
(* ══════════════════════════════════════════════════════════════════════════════════════ *)

(* The RHS is the test function ([fun page -> …]); register it with its source file (for
   [--only-file], so `fennec test browser` can run one suite file against its own instance). *)
let expand_browser ~ctxt payload =
  let loc = Expansion_context.Extension.extension_point_loc ctxt in
  if !drop_tests then noop ~loc
  else
    match payload with
    | PStr [ { pstr_desc = Pstr_value (_, [ vb ]); _ } ] ->
      (match extract_name vb with
       | Some name ->
         let expr = vb.pvb_expr in
         let file = Ast_builder.Default.estring ~loc (loc_file loc) in
         let ename = Ast_builder.Default.estring ~loc name in
         [%stri let () = Fennec_hunt.Live.test_loc ~name:[%e ename] ~file:[%e file] [%e expr]]
       | None ->
         Location.raise_errorf ~loc "let%%browser requires a string literal name: let%%browser \"name\" = fun page -> ...")
    | _ ->
      Location.raise_errorf ~loc "let%%browser requires: let%%browser \"name\" = fun page -> <body>"

let ext_browser =
  Extension.V3.declare_inline "browser"
    Extension.Context.structure_item Ast_pattern.(__)
    (fun ~ctxt payload -> [ expand_browser ~ctxt payload ])

(* ══════════════════════════════════════════════════════════════════════════════════════ *)
(*  let%http "name" = <fun () -> unit>   (Http cut)                                         *)
(* ══════════════════════════════════════════════════════════════════════════════════════ *)

(* The RHS is the suite body ([fun () -> check …; check …]); register it with its source file
   (for [--only-file]). The target URL is the harness-assigned FENNEC_TEST_URL at run time. *)
let expand_http ~ctxt payload =
  let loc = Expansion_context.Extension.extension_point_loc ctxt in
  if !drop_tests then noop ~loc
  else
    match payload with
    | PStr [ { pstr_desc = Pstr_value (_, [ vb ]); _ } ] ->
      (match extract_name vb with
       | Some name ->
         let expr = vb.pvb_expr in
         let file = Ast_builder.Default.estring ~loc (loc_file loc) in
         let ename = Ast_builder.Default.estring ~loc name in
         [%stri let () = Fennec_hunt.Http.hunt_loc ~name:[%e ename] ~file:[%e file] [%e expr]]
       | None ->
         Location.raise_errorf ~loc "let%%http requires a string literal name: let%%http \"name\" = fun () -> ...")
    | _ ->
      Location.raise_errorf ~loc "let%%http requires: let%%http \"name\" = fun () -> <checks>"

let ext_http =
  Extension.V3.declare_inline "http"
    Extension.Context.structure_item Ast_pattern.(__)
    (fun ~ctxt payload -> [ expand_http ~ctxt payload ])

(* ══════════════════════════════════════════════════════════════════════════════════════ *)
(*  Exported rules (for any driver to register)                                          *)
(* ══════════════════════════════════════════════════════════════════════════════════════ *)

let rules = [
  Context_free.Rule.extension ext_test;
  Context_free.Rule.extension ext_test_unit;
  Context_free.Rule.extension ext_system;
  Context_free.Rule.extension ext_system_manual;
  Context_free.Rule.extension ext_browser;
  Context_free.Rule.extension ext_http;
]

(* ══════════════════════════════════════════════════════════════════════════════════════ *)
(*  Doctests — executable {@ocaml[ … ]} blocks in doc comments (rustdoc-style)              *)
(* ══════════════════════════════════════════════════════════════════════════════════════ *)

(* An odoc code block tagged [ocaml] is treated as an executable example: it renders in the docs
   AND runs as a test (so examples cannot drift). Opt-in by the tag — a plain [{[ … ]}] block stays
   purely illustrative, and [{@ocaml skip[ … ]}] renders highlighted but does not run. Each block
   is compiled + run IN MODULE SCOPE (a local module, so multi-statement blocks work and the block
   sees the module's own definitions), registered like an inline test and stripped in production. *)

let str_contains hay needle =
  let hn = String.length hay and nn = String.length needle in
  if nn = 0 then true
  else
    let rec go i = if i + nn > hn then false else if String.sub hay i nn = needle then true else go (i + 1) in
    go 0

(* extract the bodies of executable [{@ocaml … [ … ]}] blocks from a doc string (skipping ones whose
   labels contain [skip]). Pure. *)
let code_blocks (doc : string) : string list =
  let n = String.length doc in
  let find sub start =
    let sl = String.length sub in
    let rec go j = if j + sl > n then -1 else if String.sub doc j sl = sub then j else go (j + 1) in
    go start
  in
  let blocks = ref [] and i = ref 0 and go = ref true in
  while !go do
    let tag = find "{@ocaml" !i in
    if tag < 0 then go := false
    else
      let lb = find "[" (tag + 7) in
      let rb = if lb < 0 then -1 else find "]}" (lb + 1) in
      if lb < 0 || rb < 0 then go := false
      else begin
        let labels = String.sub doc (tag + 7) (lb - (tag + 7)) in
        if not (str_contains labels "skip") then blocks := String.sub doc (lb + 1) (rb - lb - 1) :: !blocks;
        i := rb + 2
      end
  done;
  List.rev !blocks

(* the string payload of a doc-comment attribute, if any *)
let doc_string (a : attribute) : string option =
  match (a.attr_name.txt, a.attr_payload) with
  | ("ocaml.doc" | "ocaml.text"), PStr [ { pstr_desc = Pstr_eval ({ pexp_desc = Pexp_constant (Pconst_string (s, _, _)); _ }, _); _ } ] -> Some s
  | _ -> None

(* doc strings attached to a structure item (covers the common item kinds + a floating comment) *)
let item_doc_strings (item : structure_item) : string list =
  let of_attrs attrs = List.filter_map doc_string attrs in
  match item.pstr_desc with
  | Pstr_value (_, vbs) -> List.concat_map (fun (vb : value_binding) -> of_attrs vb.pvb_attributes) vbs
  | Pstr_type (_, tds) -> List.concat_map (fun (td : type_declaration) -> of_attrs td.ptype_attributes) tds
  | Pstr_primitive vd -> of_attrs vd.pval_attributes
  | Pstr_exception te -> of_attrs te.ptyexn_attributes
  | Pstr_module mb -> of_attrs mb.pmb_attributes
  | Pstr_attribute a -> (match doc_string a with Some s -> [ s ] | None -> [])
  | _ -> []

(* one doctest registration from a code block: parse it (a structure, else a bare expression wrapped
   as [let () = ignore …]), run it inside an anonymous local module so it sees the module's own
   definitions, and register it like an inline test. *)
let doctest_item ~loc ~code : structure_item option =
  (* point the block's locations near the doc comment in the real file, so a compile error or a
     failed [assert] inside the example reports a sensible file:line (not "line 1" of the block). *)
  let lexbuf () =
    let lb = Lexing.from_string code in
    Lexing.set_position lb { pos_fname = loc_file loc; pos_lnum = loc_line loc; pos_bol = 0; pos_cnum = 0 };
    Lexing.set_filename lb (loc_file loc);
    lb
  in
  let parse () = Ppxlib.Parse.implementation (lexbuf ()) in
  let parse_expr () = [ [%stri let () = ignore [%e Ppxlib.Parse.expression (lexbuf ())]] ] in
  match (try parse () with _ -> (try parse_expr () with _ -> [])) with
  | [] -> None
  | items ->
    let body = Ast_builder.Default.pexp_letmodule ~loc { txt = None; loc } (Ast_builder.Default.pmod_structure ~loc items) [%expr ()] in
    let name = Ast_builder.Default.estring ~loc (Printf.sprintf "doc example (%s:%d)" (Filename.basename (loc_file loc)) (loc_line loc)) in
    let file = Ast_builder.Default.estring ~loc (loc_file loc) in
    let line = Ast_builder.Default.eint ~loc (loc_line loc) in
    Some [%stri let () = Fennec_hunt_unit.test_unit_loc ~name:[%e name] ~file:[%e file] ~line:[%e line] (fun () -> [%e body])]

(* whole-structure pass: append a registration for every executable doc block. Appended at the END
   so a doctest sees the whole module. Stripped (emits nothing) in a production build. Registered by
   BOTH ppx drivers (the standalone test ppx and the fur ppx). *)
let expand_doctests (str : structure) : structure =
  if !drop_tests then str
  else
    str
    @ List.concat_map
        (fun (item : structure_item) ->
          let loc = item.pstr_loc in
          List.concat_map (fun doc -> List.filter_map (fun code -> doctest_item ~loc ~code) (code_blocks doc)) (item_doc_strings item))
        str
