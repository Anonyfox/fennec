(* Regression test for the Unicode lint: it must raise on an author-written plain
   non-ASCII string literal and stay silent on ASCII and on ghost-located nodes
   (the synthetic strings other ppxes produce). The example build proves it fires
   in situ; this guards against the lint silently going no-op. *)

open Ppxlib

let pos fname = { Lexing.pos_fname = fname; pos_lnum = 1; pos_bol = 0; pos_cnum = 0 }
let real_loc = { Location.loc_start = pos "t.ml"; loc_end = pos "t.ml"; loc_ghost = false }
let ghost_loc = { real_loc with loc_ghost = true }

let raises f = try f (); false with _ -> true

let str ~loc s = { (Ast_builder.Default.estring ~loc s) with pexp_loc = loc }

let fails = ref 0
let check name c = if c then Printf.printf "  ok   %s\n" name else (incr fails; Printf.printf "  FAIL %s\n" name)

let () =
  check "non-ASCII plain literal (real loc) -> error"
    (raises (fun () -> Fennec_unicode_ppx.linter#expression (str ~loc:real_loc "café 🦊")));
  check "ASCII plain literal -> silent"
    (not (raises (fun () -> Fennec_unicode_ppx.linter#expression (str ~loc:real_loc "plain ascii"))));
  check "non-ASCII at ghost loc (ppx-synthesised) -> silent"
    (not (raises (fun () -> Fennec_unicode_ppx.linter#expression (str ~loc:ghost_loc "café"))));
  if !fails = 0 then print_endline "all unicode-ppx lint tests passed."
  else (Printf.printf "%d FAILED\n" !fails; exit 1)
