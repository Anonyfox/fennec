(* Doc-coverage for `fennec docs`. OCaml has no missing-docs lint (Rust's [missing_docs] has no
   equivalent; warning 70 only flags a missing .mli FILE, not undocumented items) — so this is
   fennec's. It parses an .mli (the public surface) — or, with [~private_], a .ml (top-level
   definitions) — via ppxlib and reports every export lacking a doc comment (an [@@ocaml.doc]
   attribute, i.e. a [(** ... *)] before the item). Extraction is pure (unit-tested); only [run]
   touches the filesystem. *)

open Ppxlib

type item = { kind : string; name : string; line : int; documented : bool }

let has_doc attrs = List.exists (fun (a : attribute) -> a.attr_name.txt = "ocaml.doc") attrs
let line (loc : Location.t) = loc.loc_start.pos_lnum

(* documentable exports of a parsed interface (.mli) *)
let interface_items (sg : signature) : item list =
  List.concat_map
    (fun (it : signature_item) ->
      match it.psig_desc with
      | Psig_value vd ->
        [ { kind = "val"; name = vd.pval_name.txt; line = line vd.pval_loc; documented = has_doc vd.pval_attributes } ]
      | Psig_type (_, tds) | Psig_typesubst tds ->
        List.map (fun (td : type_declaration) -> { kind = "type"; name = td.ptype_name.txt; line = line td.ptype_loc; documented = has_doc td.ptype_attributes }) tds
      | Psig_exception te ->
        [ { kind = "exception"; name = te.ptyexn_constructor.pext_name.txt; line = line te.ptyexn_loc; documented = has_doc te.ptyexn_attributes } ]
      | Psig_module md ->
        [ { kind = "module"; name = Option.value md.pmd_name.txt ~default:"_"; line = line md.pmd_loc; documented = has_doc md.pmd_attributes } ]
      | Psig_modtype mtd ->
        [ { kind = "module type"; name = mtd.pmtd_name.txt; line = line mtd.pmtd_loc; documented = has_doc mtd.pmtd_attributes } ]
      | _ -> [])
    sg

(* top-level definitions of a parsed implementation (.ml) — for --private *)
let implementation_items (st : structure) : item list =
  List.concat_map
    (fun (it : structure_item) ->
      match it.pstr_desc with
      | Pstr_value (_, vbs) ->
        List.filter_map
          (fun (vb : value_binding) ->
            match vb.pvb_pat.ppat_desc with
            | Ppat_var n -> Some { kind = "let"; name = n.txt; line = line vb.pvb_loc; documented = has_doc vb.pvb_attributes }
            | _ -> None)
          vbs
      | Pstr_type (_, tds) ->
        List.map (fun (td : type_declaration) -> { kind = "type"; name = td.ptype_name.txt; line = line td.ptype_loc; documented = has_doc td.ptype_attributes }) tds
      | Pstr_exception te ->
        [ { kind = "exception"; name = te.ptyexn_constructor.pext_name.txt; line = line te.ptyexn_loc; documented = has_doc te.ptyexn_attributes } ]
      | _ -> [])
    st

(* parse one file → its documentable items; a parse error (e.g. an .mlx with JSX the plain parser
   rejects) yields [] — skip the file, never crash the whole run *)
let items_of_file path : item list =
  let src = try In_channel.with_open_bin path In_channel.input_all with _ -> "" in
  let lexbuf = Lexing.from_string src in
  Lexing.set_filename lexbuf path;
  try
    if Filename.check_suffix path ".mli" then interface_items (Parse.interface lexbuf)
    else implementation_items (Parse.implementation lexbuf)
  with _ -> []

(* ── tests (pure extraction) ── *)

let parse_sig s = interface_items (Parse.interface (Lexing.from_string s))

let%test "documented val detected" =
  match parse_sig "(** doc *)\nval foo : int -> int" with [ i ] -> i.documented && i.name = "foo" && i.kind = "val" | _ -> false
let%test "undocumented val detected" =
  match parse_sig "val bar : int" with [ i ] -> (not i.documented) && i.name = "bar" | _ -> false
let%test "type, module, exception kinds" =
  let items = parse_sig "(** d *)\ntype t = int\nval v : t\nmodule M : sig end\nexception E" in
  List.length items = 4 && List.exists (fun i -> i.kind = "module" && i.name = "M") items
let%test "private: .ml top-level let" =
  match implementation_items (Parse.implementation (Lexing.from_string "let helper x = x")) with
  | [ i ] -> i.kind = "let" && i.name = "helper" && not i.documented
  | _ -> false

(* ── filesystem walk + report (the only impure part) ── *)

let tty = lazy (try Unix.isatty Unix.stdout with _ -> false)
let c code s = if Lazy.force tty then Printf.sprintf "\027[%sm%s\027[0m" code s else s

(* recursively collect files with one of [exts] under [dir], skipping build/vcs/hidden dirs *)
let rec collect exts acc dir =
  match Sys.readdir dir with
  | exception _ -> acc
  | entries ->
    Array.fold_left
      (fun acc e ->
        if e = "_build" || e = "node_modules" || (String.length e > 0 && e.[0] = '.') then acc
        else
          let p = Filename.concat dir e in
          if (try Sys.is_directory p with _ -> false) then collect exts acc p
          else if List.exists (fun ext -> Filename.check_suffix p ext) exts then p :: acc
          else acc)
      acc entries

let strip_dot p = if String.length p > 2 && p.[0] = '.' && p.[1] = '/' then String.sub p 2 (String.length p - 2) else p

let run ~(paths : string list) ~strict ~private_ : int =
  let roots = match paths with [] -> [ "." ] | ps -> ps in
  let exts = if private_ then [ ".mli"; ".ml" ] else [ ".mli" ] in
  let files =
    List.concat_map
      (fun r ->
        if (try Sys.is_directory r with _ -> false) then collect exts [] r
        else if List.exists (fun e -> Filename.check_suffix r e) exts then [ r ]
        else [])
      roots
    |> List.sort_uniq compare
  in
  let total = ref 0 and undoc = ref 0 in
  List.iter
    (fun f ->
      let items = items_of_file f in
      total := !total + List.length items;
      match List.filter (fun it -> not it.documented) items with
      | [] -> ()
      | missing ->
        Printf.printf "\n%s\n%!" (c "1" (strip_dot f));
        List.iter
          (fun it ->
            incr undoc;
            Printf.printf "  %s %s  %s %s\n%!" (c "33" "\u{2022}")
              (c "2" (Printf.sprintf "%s:%d" (Filename.basename f) it.line)) it.kind (c "1" it.name))
          (List.sort (fun a b -> compare a.line b.line) missing))
    files;
  if !undoc = 0 then (
    Printf.printf "%s %d public export%s documented\n%!" (c "1;32" "\u{2714}") !total (if !total = 1 then "" else "s");
    0)
  else (
    Printf.printf "\n%s %d of %d public export%s undocumented%s\n%!"
      (if strict then c "1;31" "\u{2717}" else c "1;33" "\u{26a0}")
      !undoc !total (if !total = 1 then "" else "s")
      (if strict then "" else c "2" "  (--strict to fail; --private to include .ml)");
    if strict then 1 else 0)
