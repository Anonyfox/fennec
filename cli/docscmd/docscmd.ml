(* Doc-coverage for `fennec docs`. OCaml has no missing-docs lint (Rust's [missing_docs] has no
   equivalent; warning 70 only flags a missing .mli FILE, not undocumented items) — so this is
   fennec's. It parses .mli/.ml via ppxlib and reports exports lacking a doc comment.

   Because odoc renders the CURATED .mli (a doc in the .ml is invisible when an .mli exists), each
   .mli export is one of three states:
     - documented in the .mli            → ✔ (it renders)
     - bare in the .mli, documented in .ml → ⤷ "ml-only": won't render publicly; move it (or --port)
     - documented in neither             → ✗ undocumented
   [--port] copies each ml-only doc into the .mli where the .mli lacks one (idempotent; the .mli
   wins on conflict; the .ml is read but never modified) — the explicit, reviewable "port over".

   Extraction + classification + the port rewrite are pure (unit-tested); only the file walk and
   read/write touch the world. *)

open Ppxlib

type item = { kind : string; name : string; line : int; doc : string option }

(* the text of the first [@@ocaml.doc] attribute (the (** ... *) content), if any *)
let doc_text (attrs : attribute list) : string option =
  List.find_map
    (fun (a : attribute) ->
      if a.attr_name.txt <> "ocaml.doc" then None
      else
        match a.attr_payload with
        | PStr [ { pstr_desc = Pstr_eval ({ pexp_desc = Pexp_constant (Pconst_string (s, _, _)); _ }, _); _ } ] -> Some s
        | _ -> None)
    attrs

let line (loc : Location.t) = loc.loc_start.pos_lnum

(* a .mli [val]/[external] and a .ml [let]/[external] are the same export — normalize so they match *)
let norm_kind = function "val" | "let" | "external" -> "value" | k -> k

(* documentable exports of a parsed interface (.mli) *)
let interface_items (sg : signature) : item list =
  List.concat_map
    (fun (it : signature_item) ->
      match it.psig_desc with
      | Psig_value vd -> [ { kind = "val"; name = vd.pval_name.txt; line = line vd.pval_loc; doc = doc_text vd.pval_attributes } ]
      | Psig_type (_, tds) | Psig_typesubst tds ->
        List.map (fun (td : type_declaration) -> { kind = "type"; name = td.ptype_name.txt; line = line td.ptype_loc; doc = doc_text td.ptype_attributes }) tds
      | Psig_exception te -> [ { kind = "exception"; name = te.ptyexn_constructor.pext_name.txt; line = line te.ptyexn_loc; doc = doc_text te.ptyexn_attributes } ]
      | Psig_module md -> [ { kind = "module"; name = Option.value md.pmd_name.txt ~default:"_"; line = line md.pmd_loc; doc = doc_text md.pmd_attributes } ]
      | Psig_modtype mtd -> [ { kind = "module type"; name = mtd.pmtd_name.txt; line = line mtd.pmtd_loc; doc = doc_text mtd.pmtd_attributes } ]
      | _ -> [])
    sg

(* top-level definitions of a parsed implementation (.ml) — for --private and the .mli cross-ref *)
let implementation_items (st : structure) : item list =
  List.concat_map
    (fun (it : structure_item) ->
      match it.pstr_desc with
      | Pstr_value (_, vbs) ->
        List.filter_map
          (fun (vb : value_binding) ->
            match vb.pvb_pat.ppat_desc with
            | Ppat_var n -> Some { kind = "let"; name = n.txt; line = line vb.pvb_loc; doc = doc_text vb.pvb_attributes }
            | _ -> None)
          vbs
      | Pstr_primitive vd -> [ { kind = "external"; name = vd.pval_name.txt; line = line vd.pval_loc; doc = doc_text vd.pval_attributes } ]
      | Pstr_type (_, tds) -> List.map (fun (td : type_declaration) -> { kind = "type"; name = td.ptype_name.txt; line = line td.ptype_loc; doc = doc_text td.ptype_attributes }) tds
      | Pstr_exception te -> [ { kind = "exception"; name = te.ptyexn_constructor.pext_name.txt; line = line te.ptyexn_loc; doc = doc_text te.ptyexn_attributes } ]
      | Pstr_module mb -> [ { kind = "module"; name = Option.value mb.pmb_name.txt ~default:"_"; line = line mb.pmb_loc; doc = doc_text mb.pmb_attributes } ]
      | Pstr_modtype mtd -> [ { kind = "module type"; name = mtd.pmtd_name.txt; line = line mtd.pmtd_loc; doc = doc_text mtd.pmtd_attributes } ]
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

let documented = function { doc = Some _; _ } -> true | _ -> false

(* ── tests (pure extraction) ── *)

let parse_sig s = interface_items (Parse.interface (Lexing.from_string s))
let parse_impl s = implementation_items (Parse.implementation (Lexing.from_string s))

let%test "documented val carries its text" =
  match parse_sig "(** doc here *)\nval foo : int -> int" with [ i ] -> i.doc = Some " doc here " && i.name = "foo" && i.kind = "val" | _ -> false
let%test "undocumented val has no doc" =
  match parse_sig "val bar : int" with [ i ] -> i.doc = None && i.name = "bar" | _ -> false
let%test "type, module, exception kinds" =
  let items = parse_sig "(** d *)\ntype t = int\nval v : t\nmodule M : sig end\nexception E" in
  List.length items = 4 && List.exists (fun i -> i.kind = "module" && i.name = "M") items
let%test ".ml: let / external / module captured" =
  let ks = List.map (fun i -> i.kind) (parse_impl "let a = 1\nexternal b : int -> int = \"x\"\nmodule M = struct end") in
  ks = [ "let"; "external"; "module" ]
let%test "norm_kind unifies val/let/external" =
  norm_kind "val" = "value" && norm_kind "let" = "value" && norm_kind "external" = "value" && norm_kind "type" = "type"

(* ── 3-state classification ── *)

type status = Documented | Ml_only of string (* the .ml doc text, for --port *) | Undocumented

(* the sibling .ml of an .mli: foo.mli → foo.ml *)
let sibling_ml mli = Filename.chop_suffix mli ".mli" ^ ".ml"

(* (normalized kind, name) → doc text, for the DOCUMENTED top-level items of [ml_path] *)
let ml_doc_index ml_path : (string * string, string) Hashtbl.t =
  let tbl = Hashtbl.create 64 in
  if Sys.file_exists ml_path then
    List.iter (fun it -> match it.doc with Some t -> Hashtbl.replace tbl (norm_kind it.kind, it.name) t | None -> ()) (items_of_file ml_path);
  tbl

let classify ~ml_index (it : item) : status =
  match it.doc with
  | Some _ -> Documented
  | None -> (match Hashtbl.find_opt ml_index (norm_kind it.kind, it.name) with Some t -> Ml_only t | None -> Undocumented)

let%test "classify: documented in .mli" =
  classify ~ml_index:(Hashtbl.create 1) { kind = "val"; name = "f"; line = 1; doc = Some "x" } = Documented
let%test "classify: bare .mli but .ml documents it → ml-only" =
  let idx = Hashtbl.create 1 in Hashtbl.replace idx ("value", "f") "the doc";
  classify ~ml_index:idx { kind = "val"; name = "f"; line = 1; doc = None } = Ml_only "the doc"
let%test "classify: bare in both → undocumented" =
  classify ~ml_index:(Hashtbl.create 1) { kind = "val"; name = "f"; line = 1; doc = None } = Undocumented
let%test "classify: kind-aware (a documented .ml TYPE doesn't cover a bare .mli VAL of the same name)" =
  let idx = Hashtbl.create 1 in Hashtbl.replace idx ("type", "f") "type doc";
  classify ~ml_index:idx { kind = "val"; name = "f"; line = 1; doc = None } = Undocumented

(* ── the --port rewrite (pure core) ── *)

(* leading whitespace of [s] *)
let indent_of s =
  let n = String.length s in
  let i = ref 0 in
  while !i < n && (s.[!i] = ' ' || s.[!i] = '\t') do incr i done;
  String.sub s 0 !i

(* render a doc-comment for the .mli from the .ml's doc text, at [indent] *)
let render_doc ~indent text =
  let t = String.trim text in
  match String.split_on_char '\n' t with
  | [ one ] -> Printf.sprintf "%s(** %s *)" indent one
  | first :: rest -> Printf.sprintf "%s(** %s\n%s *)" indent first (String.concat "\n" (List.map (fun l -> Printf.sprintf "%s    %s" indent (String.trim l)) rest))
  | [] -> Printf.sprintf "%s(** *)" indent

let str_contains hay needle =
  let hn = String.length hay and nn = String.length needle in
  if nn = 0 then true else (let rec go i = if i + nn > hn then false else if String.sub hay i nn = needle then true else go (i + 1) in go 0)

(* insert a doc comment before each given 1-based line of [src]. [inserts] is (line, text). A text
   that would close the comment early is skipped. One pass, so line numbers never shift, and the
   rest of the file is byte-preserved. Pure. *)
let port_source (src : string) (inserts : (int * string) list) : string =
  let by_line = Hashtbl.create 16 in
  List.iter (fun (l, t) -> if not (str_contains t "*)") then Hashtbl.replace by_line l t) inserts;
  let lines = String.split_on_char '\n' src in
  let n = List.length lines in
  let buf = Buffer.create (String.length src + 256) in
  List.iteri
    (fun i line ->
      (match Hashtbl.find_opt by_line (i + 1) with
       | Some text -> Buffer.add_string buf (render_doc ~indent:(indent_of line) text); Buffer.add_char buf '\n'
       | None -> ());
      Buffer.add_string buf line;
      if i < n - 1 then Buffer.add_char buf '\n')
    lines;
  Buffer.contents buf

let%test "port_source inserts before the line, keeping indentation" =
  port_source "  val f : int\n" [ (1, "f docs") ] = "  (** f docs *)\n  val f : int\n"
let%test "port_source multi-insert keeps line alignment (no shifting)" =
  let out = port_source "val a : int\nval b : int\n" [ (1, "da"); (2, "db") ] in
  out = "(** da *)\nval a : int\n(** db *)\nval b : int\n"
let%test "port_source skips a doc containing the comment terminator" =
  port_source "val f : int\n" [ (1, "has *) inside") ] = "val f : int\n"
let%test "render_doc multi-line" =
  render_doc ~indent:"" "first\nsecond" = "(** first\n    second *)"

(* ── filesystem walk + report / port (the impure part) ── *)

let tty = lazy (try Unix.isatty Unix.stdout with _ -> false)
let c code s = if Lazy.force tty then Printf.sprintf "\027[%sm%s\027[0m" code s else s

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

(* classify every export of a file: .mli cross-references its sibling .ml; a .ml (under --private)
   is just documented-or-not (it IS the public surface when there's no .mli) *)
let classify_file f : (item * status) list =
  let items = items_of_file f in
  if Filename.check_suffix f ".mli" then
    let idx = ml_doc_index (sibling_ml f) in
    List.map (fun it -> (it, classify ~ml_index:idx it)) items
  else List.map (fun it -> (it, if documented it then Documented else Undocumented)) items

let gather ~private_ paths =
  let roots = match paths with [] -> [ "." ] | ps -> ps in
  let exts = if private_ then [ ".mli"; ".ml" ] else [ ".mli" ] in
  List.concat_map
    (fun r ->
      if (try Sys.is_directory r with _ -> false) then collect exts [] r
      else if List.exists (fun e -> Filename.check_suffix r e) exts then [ r ] else [])
    roots
  |> List.sort_uniq compare

let do_report ~strict files : int =
  let total = ref 0 and undoc = ref 0 and mlonly = ref 0 in
  List.iter
    (fun f ->
      let classified = classify_file f in
      total := !total + List.length classified;
      match List.filter (fun (_, s) -> s <> Documented) classified with
      | [] -> ()
      | flagged ->
        Printf.printf "\n%s\n%!" (c "1" (strip_dot f));
        List.iter
          (fun (it, s) ->
            let loc = c "2" (Printf.sprintf "%s:%d" (Filename.basename f) it.line) in
            match s with
            | Documented -> ()
            | Undocumented -> incr undoc; Printf.printf "  %s %s  %s %s\n%!" (c "33" "\u{2022}") loc it.kind (c "1" it.name)
            | Ml_only _ ->
              incr mlonly;
              Printf.printf "  %s %s  %s %s  %s\n%!" (c "36" "\u{2937}") loc it.kind (c "1" it.name)
                (c "2" "documented in .ml — won't render; move to the .mli, or `fennec docs --port`"))
          (List.sort (fun (a, _) (b, _) -> compare a.line b.line) flagged))
    files;
  if !undoc = 0 && !mlonly = 0 then (
    Printf.printf "%s %d public export%s documented\n%!" (c "1;32" "\u{2714}") !total (if !total = 1 then "" else "s");
    0)
  else begin
    let parts =
      List.filter (fun s -> s <> "")
        [ (if !undoc > 0 then Printf.sprintf "%d undocumented" !undoc else "");
          (if !mlonly > 0 then Printf.sprintf "%d documented only in .ml" !mlonly else "") ]
    in
    Printf.printf "\n%s %s of %d public export%s%s\n%!"
      (if strict then c "1;31" "\u{2717}" else c "1;33" "\u{26a0}")
      (String.concat " + " parts) !total (if !total = 1 then "" else "s")
      (if strict then "" else c "2" "  (--strict to fail; --port to move .ml docs into the .mli)");
    if strict then 1 else 0
  end

let do_port files : int =
  let ported = ref 0 and touched = ref 0 in
  List.iter
    (fun f ->
      if Filename.check_suffix f ".mli" then begin
        let inserts =
          List.filter_map (fun (it, s) -> match s with Ml_only t -> Some (it.line, t) | _ -> None) (classify_file f)
        in
        if inserts <> [] then begin
          let src = try In_channel.with_open_bin f In_channel.input_all with _ -> "" in
          let out = port_source src inserts in
          if out <> src then begin
            (try Out_channel.with_open_bin f (fun oc -> Out_channel.output_string oc out) with _ -> ());
            incr touched;
            ported := !ported + List.length inserts;
            Printf.printf "  %s %s %s\n%!" (c "1;32" "\u{2192}") (c "1" (strip_dot f)) (c "2" (Printf.sprintf "(+%d doc%s)" (List.length inserts) (if List.length inserts = 1 then "" else "s")))
          end
        end
      end)
    files;
  if !ported = 0 then Printf.printf "%s nothing to port (no .ml-only docs found)\n%!" (c "1;32" "\u{2714}")
  else Printf.printf "\n%s ported %d doc%s into %d file%s — review the diff\n%!" (c "1;32" "\u{2714}") !ported (if !ported = 1 then "" else "s") !touched (if !touched = 1 then "" else "s");
  0

let run ~(paths : string list) ~strict ~private_ ~port : int =
  let files = gather ~private_:(private_ || port) paths in
  if port then do_port files else do_report ~strict files
