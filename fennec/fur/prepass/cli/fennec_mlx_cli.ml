(* ════════════════════════════════════════════════════════════════════════════════════════
   Fennec_mlx_cli — the `.mlx` dialect preprocessor, IN-PROCESS over the vendored mlx parser.

   This is the engine behind `fennec mlx-pp <input-file>` (registered in cli/fennec.ml). Dune's
   dialect runs `%{bin:fennec} mlx-pp %{input-file}` on every `.mlx`, expecting a binary AST on
   stdout — the exact contract mlx-pp had. We now satisfy it with NO external process and NO
   external `mlx` opam package: mlx's parser is vendored as the {!Fennec_mlx} library
   (fennec/fur/prepass/vendor/), so we parse the file directly in-process.

   ── the pipeline (mirrors mlx-pp's pp.ml, with the fennec pre-pass + location remap folded in) ──
     1. read the .mlx source;
     2. {!Fennec_mlx_prepass.transform_with_map} — the fennec PRE-PASS: quote bare JSX text + map
        `{expr}`→`(expr)`, returning the transformed source AND a position map (Posmap) that undoes
        the byte shift quoting introduces;
     3. {!Fennec_mlx.Parse.implementation} on the transformed source (lexbuf filename = the input
        path) — the vendored parser, mlx's SAME source, so the AST is byte-identical to stock mlx;
     4. ppxlib's [Convert] from the vendored [OCaml_501] AST to the running compiler's AST — exactly
        what pp.ml does (it then marshals the result);
     5. {!rewrite_locations} — remap every location through the Posmap back to the ORIGINAL .mlx
        (column- AND line-exact), and stamp the original filename;
     6. marshal: [ast_impl_magic_number] ++ [output_value input_file] ++ [output_value structure] —
        byte-for-byte the on-disk format mlx-pp emitted (and the OLD fennec-mlx-pp re-emitted).

   On a parse error we mirror pp.ml's handling ([Location.error_of_exn]), but first REMAP the error's
   location through the Posmap so it points at the real .mlx line/col (not the quoted/collapsed
   transformed buffer), print the OCaml-standard `File "…", line, col` report, and exit 1.

   ── why the marshalled bytes match the OLD driver exactly ──
   The old fennec-mlx-pp read mlx-pp's stdout (which mlx-pp wrote with
   [Ppxlib_ast.Compiler_version.Ast.Config.ast_impl_magic_number] + two [output_value]s), unmarshalled
   the [Parsetree.structure], remapped locations with the SAME [rewrite_locations], and re-emitted with
   compiler-libs' [Config.ast_impl_magic_number]. Here we PRODUCE that same [Parsetree.structure]
   directly ([Conv.copy_structure] yields the running compiler's AST, which IS compiler-libs'
   [Parsetree]) and marshal it identically. Same magic, same value, same layout ⇒ identical bytes. *)

module Conv =
  Ppxlib_ast.Convert (Ppxlib_ast__Versions.OCaml_501) (Ppxlib_ast.Compiler_version)

let read_all_in ic =
  let bufsz = 65536 in
  let buf = Buffer.create bufsz in
  let chunk = Bytes.create bufsz in
  let rec loop () =
    let n = input ic chunk 0 bufsz in
    if n > 0 then (Buffer.add_subbytes buf chunk 0 n; loop ())
  in
  loop ();
  Buffer.contents buf

let read_file path =
  let ic = open_in_bin path in
  Fun.protect ~finally:(fun () -> close_in_noerr ic) (fun () -> read_all_in ic)

(* Remap a parsetree structure's locations: every [pos_fname] becomes [fname], and every position is
   remapped through [pm] back to the ORIGINAL .mlx source (column- AND line-exact). When [pm] is the
   identity (no bare-text run was rewritten) only the filename is swapped. Uses compiler-libs'
   Ast_mapper, whose default traversal visits all locations. Identical to the old driver's function. *)
let rewrite_locations (fname : string) (pm : Fennec_mlx_prepass.Posmap.t)
    (str : Parsetree.structure) : Parsetree.structure =
  let open Ast_mapper in
  let identity = Fennec_mlx_prepass.Posmap.is_identity pm in
  let map_pos (p : Lexing.position) =
    (* Leave GHOST / dummy positions (Lexing.dummy_pos, pos_cnum < 0 — carried by synthesized
       Location.none nodes the JSX desugar inserts) byte-for-byte: swap only the filename, never the
       offset/line. Only REAL source positions are remapped through the pre-pass position map. *)
    let p =
      if identity || p.Lexing.pos_cnum < 0 then p
      else Fennec_mlx_prepass.Posmap.remap_pos pm p
    in
    { p with Lexing.pos_fname = fname }
  in
  let map_loc (l : Location.t) =
    { l with Location.loc_start = map_pos l.loc_start; loc_end = map_pos l.loc_end }
  in
  let mapper = { default_mapper with location = (fun _ l -> map_loc l) } in
  mapper.structure mapper str

(* Print a parse error remapped to the original .mlx, then exit 1 — pp.ml's error path, with the
   Posmap remap added so the location points at the real source, not the transformed buffer. The
   error carries a Location.t (via Location.error_of_exn); we run its main + sub locations through
   [pm] before reporting. Falls back to re-raising if the exn is not a recognised parse error. *)
let report_error_and_exit (pm : Fennec_mlx_prepass.Posmap.t) (fname : string) (exn : exn) : 'a =
  match Location.error_of_exn exn with
  | None | Some `Already_displayed -> raise exn
  | Some (`Ok report) ->
    let identity = Fennec_mlx_prepass.Posmap.is_identity pm in
    let map_pos (p : Lexing.position) =
      let p =
        if identity || p.Lexing.pos_cnum < 0 then p
        else Fennec_mlx_prepass.Posmap.remap_pos pm p
      in
      { p with Lexing.pos_fname = fname }
    in
    let map_loc (l : Location.t) =
      { l with Location.loc_start = map_pos l.loc_start; loc_end = map_pos l.loc_end }
    in
    let map_msg (m : Location.msg) = { m with Location.loc = map_loc m.loc } in
    let report =
      { report with
        Location.main = map_msg report.Location.main;
        sub = List.map map_msg report.Location.sub }
    in
    Format.eprintf "%a@." Location.print_report report;
    exit 1

(* The whole job: read [input_file], pre-pass + parse in-process, remap locations, and write the
   binary AST to stdout in mlx-pp's exact on-disk format. Used by `fennec mlx-pp`. *)
let preprocess_to_stdout (input_file : string) : unit =
  let src = read_file input_file in
  let pre, pm = Fennec_mlx_prepass.transform_with_map src in
  let lexbuf = Lexing.from_string pre in
  Lexing.set_filename lexbuf input_file;
  let structure =
    match Fennec_mlx.Parse.implementation lexbuf with
    | str -> Conv.copy_structure str
    | exception exn -> report_error_and_exit pm input_file exn
  in
  (* FAST PATH — the pre-pass changed nothing ([pre = src]: every pure-OCaml .mlx, and any already in
     the quoted + paren form). The lexbuf filename we set IS the input path, so the parsed locations
     are already correct: skip [rewrite_locations] entirely. This mirrors the OLD driver's fast path
     (which `execv`'d mlx-pp on the ORIGINAL file) — same parser source, same Conv, same filename — so
     the marshalled bytes are BYTE-IDENTICAL, and it saves a whole-tree Ast_mapper pass.
     REWRITE PATH — the pre-pass altered bytes ([pre <> src]): run [rewrite_locations] to stamp the
     original filename and (when the posmap is non-identity) remap every location column/line-exact
     back to the .mlx. We gate on [pre = src], NOT [Posmap.is_identity pm]: a pre-pass that only drops
     whitespace-only child runs or maps `{`→`(` shifts bytes WITHOUT producing a replaced prose run
     (so the posmap is still identity), yet the OLD driver took its rewrite path there and reconstructed
     the tree — matching that reconstruction is what keeps those files byte-identical too. *)
  let structure =
    if String.equal pre src then structure
    else rewrite_locations input_file pm structure
  in
  set_binary_mode_out stdout true;
  output_string stdout Config.ast_impl_magic_number;
  output_value stdout input_file;
  output_value stdout structure

(* CLI entry: `fennec mlx-pp <input-file>`. Lenient like the old driver — the dialect always passes
   exactly one arg (the input file); we take the last arg. Returns a process exit code. *)
let main (argv : string array) : int =
  let n = Array.length argv in
  if n < 2 then (prerr_endline "fennec mlx-pp: expected an input file"; 2)
  else (preprocess_to_stdout argv.(n - 1); 0)
