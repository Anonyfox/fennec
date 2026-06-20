(* ════════════════════════════════════════════════════════════════════════════════════════
   PROOF + regression guard for the fennec MERLIN READER (ocamlmerlin-fennec-mlx).

   The reader is a `merlin-extend` external reader: Merlin spawns `ocamlmerlin-<name>` and talks the
   MERLINEXTEND002 protocol over stdin/stdout. This test plays MERLIN's driver side (via merlin-extend's
   own `Extend_driver`) and drives BOTH readers — the stock `ocamlmerlin-mlx` and our
   `ocamlmerlin-fennec-mlx` — over the real protocol, then asserts:

     1. BEFORE/AFTER on bare text: stock yields many `ocaml.error` nodes (the false in-editor syntax
        errors); fennec yields a CLEAN parsetree (0 error nodes). This is the whole point.
     2. FAITHFUL DELEGATION on already-quoted .mlx: the fennec reader's marshalled parsetree is
        BYTE-IDENTICAL to the stock reader's (the pre-pass is identity there, so delegation is exact).
     3. POSITION FIDELITY: on the bare-text fixture the fennec parsetree's locations stay within the
        buffer and the top-level binding lands on its real line (line-exact, per the line-preserving
        pre-pass).
     4. COLUMN-EXACT remap through the reader on a bare-text run + `{expr}` on one line.
     5. THE VENDORED-PARSE FALLBACK: with `ocamlmerlin-mlx` hidden from PATH, the fennec reader STILL
        returns a real, non-empty, error-free parsetree for bare text — the in-process vendored parser,
        not the old dead error node. This is the proof the editor works off `fennec-cli` alone.

   It is an EDITOR-tool test: it links `merlin-extend` to speak the protocol, so it is the
   merlin-extend-PRESENT branch of a dune `(select reader_delegation_impl.ml …)`; when merlin-extend is
   absent dune compiles the SKIP stub instead, so the BUILD never depends on merlin. At RUNTIME it also
   needs `ocamlmerlin-mlx` (the stock reader) on PATH; if that binary is absent (an editor-only opam dev
   dependency) the test SKIPS (exit 0). The reader-under-test (`ocamlmerlin-fennec-mlx`) is passed in by
   the caller (a dune dep) and put on PATH under its exact merlin-invocation name.
   ════════════════════════════════════════════════════════════════════════════════════════ *)

module P = Extend_protocol
module R = P.Reader

(* ── locate / stage the two reader binaries on PATH under their merlin names ──────────────────── *)

let on_path name = Sys.command (Printf.sprintf "command -v %s >/dev/null 2>&1" name) = 0

(* The fennec reader is handed to us by dune as a dep; its path is argv.(1). Merlin invokes a reader
   by the bare name `ocamlmerlin-<reader>`, so we symlink the built exe into a temp dir under exactly
   that name and prepend the dir to PATH. *)
let stage_fennec_reader exe_path =
  (* Resolve to an ABSOLUTE path: dune passes the reader exe as a RELATIVE dep path; a symlink to a
     relative target resolves against the SYMLINK's dir (the temp dir below), where it would not exist
     → ENOENT at exec. Absolutising fixes the staged symlink regardless of the spawner's cwd/PATH. *)
  let exe_path = if Filename.is_relative exe_path then Filename.concat (Sys.getcwd ()) exe_path else exe_path in
  let dir = Filename.temp_file "fennec_reader_bin_" "" in
  Sys.remove dir;
  Unix.mkdir dir 0o755;
  let link = Filename.concat dir "ocamlmerlin-fennec-mlx" in
  (try Unix.symlink exe_path link
   with Unix.Unix_error _ ->
     (* fall back to a copy if symlinks are unavailable *)
     let ic = open_in_bin exe_path and oc = open_out_bin link in
     (try while true do output_char oc (input_char ic) done with End_of_file -> ());
     close_in ic; close_out oc; Unix.chmod link 0o755);
  Unix.putenv "PATH" (dir ^ ":" ^ Sys.getenv "PATH");
  dir   (* return the staged dir so the fallback section can strip PATH down to JUST it *)

(* ── drive a reader over the protocol; return its parsetree (+ a marshalled-bytes view) ──────── *)

let count_errors (s : Parsetree.structure) =
  List.length
    (List.filter
       (fun (it : Parsetree.structure_item) ->
         match it.pstr_desc with
         | Pstr_eval ({ pexp_desc = Pexp_extension ({ txt; _ }, _); _ }, _) -> txt = "ocaml.error"
         | _ -> false)
       s)

(* Run Req_load (transform happens inside the reader) then Req_parse. Returns the structure. *)
let parse_with ~reader_name ~text : Parsetree.structure =
  let drv = Extend_driver.run reader_name in
  Fun.protect
    ~finally:(fun () -> Extend_driver.stop drv)
    (fun () ->
      (match Extend_driver.reader drv (R.Req_load { R.path = "/proof/index.mlx"; flags = []; text }) with
       | R.Res_loaded -> ()
       | _ -> failwith "expected Res_loaded");
      match Extend_driver.reader drv R.Req_parse with
      | R.Res_parse (R.Structure s) -> s
      | R.Res_parse (R.Signature _) -> failwith "unexpected Signature"
      | _ -> failwith "expected Res_parse")

(* deepest position seen anywhere in the structure's locations — for the bounds check *)
let max_line (s : Parsetree.structure) =
  let m = ref 0 in
  let module I = Ast_iterator in
  let it =
    { I.default_iterator with
      location = (fun _ (l : Location.t) -> if l.loc_end.pos_lnum > !m then m := l.loc_end.pos_lnum) }
  in
  List.iter (it.structure_item it) s;
  !m

(* ── the fixtures ─────────────────────────────────────────────────────────────────────────────
   BARE: the new fennec surface — bare prose children + {expr}. Stock mlx chokes on every word.
   QUOTED: the old/explicit form — quoted children + (expr). The pre-pass is identity here. *)

let bare =
  "let () = Head.title \"Home\"\n\
   let view =\n\
  \  <main className=\"page\">\n\
  \    <h1>Welcome to the Fennec site</h1>\n\
  \    <p>One component tree, server-rendered then hydrated.</p>\n\
  \    <Counter label=\"clicks\" />\n\
  \  </main>\n"

let quoted =
  "let () = Head.title \"Home\"\n\
   let view =\n\
  \  <main className=\"page\">\n\
  \    <h1>\"Welcome to the Fennec site\"</h1>\n\
  \    <p>\"One component tree, server-rendered then hydrated.\"</p>\n\
  \    <Counter label=\"clicks\" />\n\
  \  </main>\n"

let fail = ref 0
let check name cond = if not cond then (incr fail; Printf.printf "FAIL: %s\n" name)
                      else Printf.printf "ok:   %s\n" name

let run (fennec_reader_exe : string) =
  (* stage the reader-under-test on PATH under its exact merlin-invocation name; keep the staged dir *)
  let fennec_dir = stage_fennec_reader fennec_reader_exe in

  (* the stock reader must be present, else SKIP (editor-only dependency) *)
  if not (on_path "ocamlmerlin-mlx") then begin
    print_endline "reader_delegation: ocamlmerlin-mlx not on PATH — SKIPPED";
    exit 0
  end;

  (* merlin sets this for its readers; both our driver-spawn of the fennec reader AND that reader's
     own child-spawn of the stock reader require it. Set it once; it is inherited down the chain. *)
  Unix.putenv "__MERLIN_MASTER_PID" (string_of_int (Unix.getpid ()));

  (* 1. BEFORE/AFTER on bare text *)
  let stock_bare = parse_with ~reader_name:"mlx" ~text:bare in
  let fennec_bare = parse_with ~reader_name:"fennec-mlx" ~text:bare in
  let stock_errs = count_errors stock_bare and fennec_errs = count_errors fennec_bare in
  Printf.printf "  bare text: stock=%d items/%d errors, fennec=%d items/%d errors\n"
    (List.length stock_bare) stock_errs (List.length fennec_bare) fennec_errs;
  check "stock reader reports syntax errors on bare text (the status quo)" (stock_errs > 0);
  check "fennec reader parses bare text with ZERO errors" (fennec_errs = 0);
  check "fennec reader yields a non-empty parsetree for bare text" (List.length fennec_bare > 0);

  (* 2. FAITHFUL DELEGATION on already-quoted .mlx: byte-identical marshalled parsetrees *)
  let stock_q = parse_with ~reader_name:"mlx" ~text:quoted in
  let fennec_q = parse_with ~reader_name:"fennec-mlx" ~text:quoted in
  let stock_bytes = Marshal.to_string stock_q [] and fennec_bytes = Marshal.to_string fennec_q [] in
  check "quoted .mlx: stock parses cleanly" (count_errors stock_q = 0);
  check "quoted .mlx: fennec parses cleanly" (count_errors fennec_q = 0);
  check "quoted .mlx: fennec parsetree is BYTE-IDENTICAL to stock (faithful delegation)"
    (String.equal stock_bytes fennec_bytes);

  (* 3. POSITION FIDELITY on bare text: locations stay within the 7-line buffer; line-exact *)
  let nlines = List.length (String.split_on_char '\n' bare) in
  let m = max_line fennec_bare in
  Printf.printf "  bare text: buffer has %d lines; deepest fennec loc end-line = %d\n" nlines m;
  check "fennec locations stay within the buffer (no out-of-range lines)" (m <= nlines);
  check "fennec locations are real (the binding spans past line 1)" (m >= 2);

  (* 4. COLUMN-EXACT remap THROUGH THE READER. A bare-text run followed by `{expr}` on the SAME line
     shifts every later column in the transformed buffer (the added quotes), so WITHOUT the reader's
     position-map the identifier inside the `{…}` would be reported a couple of columns to the right —
     a hover/jump landing in the wrong place. Here we drive the fennec reader on such a buffer and
     assert it reports the identifier's column EXACTLY where it sits in the ORIGINAL source. *)
  let same_line = "let v = <div>todos in store: {List.length items}</div>" in
  let orig_col marker =                         (* 0-based column of [marker] in [same_line] *)
    let n = String.length same_line and m = String.length marker in
    let r = ref (-1) and i = ref 0 in
    while !r < 0 && !i <= n - m do (if String.sub same_line !i m = marker then r := !i); incr i done;
    !r in
  (* find the loc_start column of the value-identifier [name] anywhere in a parsed structure *)
  let col_of_ident (s : Parsetree.structure) (name : string) : int option =
    let found = ref None in
    let module I = Ast_iterator in
    let it = { I.default_iterator with
      expr = (fun self e ->
        (match e.pexp_desc with
         | Pexp_ident lid
           when !found = None
                && String.concat "." (Longident.flatten lid.Location.txt) = name ->
           let ls = e.pexp_loc.loc_start in found := Some (ls.pos_cnum - ls.pos_bol)
         | _ -> ());
        I.default_iterator.expr self e) } in
    List.iter (it.structure_item it) s;
    !found in
  let fennec_same = parse_with ~reader_name:"fennec-mlx" ~text:same_line in
  (match col_of_ident fennec_same "List.length", orig_col "List.length" with
   | Some got, want ->
     Printf.printf "  same-line: 'List.length' reported col=%d, original col=%d\n" got want;
     check "fennec reader remaps 'List.length' to its ORIGINAL column (not the byte-shifted one)"
       (got = want)
   | None, _ -> check "fennec reader found 'List.length' ident" false);

  (* 5. THE VENDORED-PARSE FALLBACK — the editor WORKS off fennec-cli ALONE (no ocamlmerlin-mlx).
     We hide the stock reader by running the fennec reader with a PATH that contains ONLY the fennec
     reader's own dir (so its child-spawn of `ocamlmerlin-mlx` fails → the in-process vendored parser
     takes over). We assert it still returns a REAL, non-empty parsetree with ZERO errors for the
     bare-text fixture — i.e. NOT the old dead "install ocamlmerlin-mlx" error node. (Saved PATH is
     restored after, so the earlier sections are unaffected — they ran already, but be tidy.) *)
  let saved_path = Sys.getenv "PATH" in
  (* PATH = ONLY the dir holding the fennec reader symlink, so `ocamlmerlin-mlx` is unreachable but the
     fennec reader itself is still spawnable (its symlink lives there). *)
  Unix.putenv "PATH" fennec_dir;
  (* sanity: with this PATH the stock reader must indeed be gone, else the test proves nothing *)
  check "fallback precondition: ocamlmerlin-mlx is NOT reachable on the stripped PATH"
    (not (on_path "ocamlmerlin-mlx"));
  let fennec_fallback = parse_with ~reader_name:"fennec-mlx" ~text:bare in
  Unix.putenv "PATH" saved_path;
  Printf.printf "  fallback (no ocamlmerlin-mlx): fennec=%d items/%d errors\n"
    (List.length fennec_fallback) (count_errors fennec_fallback);
  check "fallback: fennec reader returns a NON-EMPTY parsetree off fennec-cli alone"
    (List.length fennec_fallback > 0);
  check "fallback: fennec reader parses bare text with ZERO errors (vendored parser, not the dead node)"
    (count_errors fennec_fallback = 0);
  check "fallback: it is NOT the single install-ocamlmerlin-mlx error node"
    (not (List.length fennec_fallback = 1 && count_errors fennec_fallback = 1));

  Printf.printf "reader_delegation: %s\n" (if !fail = 0 then "ALL CHECKS PASSED" else "FAILURES");
  if !fail > 0 then exit 1
