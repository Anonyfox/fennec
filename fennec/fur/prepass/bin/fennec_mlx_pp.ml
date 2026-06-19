(* ════════════════════════════════════════════════════════════════════════════════════════
   fennec-mlx-pp — the .mlx dialect preprocessor binary.

   Dune's dialect runs `fennec-mlx-pp %{input-file}`, expecting a binary AST on stdout (the same
   contract mlx-pp has). This driver:

     1. reads the input .mlx;
     2. runs the fennec PRE-PASS (Fennec_mlx_prepass.transform) — bare text + {expr} become the
        quoted-text + (expr) form mlx accepts;
     3. writes the pre-passed source to a temp file and runs the REAL `mlx-pp` on it, capturing its
        binary-AST stdout (mlx does the actual parse — we never re-implement it);
     4. REWRITES the filename embedded in that binary AST from the temp path back to the ORIGINAL
        input path, so every location, every error message, and the fur ppx's `.mlx`-suffix /
        route-param / data-fur logic see the real source path — never a temp file.

   Step 4 is a VALUE-level rewrite (unmarshal → swap pos_fname → re-marshal), not byte patching, so
   it is robust to marshalling layout. The on-disk format mlx-pp emits is exactly:
       <ast_impl_magic_number : 12 raw bytes> <output_value fname : string> <output_value structure>
   and we reproduce it with the original filename.

   If `mlx-pp` is missing or errors, we propagate its exit code / stderr unchanged. *)

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

(* swap every [pos_fname] in a parsetree structure to [fname] (a single shared string), preserving
   everything else. Uses compiler-libs' Ast_mapper, whose default traversal visits all locations. *)
let rewrite_filename (fname : string) (str : Parsetree.structure) : Parsetree.structure =
  let open Ast_mapper in
  let map_pos (p : Lexing.position) = { p with Lexing.pos_fname = fname } in
  let map_loc (l : Location.t) =
    { l with Location.loc_start = map_pos l.loc_start; loc_end = map_pos l.loc_end }
  in
  let mapper = { default_mapper with location = (fun _ l -> map_loc l) } in
  mapper.structure mapper str

let () =
  (* the dialect always passes exactly one arg: the input file. Be lenient: last arg = input. *)
  let input_file =
    let n = Array.length Sys.argv in
    if n < 2 then (prerr_endline "fennec-mlx-pp: expected an input file"; exit 2)
    else Sys.argv.(n - 1)
  in
  let src = read_file input_file in
  let pre = Fennec_mlx_prepass.transform src in

  (* FAST PATH — the pre-pass changed nothing (a pure-OCaml file, or one already in the quoted +
     paren form). Run mlx-pp on the ORIGINAL file and stream its bytes through UNCHANGED: this keeps
     the embedded filename naturally correct AND makes the output BYTE-IDENTICAL to plain mlx-pp, so
     every already-migrated / non-JSX file is provably untouched by introducing fennec-mlx-pp. *)
  if String.equal pre src then begin
    let (r, w) = Unix.pipe () in
    let pid = Unix.create_process "mlx-pp" [| "mlx-pp"; input_file |] Unix.stdin w Unix.stderr in
    Unix.close w;
    let ic = Unix.in_channel_of_descr r in
    set_binary_mode_in ic true;
    let out = read_all_in ic in
    close_in_noerr ic;
    let _, status = Unix.waitpid [] pid in
    (match status with Unix.WEXITED code when code <> 0 -> exit code
                     | Unix.WEXITED 0 -> () | _ -> exit 1);
    set_binary_mode_out stdout true;
    output_string stdout out;
    exit 0
  end;

  (* hand the pre-passed source to mlx-pp via a temp file *)
  let tmp = Filename.temp_file "fennec_mlx_pp_" ".mlx" in
  Fun.protect ~finally:(fun () -> try Sys.remove tmp with _ -> ()) @@ fun () ->
  (let oc = open_out_bin tmp in
   Fun.protect ~finally:(fun () -> close_out_noerr oc) (fun () -> output_string oc pre));

  (* run mlx-pp <tmp>, capturing stdout (binary AST). We use Unix to read its stdout fully. *)
  let (r, w) = Unix.pipe () in
  let pid =
    Unix.create_process "mlx-pp" [| "mlx-pp"; tmp |] Unix.stdin w Unix.stderr
  in
  Unix.close w;
  let ic = Unix.in_channel_of_descr r in
  set_binary_mode_in ic true;
  let out = read_all_in ic in
  close_in_noerr ic;
  let _, status = Unix.waitpid [] pid in
  (match status with
   | Unix.WEXITED 0 -> ()
   | Unix.WEXITED code -> exit code     (* mlx-pp already printed its diagnostic to our stderr *)
   | _ -> exit 1);

  (* parse the binary AST: magic (raw) ++ output_value fname ++ output_value structure, then rewrite
     the filename back to the original input path and re-emit in the same layout. *)
  let magic = Config.ast_impl_magic_number in
  let mlen = String.length magic in
  if String.length out < mlen || String.sub out 0 mlen <> magic then begin
    (* not the impl-AST we expected (e.g. mlx-pp -print-ml / future format change): pass it through
       untouched rather than risk corrupting it. *)
    set_binary_mode_out stdout true;
    output_string stdout out;
    exit 0
  end;
  (* read the two marshaled values that follow the magic *)
  let pos = ref mlen in
  let read_value () : Obj.t =
    let v = Marshal.from_string out !pos in
    pos := !pos + Marshal.total_size (Bytes.unsafe_of_string out) !pos;
    v
  in
  let (_fname : string) = (Obj.obj (read_value ()) : string) in
  let (structure : Parsetree.structure) = (Obj.obj (read_value ()) : Parsetree.structure) in
  let structure = rewrite_filename input_file structure in
  set_binary_mode_out stdout true;
  output_string stdout magic;
  output_value stdout input_file;
  output_value stdout structure
