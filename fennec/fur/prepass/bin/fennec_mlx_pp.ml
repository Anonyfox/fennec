(* ════════════════════════════════════════════════════════════════════════════════════════
   fennec-mlx-pp — the .mlx dialect preprocessor binary.

   Dune's dialect runs `fennec-mlx-pp %{input-file}`, expecting a binary AST on stdout (the same
   contract mlx-pp has). mlx's real parser is NOT a linkable library (it lives in mlx-pp's private
   executable modules — a 49K-line menhir grammar over a pinned compiler AST), so we cannot call it
   in-process; instead we drive the `mlx-pp` binary, but with the spawn folded down to the minimum:

     ── FAST PATH (the file uses no bare-text / {expr} — the pre-pass is a no-op) ───────────────────
     We `execv` mlx-pp on the ORIGINAL file: the process image is REPLACED, so mlx-pp parses the file
     directly and emits its AST to our stdout. ONE process, no temp file, no byte copy, and the output
     is byte-identical to plain mlx-pp. (The previous design spawned mlx-pp as a CHILD and copied every
     byte through — two processes; execv removes the second.)

     ── REWRITE PATH (bare text / {expr} present) ──────────────────────────────────────────────────
     We run the fennec PRE-PASS, then feed the pre-passed source to mlx-pp's STDIN (no temp file — a
     forked writer streams it while we read mlx-pp's stdout), capture its binary AST, and rewrite that
     AST's locations: column-EXACT back to the original .mlx via the pre-pass POSITION MAP, and the
     embedded filename from `*stdin*` to the original path. Then we re-emit. ONE child spawn, no temp
     file. The position map (Fennec_mlx_prepass.Posmap) undoes the byte shift that quoting a bare-text
     run introduces, so an OCaml/mlx type error after a prose run points at the true original column
     (and the true original line — a multi-line prose run collapses, shifting later lines, which the
     map also repairs). The locations are a VALUE-level rewrite (unmarshal → map → re-marshal), robust
     to marshalling layout. The on-disk format mlx-pp emits is exactly:
         <ast_impl_magic_number : 12 raw bytes> <output_value fname : string> <output_value structure>
     and we reproduce it with the original filename + remapped locations.

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

(* Rewrite a parsetree structure's locations: every [pos_fname] becomes [fname], and every position is
   remapped through [pm] back to the ORIGINAL .mlx source (column- AND line-exact). When [pm] is the
   identity (no bare-text run was rewritten — never the case on this path, but cheap to honour) only the
   filename is swapped. Uses compiler-libs' Ast_mapper, whose default traversal visits all locations. *)
let rewrite_locations (fname : string) (pm : Fennec_mlx_prepass.Posmap.t)
    (str : Parsetree.structure) : Parsetree.structure =
  let open Ast_mapper in
  let identity = Fennec_mlx_prepass.Posmap.is_identity pm in
  let map_pos (p : Lexing.position) =
    (* Leave GHOST / dummy positions (Lexing.dummy_pos, pos_cnum < 0 — carried by synthesized
       Location.none nodes the JSX desugar inserts) byte-for-byte as the old driver did: swap only the
       filename, never the offset/line, so a `Location.none` stays a `Location.none`. Only REAL source
       positions are remapped through the pre-pass position map. *)
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

(* Spawn `mlx-pp` reading the pre-passed source from STDIN, returning its full stdout (binary AST). We
   FORK a writer child that streams [src] into mlx-pp's stdin and exits, while WE read mlx-pp's stdout —
   so a large AST (which can exceed the 64 KiB pipe buffer before the writer finishes) never deadlocks.
   Propagates mlx-pp's exit code / stderr unchanged on failure. *)
let run_mlx_pp_stdin (src : string) : string =
  (* Both pipes are CLOEXEC so the spawned mlx-pp does NOT inherit the ends it must not hold: it would
     otherwise keep a copy of [in_w] (its own stdin write-end) open and never see EOF → hang forever.
     [create_process] dups [in_r]→fd0 and [out_w]→fd1 in the child; dup clears cloexec on those, so
     mlx-pp keeps its std fds, while [in_w]/[out_r] stay cloexec and vanish at its exec. (The writer
     fork below does NOT exec, so cloexec is irrelevant to it — it keeps [in_w] across the fork.) *)
  let (in_r, in_w) = Unix.pipe ~cloexec:true () in     (* mlx-pp's stdin  : the writer fork writes in_w *)
  let (out_r, out_w) = Unix.pipe ~cloexec:true () in   (* mlx-pp's stdout : we read out_r *)
  let mlx_pid =
    Unix.create_process "mlx-pp" [| "mlx-pp" |] in_r out_w Unix.stderr
  in
  Unix.close in_r;
  Unix.close out_w;
  (* writer fork: close the read end of stdout we don't need, stream src, then exit hard. *)
  let writer_pid = Unix.fork () in
  if writer_pid = 0 then begin
    (try Unix.close out_r with _ -> ());
    let oc = Unix.out_channel_of_descr in_w in
    set_binary_mode_out oc true;
    (try output_string oc src; flush oc with _ -> ());
    (try close_out oc with _ -> ());   (* closes in_w → mlx-pp sees EOF on stdin *)
    Stdlib.exit 0
  end;
  Unix.close in_w;   (* parent holds no write end → only the fork can keep stdin open *)
  let ic = Unix.in_channel_of_descr out_r in
  set_binary_mode_in ic true;
  let out = read_all_in ic in
  close_in_noerr ic;
  ignore (Unix.waitpid [] writer_pid);
  let _, status = Unix.waitpid [] mlx_pid in
  (match status with
   | Unix.WEXITED 0 -> ()
   | Unix.WEXITED code -> exit code   (* mlx-pp already printed its diagnostic to our stderr *)
   | _ -> exit 1);
  out

let () =
  (* the dialect always passes exactly one arg: the input file. Be lenient: last arg = input. *)
  let input_file =
    let n = Array.length Sys.argv in
    if n < 2 then (prerr_endline "fennec-mlx-pp: expected an input file"; exit 2)
    else Sys.argv.(n - 1)
  in
  let src = read_file input_file in
  let pre, pm = Fennec_mlx_prepass.transform_with_map src in

  (* FAST PATH — the pre-pass changed nothing (a pure-OCaml file, or one already in the quoted + paren
     form). EXEC mlx-pp on the ORIGINAL file, REPLACING this process: mlx-pp reads the file, embeds the
     correct filename, and writes its AST straight to our stdout. One process, no temp file, no copy —
     and provably byte-identical to plain mlx-pp, so every already-migrated / non-JSX file is untouched
     by introducing fennec-mlx-pp. [execvp] searches PATH for mlx-pp exactly like the old child spawn. *)
  if String.equal pre src then begin
    (try Unix.execvp "mlx-pp" [| "mlx-pp"; input_file |]
     with Unix.Unix_error (e, _, _) ->
       Printf.eprintf "fennec-mlx-pp: could not exec mlx-pp: %s\n" (Unix.error_message e);
       exit 127)
  end;

  (* REWRITE PATH — feed the pre-passed source to mlx-pp via stdin (no temp file), then remap the AST. *)
  let out = run_mlx_pp_stdin pre in

  (* parse the binary AST: magic (raw) ++ output_value fname ++ output_value structure, then remap the
     locations to the original .mlx (column/line-exact via [pm]) + rewrite the filename, and re-emit. *)
  let magic = Config.ast_impl_magic_number in
  let mlen = String.length magic in
  if String.length out < mlen || String.sub out 0 mlen <> magic then begin
    (* not the impl-AST we expected (e.g. a future format change): pass it through untouched rather
       than risk corrupting it. *)
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
  let structure = rewrite_locations input_file pm structure in
  set_binary_mode_out stdout true;
  output_string stdout magic;
  output_value stdout input_file;
  output_value stdout structure
