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

   If `mlx-pp` is missing or errors, we propagate its exit code / stderr unchanged.

   ── PERF VERDICT (measured, macOS arm64) ──────────────────────────────────────────────────────────
   The standalone tax over plain mlx-pp is ~3.4 ms, and it is almost ENTIRELY this binary's OWN process
   startup, NOT a foldable spawn: a bare OCaml exe starts in ~1.7 ms (the fork+exec+runtime floor), +unix
   ~2.0 ms, +compiler-libs.common ~3.4 ms (the Parsetree/Ast_mapper/Config we need for the rewrite path).
   So:
     • FAST path: execv removed the only foldable cost (the child-spawn + byte-copy) — ~0.25 ms recovered;
       what remains is this binary having to START (and load compiler-libs) before it can decide it is the
       fast path. Splitting a stdlib-only front off to dodge compiler-libs would recover ~1.3 ms HERE, but
       only for non-bare-text files, and is rejected: it is invisible in the dev loop (below) and adds a
       second installed binary + a double pre-pass for a cold-build-only gain — not worth the surface.
     • REWRITE path: stuck at ~2 process startups (this binary + an mlx-pp CHILD) because we must
       POST-PROCESS mlx-pp's AST (remap locations) — that child cannot be execv'd away. Same speed as the
       old temp-file design, now with column-exact errors. In-process would need mlx's 49K-line parser as
       a library (it is not one — do NOT vendor it), so it is not on the table.
   The dev-server WARM REBUILD (the king metric) is pre-pass-INSENSITIVE: dune re-preprocesses only the
   changed file, ~8 ms against a ~125 ms incremental OCaml recompile (~6 %), so before/after are within
   noise (median 129 → 126 ms). The win here is correctness (column-exact) + a leaner fast path + no temp
   file, not a dev-loop speedup — which the measurements show is not available without in-process linking. *)

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

(* Replace mlx-pp's stdin name `*stdin*` with [fname] in a captured stderr diagnostic, so a HARD
   syntax error from mlx-pp itself (which never yields an AST to remap) points at the user's real .mlx
   rather than at `*stdin*`. mlx prints the OCaml-standard `File "…", line L, …`, so we rewrite the
   token inside the quotes. (Line is exact — the pre-pass is line-preserving; a column inside a quoted
   prose run may be off by the inserted bytes, the documented limit. This is strictly better than the
   old temp-file design, whose syntax errors pointed at a /tmp path.) *)
(* One-line breadcrumb on stderr when `mlx-pp` cannot be found (ENOENT). The dialect runs us from
   inside dune, so without this the failure surfaces as a bare "No such file or directory" with no
   hint at what to install. We emit this BEFORE propagating the exit code, so the dune error carries
   a fix. Only for ENOENT — a present-but-failing mlx-pp already prints its own (rewritten) stderr. *)
let mlx_pp_missing_hint () =
  prerr_endline
    "fennec-mlx-pp: `mlx-pp` not found — run `opam install mlx` (or `fennec doctor`)"

let stdin_name = "*stdin*"
let rewrite_stderr_fname (fname : string) (err : string) : string =
  (* replace every occurrence of the quoted `"*stdin*"` (and the bare token, belt-and-braces). *)
  let replace_all hay needle rep =
    if needle = "" then hay else
    let b = Buffer.create (String.length hay) in
    let nl = String.length needle in
    let i = ref 0 and n = String.length hay in
    while !i < n do
      if !i + nl <= n && String.sub hay !i nl = needle then (Buffer.add_string b rep; i := !i + nl)
      else (Buffer.add_char b hay.[!i]; incr i)
    done;
    Buffer.contents b
  in
  err |> (fun e -> replace_all e ("\"" ^ stdin_name ^ "\"") ("\"" ^ fname ^ "\""))
      |> (fun e -> replace_all e stdin_name fname)

(* Spawn `mlx-pp` reading the pre-passed source from STDIN, returning its full stdout (binary AST). We
   FORK a writer child that streams [src] into mlx-pp's stdin and exits, while WE read mlx-pp's stdout —
   so a large AST (which can exceed the 64 KiB pipe buffer before the writer finishes) never deadlocks.
   On failure we re-emit mlx-pp's stderr with `*stdin*` rewritten to [orig_fname] and exit its code. *)
let run_mlx_pp_stdin ~(orig_fname : string) (src : string) : string =
  (* All three non-std pipe ends are CLOEXEC so the spawned mlx-pp does NOT inherit (and hold open) ends
     it must not — most importantly its own stdin write-end [in_w], which would otherwise keep stdin
     from ever reaching EOF → hang forever. [create_process] dups [in_r]→fd0, [out_w]→fd1, [err_w]→fd2
     in the child; dup clears cloexec on those, so mlx-pp keeps its std fds while the parent-side ends
     vanish at its exec. (The writer fork does NOT exec, so cloexec is irrelevant to it.) *)
  let (in_r, in_w) = Unix.pipe ~cloexec:true () in     (* mlx-pp's stdin  : the writer fork writes in_w *)
  let (out_r, out_w) = Unix.pipe ~cloexec:true () in   (* mlx-pp's stdout : we read out_r *)
  let (err_r, err_w) = Unix.pipe ~cloexec:true () in   (* mlx-pp's stderr : we read err_r, rewrite, re-emit *)
  let mlx_pid =
    (* On some platforms create_process resolves PATH in the parent and raises ENOENT here when
       mlx-pp is absent; on others the child's exec fails and we see WEXITED 127 below. Cover both:
       emit the install hint and exit 127 on a parent-side ENOENT. *)
    try Unix.create_process "mlx-pp" [| "mlx-pp" |] in_r out_w err_w
    with Unix.Unix_error (Unix.ENOENT, _, _) -> mlx_pp_missing_hint (); exit 127
  in
  Unix.close in_r;
  Unix.close out_w;
  Unix.close err_w;
  (* writer fork: close the read ends we don't need, stream src, then exit hard. *)
  let writer_pid = Unix.fork () in
  if writer_pid = 0 then begin
    (try Unix.close out_r with _ -> ());
    (try Unix.close err_r with _ -> ());
    let oc = Unix.out_channel_of_descr in_w in
    set_binary_mode_out oc true;
    (try output_string oc src; flush oc with _ -> ());
    (try close_out oc with _ -> ());   (* closes in_w → mlx-pp sees EOF on stdin *)
    Stdlib.exit 0
  end;
  Unix.close in_w;   (* parent holds no write end → only the fork can keep stdin open *)
  (* read stdout AND stderr. mlx-pp's stderr is tiny (one diagnostic) and only written on error, so a
     sequential read — stdout fully, then stderr fully — cannot deadlock: on success stderr is empty;
     on failure mlx-pp closes stdout (EOF here) before/around writing the small stderr, which fits the
     64 KiB pipe buffer unread. *)
  let ic = Unix.in_channel_of_descr out_r in
  set_binary_mode_in ic true;
  let out = read_all_in ic in
  close_in_noerr ic;
  let ec = Unix.in_channel_of_descr err_r in
  let err = read_all_in ec in
  close_in_noerr ec;
  ignore (Unix.waitpid [] writer_pid);
  let _, status = Unix.waitpid [] mlx_pid in
  (match status with
   | Unix.WEXITED 0 -> ()
   | Unix.WEXITED code ->
     (* 127 with an empty stderr is the classic "child could not exec mlx-pp" (it was not on PATH):
        the failed exec leaves no diagnostic, so add the install breadcrumb before propagating. *)
     if code = 127 && String.trim err = "" then mlx_pp_missing_hint ();
     output_string stderr (rewrite_stderr_fname orig_fname err); flush stderr; exit code
   | _ -> output_string stderr (rewrite_stderr_fname orig_fname err); flush stderr; exit 1);
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
       (* ENOENT = mlx-pp is not on PATH (the `mlx` opam package is not installed). Lead with the
          actionable hint, then the raw error, then exit 127 (the conventional "command not found"). *)
       if e = Unix.ENOENT then mlx_pp_missing_hint ();
       Printf.eprintf "fennec-mlx-pp: could not exec mlx-pp: %s\n" (Unix.error_message e);
       exit 127)
  end;

  (* REWRITE PATH — feed the pre-passed source to mlx-pp via stdin (no temp file), then remap the AST. *)
  let out = run_mlx_pp_stdin ~orig_fname:input_file pre in

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
