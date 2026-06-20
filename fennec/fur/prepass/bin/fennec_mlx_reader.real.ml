(* ════════════════════════════════════════════════════════════════════════════════════════
   Fennec_mlx_reader (real branch) — the fennec MERLIN READER for the .mlx dialect.

   This is the `merlin-extend`-PRESENT branch of the dune `(select fennec_mlx_reader.ml …)` behind the
   `ocamlmerlin-fennec-mlx` executable. When merlin-extend is absent, dune picks the sibling
   `fennec_mlx_reader.stub.ml` instead, so the exe always builds and the BUILD never depends on merlin.

   The dune `(dialect (name mlx) … (merlin_reader fennec-mlx))` makes Merlin (inside ocamllsp)
   invoke that binary as the external reader for every `.mlx` buffer it type-checks for the editor.

   ────────────────────────────────────────────────────────────────────────────────────────
   WHY this exists
   ────────────────────────────────────────────────────────────────────────────────────────
   The fennec `.mlx` surface adds bare JSX text children + `{expr}` interpolation on top of mlx.
   The BUILD already handles it: the dialect's `(preprocess (run %{bin:fennec} mlx-pp …))` runs the
   fennec PRE-PASS (`Fennec_mlx_prepass.transform`) — quoting bare text + normalising `{expr}` →
   `(expr)` — then parses with the VENDORED mlx parser in-process. So `dune build` is perfect.

   The EDITOR is a separate path. Merlin does NOT run the dialect's `(preprocess …)`; for syntax it
   runs the dialect's `(merlin_reader …)` — an external "reader" subprocess that Merlin feeds the raw
   editor buffer and asks for a parsetree/outline. With the stock `mlx` reader pointed at it, bare
   text is a syntax error (every prose word is an unexpected token), so the editor shows red squiggles
   even though the build is clean. This reader closes that gap: it runs the SAME pre-pass over the
   buffer, then delegates to the stock `ocamlmerlin-mlx` reader on the transformed buffer.

   ────────────────────────────────────────────────────────────────────────────────────────
   HOW Merlin invokes a reader  (the external-reader protocol)
   ────────────────────────────────────────────────────────────────────────────────────────
   A reader is a `merlin-extend` extension. Merlin spawns `ocamlmerlin-<name>` (here, with
   `<name> = fennec-mlx`, the binary `ocamlmerlin-fennec-mlx`) with EMPTY argv and talks the
   `MERLINEXTEND002` protocol over stdin/stdout (marshalled OCaml values): a handshake, then a loop of
   `Req_load buffer` / `Req_parse` / `Req_parse_for_completion pos` / `Req_get_ident_at pos` /
   `Req_print_outcome` / `Req_pretty_print` requests, each answered with the matching `Res_*`. The
   buffer carries `{ path; flags; text }` — crucially, the FULL editor text — so we can transform it.
   Merlin sets `__MERLIN_MASTER_PID` in our environment; the stock reader requires it, and since we
   inherit our environment into the child, it is propagated for free.

   ────────────────────────────────────────────────────────────────────────────────────────
   THE DESIGN: a DELEGATING reader, not a fork
   ────────────────────────────────────────────────────────────────────────────────────────
   `Extend_main.extension_main` implements the MERLIN-facing side of the protocol for us (handshake +
   dispatch); we only supply a `Reader.V0` module. For the STOCK-reader side we reuse the very same
   `merlin-extend` machinery from the driver's perspective: `Extend_driver.run "mlx"` spawns
   `ocamlmerlin-mlx` and performs ITS handshake, giving us a typed `request → response` channel to it.

   Our `Reader.V0` therefore forwards every callback to that child reader, and ONLY rewrites the
   buffer text in `load` (and in `parse_line`, the manual-expression entry, which we leave verbatim —
   it is a user-typed OCaml expression, never `.mlx` markup). Because we link the same
   `Extend_protocol` types as Merlin and the child, all marshalling is type-faithful: we never decode
   a parsetree ourselves — we hand Merlin's bytes straight back. This is far more robust than byte
   patching: any future protocol field flows through untouched.

   ────────────────────────────────────────────────────────────────────────────────────────
   POSITION ACCURACY
   ────────────────────────────────────────────────────────────────────────────────────────
   The pre-pass is LINE-PRESERVING (it never adds or removes a newline — it only quotes a text run in
   place, rewrites `{`→`(` / `}`→`)` 1:1, and re-emits dropped inter-element whitespace verbatim). So
   every LINE number the child reader reports is EXACT against the original buffer, on every line.
   COLUMNS are exact on every line the pre-pass did not rewrite (the vast majority — all code lines,
   all attribute lines, all already-quoted text); on a line where bare text was quoted, columns within
   that text shift by the inserted quote bytes (and `{expr}`→`(expr)` keeps the column, parens being
   one byte like braces). The practical contract Merlin needs — no FALSE syntax errors, and working
   hover / type / complete / jump on the vast majority of positions — holds. Column drift inside a
   rewritten prose run is the documented limitation; it never produces a wrong error, only a hover box
   that may be a few columns off inside quoted prose.

   ────────────────────────────────────────────────────────────────────────────────────────
   GRACEFUL DEGRADATION — the in-process VENDORED-PARSE FALLBACK
   ────────────────────────────────────────────────────────────────────────────────────────
   If the stock `ocamlmerlin-mlx` is not installed (it is an editor-only opam dev dependency, never a
   build dependency), spawning the child fails. We then parse the buffer OURSELVES, in-process, with
   the SAME vendored mlx parser the build uses (`Fennec_mlx_cli.parse_transformed` over the vendored
   `Fennec_mlx`), remapping its locations back to the original buffer through the posmap. So the editor
   WORKS off `fennec-cli` alone — valid buffers and bare-text `.mlx` parse, hover/jump/complete resolve
   over the real tree, and there are NO false syntax errors. This has no error-RECOVERY (the stock
   reader, when present, still wins — it reconstructs a partial tree from a buffer that is broken
   mid-edit), so on a genuine syntax error the vendored parser raises and we degrade to a single,
   clearly-worded error node (the old behaviour). The difference is a working editor vs a dead one when
   only `fennec-cli` is installed. The dune BUILD never depends on any of this.
   ════════════════════════════════════════════════════════════════════════════════════════ *)

module P = Extend_protocol
module R = P.Reader

(* ── the child stock reader (ocamlmerlin-mlx), spawned once, lazily ───────────────────────────
   [Extend_driver.run "mlx"] forks `ocamlmerlin-mlx` (empty argv) and runs its handshake. We hold one
   child for the whole process lifetime (Merlin keeps a reader alive across requests for a buffer).
   Wrapped in a [result] so a missing/incompatible stock reader degrades instead of crashing. *)
let child : (Extend_driver.t, exn) result Lazy.t =
  lazy (try Ok (Extend_driver.run "mlx") with exn -> Error exn)

(* The one place the fennec surface is normalised: bare text → quoted, `{expr}` → `(expr)`. Pure,
   total, line-preserving. Identity on any already-quoted / non-JSX buffer (so delegation is exact).
   Returns the transformed buffer AND a {!Fennec_mlx_prepass.Posmap.t} that maps any byte offset in the
   transformed text back to the ORIGINAL editor buffer — so we can remap the child reader's AST
   locations to column- (and line-) EXACT original positions before handing them to Merlin. *)
let transform_buf (text : string) : string * Fennec_mlx_prepass.Posmap.t =
  Fennec_mlx_prepass.transform_with_map text

(* Remap every location in a returned parsetree back to the original buffer through [pm]. A GHOST /
   dummy position (Lexing.dummy_pos, pos_cnum < 0) is left as-is (only Location.none nodes carry it);
   only REAL positions are remapped. When [pm] is the identity (already-quoted / non-JSX buffer) this
   is skipped wholesale, so faithful delegation stays byte-exact. The pre-pass shifts the filename not
   at all (we never rewrote the path), so — unlike the build driver — we touch ONLY the offsets here. *)
let remap_parsetree (pm : Fennec_mlx_prepass.Posmap.t) (pt : R.parsetree) : R.parsetree =
  if Fennec_mlx_prepass.Posmap.is_identity pm then pt
  else begin
    let open Ast_mapper in
    let map_pos (p : Lexing.position) =
      if p.Lexing.pos_cnum < 0 then p else Fennec_mlx_prepass.Posmap.remap_pos pm p
    in
    let map_loc (l : Location.t) =
      { l with Location.loc_start = map_pos l.loc_start; loc_end = map_pos l.loc_end }
    in
    let m = { default_mapper with location = (fun _ l -> map_loc l) } in
    match pt with
    | R.Structure s -> R.Structure (m.structure m s)
    | R.Signature s -> R.Signature (m.signature m s)
  end

(* Synthesise a one-item structure carrying a single syntax-error extension node at the very start
   of the buffer. The LAST-RESORT degraded result: reached only when there is no stock reader AND the
   in-process vendored fallback itself failed to parse (a genuinely broken buffer mid-edit).
   [Extend_helper.syntax_error] builds exactly the node Merlin renders as a diagnostic, so the editor
   shows one informative message instead of crashing the LSP. *)
let error_structure (msg : string) : R.parsetree =
  let ext = Extend_helper.syntax_error msg Location.none in
  let item =
    { Parsetree.pstr_loc = Location.none;
      pstr_desc =
        Pstr_eval
          ( { Parsetree.pexp_loc = Location.none;
              pexp_loc_stack = [];
              pexp_attributes = [];
              pexp_desc = Pexp_extension ext },
            [] ) }
  in
  R.Structure [ item ]

let no_stock_reader_msg =
  "fennec: this .mlx buffer could not be parsed (the in-process vendored parser found a syntax error \
   and the optional ocamlmerlin-mlx error-recovery reader is not installed). The build is unaffected \
   (it uses `fennec mlx-pp`). For richer in-editor recovery, install: opam install ocamlmerlin-mlx"

module Fennec_reader : R.V0 = struct
  (* Per-buffer internal state: whether the child stock reader is available, the position map for the
     buffer last loaded (so [parse]/[for_completion] can remap AST locations back to the ORIGINAL
     editor buffer — column- and line-exact), the TRANSFORMED buffer text + its path (so the
     vendored-parse fallback can parse it in-process when there is no child), and the bare path. *)
  type t = {
    have_child : bool;
    posmap : Fennec_mlx_prepass.Posmap.t;
    transformed : string;   (* the post-pre-pass buffer — fed to the in-process vendored parser *)
    path : string;          (* the buffer path — stamped onto the fallback parse's locations *)
  }

  (* Forward a request to the child, or signal that there is no child. *)
  let to_child (req : R.request) : R.response option =
    match Lazy.force child with
    | Ok drv -> Some (Extend_driver.reader drv req)
    | Error _ -> None

  (* The VENDORED-PARSE FALLBACK (no stock reader): parse the transformed buffer in-process with the
     SAME vendored mlx parser the build uses ([Fennec_mlx_cli.parse_transformed]), then remap its
     locations back to the original editor buffer through the posmap. So the editor gets a REAL
     parsetree off fennec-cli alone — valid buffers + bare text resolve, no false errors — instead of
     the dead error node. On a genuine syntax error (an actually-broken buffer mid-edit) the vendored
     parser raises; we then degrade to a single informative error node, exactly as before, so the LSP
     stays up. This has no error-RECOVERY (the stock reader, when present, is still better — it
     recovers partial trees), but it is the difference between a working editor and a dead one. *)
  let fallback_parse (t : t) : R.parsetree =
    match Fennec_mlx_cli.parse_transformed ~fname:t.path t.transformed with
    | structure -> remap_parsetree t.posmap (R.Structure structure)
    | exception _ -> error_structure no_stock_reader_msg

  (* [load]: TRANSFORM the buffer text (capturing the position map + the transformed text for the
     fallback), then hand the transformed buffer to the child's [Req_load]. Everything downstream
     (parse/completion/ident) then operates on a buffer the stock reader can parse (if present) — and
     we remap its locations back to the original buffer using the captured map. When no child is
     present we keep the transformed text + path so [parse] can run the in-process vendored fallback. *)
  let load (buf : R.buffer) : t =
    let text', posmap = transform_buf buf.R.text in
    let base = { have_child = true; posmap; transformed = text'; path = buf.R.path } in
    let buf' = { buf with R.text = text' } in
    match to_child (R.Req_load buf') with
    | Some R.Res_loaded -> base
    | Some _ -> base (* unexpected, but the child accepted a load *)
    | None -> { base with have_child = false }

  let parse (t : t) : R.parsetree =
    if not t.have_child then fallback_parse t
    else
      match to_child R.Req_parse with
      | Some (R.Res_parse pt) -> remap_parsetree t.posmap pt
      | _ -> fallback_parse t

  let for_completion (t : t) (pos : Lexing.position) : R.complete_info * R.parsetree =
    if not t.have_child then ({ R.complete_labels = true }, fallback_parse t)
    else
      (* the completion POSITION Merlin gives us is in ORIGINAL-buffer coordinates; the child expects
         TRANSFORMED-buffer coordinates. Map it forward (original → transformed) before asking, then
         map the returned AST back. Forward-mapping is the inverse of the pre-pass shift: for a position
         at/after a quoted run the transformed offset is larger. We approximate it well enough for
         completion by walking the same replaced-run deltas — see {!Posmap.fwd_pos}. *)
      let pos' = Fennec_mlx_prepass.Posmap.fwd_pos t.posmap pos in
      match to_child (R.Req_parse_for_completion pos') with
      | Some (R.Res_parse_for_completion (info, pt)) -> (info, remap_parsetree t.posmap pt)
      | _ -> ({ R.complete_labels = true }, fallback_parse t)

  (* [parse_line] is a user-typed expression (merlin's "type this expression" / locate-on-input),
     NOT `.mlx` markup — pass the text through unmodified to the child. *)
  let parse_line (t : t) (pos : Lexing.position) (str : string) : R.parsetree =
    if not t.have_child then error_structure no_stock_reader_msg
    else
      match to_child (R.Req_parse_line (pos, str)) with
      | Some (R.Res_parse pt) -> pt
      | _ -> error_structure no_stock_reader_msg

  let ident_at (t : t) (pos : Lexing.position) : string Location.loc list =
    if not t.have_child then []
    else
      match to_child (R.Req_get_ident_at pos) with
      | Some (R.Res_get_ident_at l) -> l
      | _ -> []

  (* The two printers operate on values Merlin already holds; the child renders them to strings. On
     the degraded path, [print_outcome] falls back to compiler-libs' Oprint (so completion/type
     output is still sensible), and [pretty_print] is a no-op — exactly what the stock mlx reader
     itself does for case-destruction (`let pretty_print _ _ = ()`). *)
  let print_outcome (ppf : Format.formatter) (otree : R.outcometree) : unit =
    match to_child (R.Req_print_outcome [ otree ]) with
    | Some (R.Res_print_outcome [ s ]) -> Format.pp_print_string ppf s
    | _ -> Extend_helper.print_outcome_using_oprint ppf otree

  let pretty_print (ppf : Format.formatter) (p : R.pretty_parsetree) : unit =
    match to_child (R.Req_pretty_print p) with
    | Some (R.Res_pretty_print s) -> Format.pp_print_string ppf s
    | _ -> ignore ppf; ignore p
end

(* Entry point. This whole module is the `merlin-extend`-present branch of a dune `(select …)`; it is
   compiled ONLY when merlin-extend is installed. When it is not, dune picks `fennec_mlx_reader.stub.ml`
   instead, so the executable still builds (the BUILD never depends on merlin). The thin
   `ocamlmerlin_fennec_mlx.ml` entry just calls [Fennec_mlx_reader.main ()]. *)
let main () =
  (* Windows needs binary stdin/stdout for the marshalled protocol; harmless elsewhere. *)
  (match Sys.win32 with
   | true -> set_binary_mode_in stdin true; set_binary_mode_out stdout true
   | false -> ());
  let open Extend_main in
  extension_main
    ~reader:(Reader.make_v0 (module Fennec_reader : R.V0))
    (Description.make_v0 ~name:"fennec-mlx" ~version:"0.1")
