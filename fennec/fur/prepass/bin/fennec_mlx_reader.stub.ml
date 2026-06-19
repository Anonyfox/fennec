(* ════════════════════════════════════════════════════════════════════════════════════════
   Fennec_mlx_reader (stub branch) — compiled ONLY when `merlin-extend` is NOT installed.

   dune `(select fennec_mlx_reader.ml from (merlin-extend -> …real.ml) (-> …stub.ml))` picks this file
   when merlin-extend (an EDITOR-only dependency, shipped with merlin / ocaml-lsp-server) is absent. It
   exists purely so the `ocamlmerlin-fennec-mlx` executable still BUILDS in a minimal environment — the
   build of the framework / CLI never depends on merlin.

   Without merlin-extend we cannot speak the binary external-reader protocol at all, so the only sound
   behaviour is to refuse to run. Merlin would only ever exec this binary if the user has merlin (hence
   merlin-extend) installed, in which case the real branch is the one that got compiled; so in practice
   this stub is never the one Merlin talks to. If it is somehow invoked, it prints a one-line hint to
   stderr and exits non-zero — it never emits garbage onto the protocol stream. *)

let main () =
  prerr_endline
    "ocamlmerlin-fennec-mlx: built without merlin-extend (it was not installed at build time), so \
     the fennec .mlx reader is unavailable. Install merlin / ocaml-lsp-server (which provide \
     merlin-extend) and rebuild. The dune BUILD is unaffected — it uses fennec-mlx-pp.";
  exit 1
