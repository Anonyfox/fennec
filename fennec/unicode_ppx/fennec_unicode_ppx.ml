(* fennec.unicode_ppx — turn the silent cross-target Unicode footgun into a
   compile error.

   OCaml string literals are byte strings. Melange emits a plain ["…"] literal by
   escaping each byte as \xHH, which JavaScript reads as separate Latin-1 chars —
   so a UTF-8 "café" or "🦊" silently becomes mojibake on the client while looking
   fine under native SSR. Melange's own remedy is the [{js|…|js}] delimiter, which
   it emits as a proper JS Unicode string (and which native OCaml treats as the
   exact same bytes). The catch is that this can't be applied automatically by a
   rewriter: melange.ppx re-reads the *source* between the delimiters to validate
   {js}/{j} strings, so a ppx-synthesised [{js}] literal (no faithful source) fails
   to compile. The correct literal therefore has to be written in source.

   So instead of silently fixing it, we make the mistake impossible to ship: this
   linter raises a compile error on any plain (delimiter-less) string literal that
   contains a non-ASCII byte, telling the author to use [{js|…|js}]. It is silent
   on pure-ASCII literals and on literals that already carry a delimiter (the
   {js|…|js} the author wrote). Compile-time proof beats a runtime surprise. *)

open Ppxlib

let has_non_ascii (s : string) : bool = String.exists (fun c -> Char.code c >= 0x80) s

let linter =
  object
    inherit Ast_traverse.iter as super

    method! expression e =
      (match e.pexp_desc with
      (* Only flag literals the AUTHOR wrote: a real (non-ghost) source location.
         Other ppxes in the chain (reason-react-ppx) synthesise strings carrying
         non-ASCII bytes in their marshalled prop metadata, all at ghost/[_none_]
         locations — those are not the user's footgun and must be skipped. *)
      | Pexp_constant (Pconst_string (s, sloc, None))
        when has_non_ascii s && not sloc.loc_ghost ->
        Location.raise_errorf ~loc:sloc
          "fennec: non-ASCII bytes in a plain string literal would become mojibake \
           on the Melange (client) side. Use the {js|…|js} delimiter so it is \
           correct on both SSR and CSR — e.g. {js|%s|js}." s
      | _ -> ());
      super#expression e
  end

let () =
  Driver.register_transformation "fennec_unicode_lint"
    ~impl:(fun str ->
      linter#structure str;
      str)
    ~intf:(fun sg ->
      linter#signature sg;
      sg)
