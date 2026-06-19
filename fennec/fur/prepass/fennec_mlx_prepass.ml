(* ════════════════════════════════════════════════════════════════════════════════════════
   fennec-mlx-pp PRE-PASS — bare JSX text for fennec .mlx, as a source-to-source transform.

   This is NOT a fork of mlx OCaml parser. It is a small, fennec-owned LEXER that transforms a
   fennec-flavoured .mlx (React/Svelte-style: bare text children + brace interpolation) into the
   exact form today mlx-pp already accepts (quoted-string children + paren escapes). The driver
   (pp.ml) runs THIS first, then hands the result to mlx-pp — so mlx does the real parse.

   ────────────────────────────────────────────────────────────────────────────────────────
   WHY a lexer (not a regex, not a parser)
   ────────────────────────────────────────────────────────────────────────────────────────
   The single hard requirement is: NEVER touch OCaml code. A less-than inside a string literal, an
   open-brace inside a CSS quoted-string extension, a comment-closer hidden inside a string literal
   inside a comment — none of these may be misread as JSX. The only way to get that right is to track
   the full OCaml lexical context, exactly the way the mlx lexer.mll does. So this module is a
   faithful, minimal re-implementation of the relevant slice of that lexer: the same string / char /
   nested-comment / quoted-string-extension rules, and the same rule that a less-than followed by a
   letter or underscore starts a JSX tag, while any other less-than is an operator (lexer.mll maps
   less-than then a lowercase ident to JSX_LIDENT).

   We only ADD ONE thing on top of what mlx accepts: inside a JSX CHILD region, a run that is not
   itself a brace-escape / paren-escape / string / nested-tag is treated as TEXT and emitted as a
   quoted OCaml string. Everywhere else we are byte-for-byte the identity function.

   ────────────────────────────────────────────────────────────────────────────────────────
   THE MODE MACHINE  (see scan_code / scan_tag / scan_children)
   ────────────────────────────────────────────────────────────────────────────────────────
   The cursor walks the source once. A handful of mutually-recursive scanners model the lexical
   context; each owns its sub-region and returns to its caller when that region closes:

     - Code      — ordinary OCaml. The default / outermost. Recognises the literal openers below and,
                   crucially, a JSX open tag (less-than then an ident). Everything else is verbatim.
     - Tag       — inside an opening tag, reading its attribute list. Ends at GREATER (then Children)
                   or SLASH-GREATER (element closed). Attribute VALUES are OCaml simple_expr; a
                   string / paren / record / char literal after the equals is copied verbatim, and a
                   brace attribute value is normalised to a paren one (the JSX form name={x}).
     - Children — between a tag GREATER and its matching closing tag. THE ONLY mode that rewrites:
                   bare text becomes a quoted string; a brace-escape becomes a paren-escape; a
                   paren-escape / string / nested-tag passes through. A closing tag ends it.
     - String   — inside a double-quoted string, honouring backslash escapes. Verbatim.
     - Comment  — inside an OCaml block comment, NESTED; a string / quoted-string / char literal
                   inside it is itself tracked so a comment-closer hidden in a literal does not end
                   it. Verbatim.
     - Quoted   — inside a quoted-string extension (the CSS/JS payload of a style block, etc.). Ends
                   only at its matching close delimiter. Verbatim — the braces / colons / semicolons
                   of CSS never reach the JSX logic.

   String / comment / quoted-string framing is shared by Code, Tag AND Children, so a literal that
   contains a fake tag or fake brace-escape is always inert no matter where it appears.

   ────────────────────────────────────────────────────────────────────────────────────────
   THE CHILD-TEXT CONTRACT (what bare text means)  — matches JSX
   ────────────────────────────────────────────────────────────────────────────────────────
   mlx parses JSX children as a list of simple_expr. That already accepts a paren-escape, a string, a
   nested tag, and even a lone identifier — but it CANNOT accept prose (Welcome to Fennec: the word
   "to" is a keyword, a syntax error; "a b": silently two idents). So in Children we adopt the React
   rule:

     - a run that does not start with a brace, paren, double-quote or less-than is TEXT, collected up
       to the next of those, whitespace-collapsed (JSX-style: inner whitespace runs collapse to one
       space; a run touching an element edge is trimmed on that side; an all-whitespace run between
       two elements is dropped), and emitted as a quoted OCaml string;
     - a brace-escape is the value escape, normalised to a paren-escape (a bare brace in mlx is a
       RECORD, never a child escape, so this rewrite is mandatory, not cosmetic);
     - a paren-escape, a string and a nested tag pass through unchanged (so the old syntax keeps
       working and the transform is idempotent).

   Text that must contain a brace, paren, less-than or significant edge whitespace uses the explicit
   double-quoted-string escape hatch and is copied verbatim. This is the documented fur.mli contract.

   ────────────────────────────────────────────────────────────────────────────────────────
   POSITION / LINE PRESERVATION
   ────────────────────────────────────────────────────────────────────────────────────────
   The transform never adds or removes a newline: quoting a text run replaces it in place, the
   brace-to-paren rewrites are 1:1, and when we DROP an all-whitespace inter-element run we re-emit
   its newlines (bare newlines are insignificant between tags) to keep every later line number exact.
   The fur ppx keys on the .mlx filename and on style-block content hashes, neither of which we
   perturb; data-fur scoping and filename route-param injection therefore stay correct.

   UTF-8 is handled as opaque bytes: every continuation/lead byte is at or above 0x80 and so can never
   be one of the ASCII delimiters we look for; an emoji in bare text is copied through into the quoted
   string unchanged (proven by the emoji tests).
   ════════════════════════════════════════════════════════════════════════════════════════ *)

(* ── tiny character classes, kept in lock-step with mlx's lexer.mll ──────────────────────── *)

let is_lower c = (c >= 'a' && c <= 'z') || c = '_'
let is_upper c = c >= 'A' && c <= 'Z'
(* identchar = ['A'-'Z' 'a'-'z' '_' '\'' '0'-'9'] *)
let is_identchar c =
  (c >= 'A' && c <= 'Z') || (c >= 'a' && c <= 'z') || c = '_' || c = '\'' || (c >= '0' && c <= '9')

(* `<` begins a JSX tag (open or close) iff the next char starts an identifier — mlx:
   `"<" (lowercase …)` / `"<" (uppercase …)` / `"</" …`. A `<` before a space, `<`, `=`, `>`, `/`
   without an ident, etc. is the LESS / `<-` / `<>`/`<<` … operator and must be left to OCaml. *)
let starts_ident c = is_lower c || is_upper c

(* ── output buffer + a verbatim copier ──────────────────────────────────────────────────── *)

type st = {
  src : string;
  len : int;
  buf : Buffer.t;
  mutable i : int;            (* read cursor *)
}

let peek st k = if st.i + k < st.len then st.src.[st.i + k] else '\000'
let cur st = peek st 0
let emit st c = Buffer.add_char st.buf c
let emit_s st s = Buffer.add_string st.buf s
(* copy the current char to the output and advance *)
let copy st = emit st st.src.[st.i]; st.i <- st.i + 1
(* advance without copying (used when we replace a delimiter) *)
let skip st = st.i <- st.i + 1

(* ════════════════════════════════════════════════════════════════════════════════════════
   SHARED LITERAL SCANNERS — copy a string / char / comment / quoted-string VERBATIM and return.
   These are used from Code, Tag and Children alike. Each assumes the cursor is ON the opener.
   ════════════════════════════════════════════════════════════════════════════════════════ *)

(* A double-quoted string literal — honour backslash escapes; stop after the closing quote. (mlx is
   lax about a stray backslash, but for COPYING we only need to know that an escaped quote does not
   close the string, which holds.) Cursor is on the opening quote. *)
let copy_string st =
  copy st (* opening quote *);
  let rec go () =
    if st.i >= st.len then ()                       (* unterminated — let mlx report it *)
    else
      let c = cur st in
      if c = '\\' then (copy st; if st.i < st.len then copy st; go ())
      else if c = '"' then copy st                  (* closing quote *)
      else (copy st; go ())
  in
  go ()

(* {delim|…|delim}  — cursor is on `{`; `delim` is lowercase*, already validated by the caller to be
   a real quoted-string opener. Copies through the matching `|delim}`. *)
let copy_quoted_string st ~delim =
  copy st (* { *);
  for _ = 1 to String.length delim do copy st done; (* delim *)
  copy st (* | *);
  let close = "|" ^ delim ^ "}" in
  let clen = String.length close in
  let rec go () =
    if st.i >= st.len then ()
    else if st.i + clen <= st.len && String.sub st.src st.i clen = close then
      (emit_s st close; st.i <- st.i + clen)
    else (copy st; go ())
  in
  go ()

(* is the cursor on a quoted-string opener (an open-brace, then lowercase* delim, then a bar)? return
   its delim if so. Mirrors lexer.mll's quoted-string opener rule. (We do NOT special-case the
   extension-quoted-string forms that start brace-then-percent: percent is not lowercase, so the scan
   below returns None and the generic open-brace handling copies that verbatim too — fine, since none
   of that is JSX.) *)
let quoted_string_delim st =
  if cur st <> '{' then None
  else begin
    (* a delim is `lowercase*` where mlx's lowercase = ['a'-'z' '_'] *)
    let j = ref (st.i + 1) in
    while !j < st.len && ((st.src.[!j] >= 'a' && st.src.[!j] <= 'z') || st.src.[!j] = '_') do incr j done;
    if !j < st.len && st.src.[!j] = '|' then Some (String.sub st.src (st.i + 1) (!j - (st.i + 1)))
    else None
  end

(* char literal: '\'' c '\'' etc. We must recognise these in Code/Tag so that a quote inside, say,
   `'<'` or `'"'` does not start a string. We DON'T need them inside Children (a bare `'` there is
   just text). Returns true and copies if the cursor is on a char literal. Mirrors the relevant
   lexer.mll char rules; conservative — only the well-formed shapes, else false (cursor untouched). *)
let try_copy_char st =
  if cur st <> '\'' then false
  else begin
    let c1 = peek st 1 and c2 = peek st 2 in
    (* '\'' x '\''  (incl. '\'' for the escaped quote handled by the backslash branch) *)
    if c1 = '\\' then begin
      (* '\\' <one or more> '\'' : copy until the closing quote, bounded *)
      (* shapes: '\n' '\\' '\'' '\123' '\xFF' '\o123' '\ '  — all end at the next unescaped ' *)
      let j = ref (st.i + 2) in
      (* skip the escape body up to (and we will then expect) a closing quote *)
      while !j < st.len && st.src.[!j] <> '\'' && st.src.[!j] <> '\n' do incr j done;
      if !j < st.len && st.src.[!j] = '\'' then (for _ = st.i to !j do copy st done; true)
      else false
    end
    else if c1 <> '\000' && c1 <> '\'' && c1 <> '\n' && c2 = '\'' then
      (* '<' '"' 'a' … : a single char between quotes *)
      (copy st; copy st; copy st; true)
    else false
  end

(* ════════════════════════════════════════════════════════════════════════════════════════
   COMMENT — an OCaml block comment, NESTED; tracks string + quoted-string + char literals inside so
   a comment-closer hidden in a literal does not end it. Copies verbatim. Cursor is on the opening
   paren of the comment opener. Mirrors mlx lexer.mll's `comment` rule.
   ════════════════════════════════════════════════════════════════════════════════════════ *)
let copy_comment st =
  copy st (* ( *); copy st (* * *);
  let depth = ref 1 in
  while !depth > 0 && st.i < st.len do
    let c = cur st in
    if c = '(' && peek st 1 = '*' then (copy st; copy st; incr depth)
    else if c = '*' && peek st 1 = ')' then (copy st; copy st; decr depth)
    else if c = '"' then copy_string st
    else (match quoted_string_delim st with
          | Some delim -> copy_quoted_string st ~delim
          | None -> if try_copy_char st then () else copy st)
  done

(* ════════════════════════════════════════════════════════════════════════════════════════
   CHILDREN — the rewriting heart.

   We track, within ONE children region, whether the *immediately preceding* emitted thing was an
   element/escape boundary (so a leading-edge text run is left-trimmed) and accumulate a pending text
   run with its surrounding whitespace, applying JSX collapsing only when we flush.

   `scan_expr_paren` / `scan_expr_brace` copy a balanced `(…)` / `{…}` (rewriting only the outer
   braces of the latter to parens), with full literal awareness inside (so `{ "}" }` or `( ">" )`
   are balanced correctly). These are also what makes nested JSX inside an escape work — a `<tag>`
   inside `{…}` is just OCaml that mlx will parse; we don't need to re-enter Children for it, we copy
   the bytes through and mlx handles the nested element. (Children mode is only entered for the
   TOP-LEVEL element written as bare JSX; everything inside `{…}`/`(…)` is plain mlx already.)
   ════════════════════════════════════════════════════════════════════════════════════════ *)

(* copy a balanced delimited region `open … close` VERBATIM (literal-aware), assuming cursor on the
   opener. Used for `(…)`. Returns with cursor just past the matching close. *)
let copy_balanced st ~op ~cl =
  copy st (* opener *);
  let depth = ref 1 in
  while !depth > 0 && st.i < st.len do
    let c = cur st in
    if c = '"' then copy_string st
    else if c = '(' && peek st 1 = '*' then copy_comment st
    else (match quoted_string_delim st with
          | Some delim -> copy_quoted_string st ~delim
          | None ->
            if try_copy_char st then ()
            else begin
              if c = op then incr depth
              else if c = cl then decr depth;
              copy st
            end)
  done

(* copy a balanced `{ … }` but EMIT it as `( … )` (the value-escape rewrite). Literal-aware so a
   `}` inside a string / quoted-string inside the expr does not close early; inner `{ }` (e.g. a
   record literal in the expression) are balanced and copied as-is (NOT rewritten — only the
   outermost pair is the escape). *)
let rewrite_brace_escape st =
  skip st (* drop the opening { *);
  emit st '(';
  let depth = ref 1 in
  while !depth > 0 && st.i < st.len do
    let c = cur st in
    if c = '"' then copy_string st
    else if c = '(' && peek st 1 = '*' then copy_comment st
    else (match quoted_string_delim st with
          | Some delim -> copy_quoted_string st ~delim
          | None ->
            if try_copy_char st then ()
            else if c = '{' then (incr depth; copy st)
            else if c = '}' then (decr depth; if !depth = 0 then (skip st; emit st ')') else copy st)
            else copy st)
  done

(* JSX whitespace collapse for one bare-text run — the canonical React/Babel algorithm
   (`cleanJSXElementLiteralChild`), which is what the existing `frontend_test` HTML asserts against:

     · split on newlines;
     · strip the LEADING blank of every line except the first, and the TRAILING blank of every line
       except the last (so source indentation that merely aligns markup disappears);
     · drop now-empty lines;
     · join the surviving lines with a single space.

   The decisive property: a run that is ALL on one line is left untouched (no newline ⇒ no trimming),
   so `Hello {name}!` keeps the space → "Hello " / `(name)` / "!", and `todos: {n}` keeps its
   trailing space; whereas a run spanning newlines (indentation between elements) collapses — an
   all-whitespace inter-element run becomes empty and is dropped. Returns the OCaml string LITERAL
   bytes (with quotes) or None when nothing survives. Blank = space / tab / form-feed (NOT newline,
   which is the line separator and the carriage-return that may precede it). *)
let is_hblank c = c = ' ' || c = '\t' || c = '\012'

let strip_leading_blanks s =
  let n = String.length s in
  let i = ref 0 in
  while !i < n && is_hblank s.[!i] do incr i done;
  String.sub s !i (n - !i)

let strip_trailing_blanks s =
  let n = String.length s in
  let j = ref n in
  while !j > 0 && is_hblank s.[!j - 1] do decr j done;
  String.sub s 0 !j

let collapse_text text =
  (* split on \n, tolerating a \r before it (CRLF) *)
  let raw_lines = String.split_on_char '\n' text in
  let lines = List.map (fun l ->
      let m = String.length l in
      if m > 0 && l.[m - 1] = '\r' then String.sub l 0 (m - 1) else l) raw_lines in
  let nlines = List.length lines in
  let out = Buffer.create (String.length text) in
  List.iteri (fun idx line ->
      let is_first = idx = 0 and is_last = idx = nlines - 1 in
      let line = if is_first then line else strip_leading_blanks line in
      let line = if is_last then line else strip_trailing_blanks line in
      if line <> "" then begin
        if Buffer.length out > 0 then Buffer.add_char out ' ';
        Buffer.add_string out line
      end)
    lines;
  let s = Buffer.contents out in
  if s = "" then None
  else begin
    (* OCaml-escape for a double-quoted literal *)
    let e = Buffer.create (String.length s + 2) in
    Buffer.add_char e '"';
    String.iter (fun c ->
      match c with
      | '"' -> Buffer.add_string e "\\\""
      | '\\' -> Buffer.add_string e "\\\\"
      | _ -> Buffer.add_char e c) s;
    Buffer.add_char e '"';
    Some (Buffer.contents e)
  end

(* The children loop. Cursor is just AFTER the opening tag's `>`. Consumes through (and including) the
   matching `</…>`'s `>` then returns. A nested element starting at `<` is handed to {!scan_tag}. *)
let rec scan_children st =
  (* pending bare-text accumulation (raw bytes, collapsed at flush time by the JSX algorithm) *)
  let pend = Buffer.create 32 in
  let flush () =
    let text = Buffer.contents pend in
    Buffer.clear pend;
    if text = "" then ()
    else match collapse_text text with
      | Some lit -> emit_s st lit
      | None ->
        (* an all-whitespace inter-element run: mlx skips inter-child whitespace (the lexer's blank
           rule), so it is inert as a child. Copy it VERBATIM — that both preserves the source
           indentation/formatting and keeps every byte offset and line number exact. *)
        emit_s st text
  in
  let continue = ref true in
  while !continue && st.i < st.len do
    let c = cur st in
    if c = '<' then begin
      let d = peek st 1 in
      if d = '/' then begin
        (* closing tag </…> — flush any trailing text, copy the close tag verbatim, and return. *)
        flush ();
        copy st (* < *); copy st (* / *);
        while st.i < st.len && cur st <> '>' do copy st done;
        if st.i < st.len then copy st (* > *);
        continue := false
      end
      else if starts_ident d then begin
        (* nested open tag — flush text, then hand the whole nested element to [scan_tag], which will
           itself recurse into scan_children for its body. *)
        flush ();
        scan_tag st
      end
      else begin
        (* `<` not forming a tag (someone wrote a bare `<` in text). It is not valid JSX child markup;
           accumulate it as text so it gets quoted (and mlx stays happy). A degenerate case the docs
           tell users to avoid (write `&lt;` or quote it); we still never corrupt anything. *)
        Buffer.add_char pend c; skip st
      end
    end
    else if c = '{' then begin
      (* a quoted-string-extension opener would not appear bare in child position (it would need to
         be a value), but be safe: if it IS a quoted-string opener, copy verbatim; else it is the
         value escape `{expr}` → rewrite to a paren escape. *)
      flush ();
      (match quoted_string_delim st with
       | Some delim -> copy_quoted_string st ~delim
       | None -> rewrite_brace_escape st)
    end
    else if c = '(' then begin
      (* paren escape `(expr)` — pass through verbatim (back-compat). An open-paren-star here is a
         comment, handled first. *)
      flush ();
      if peek st 1 = '*' then copy_comment st
      else copy_balanced st ~op:'(' ~cl:')'
    end
    else if c = '"' then begin
      (* explicit string child — verbatim escape hatch. *)
      flush ();
      copy_string st
    end
    else begin
      (* bare text byte — accumulate. *)
      Buffer.add_char pend c; skip st
    end
  done;
  (* if we ran off the end without a close tag, flush whatever is pending (mlx then reports the
     unterminated element). *)
  if Buffer.length pend > 0 then flush ()

(* Scan a JSX element starting at `<ident`. Copies the opening tag (attributes are simple_expr →
   their `"…"`/`(…)`/`{…record…}`/char literals are copied verbatim, NOT rewritten — attribute values
   are plain OCaml). On `>` → enter scan_children. On `/>` → self-closing, return. Cursor is ON `<`. *)
and scan_tag st =
  copy st (* < *);
  (* tag name: (lowercase|uppercase) identchar*  then optional .More for Module paths *)
  while st.i < st.len && (is_identchar (cur st) || cur st = '.') do copy st done;
  (* attribute list, up to `>` or `/>` *)
  let opened_children = ref false and self_closed = ref false and stop = ref false in
  while (not !stop) && st.i < st.len do
    let c = cur st in
    if c = '>' then (copy st; opened_children := true; stop := true)
    else if c = '/' && peek st 1 = '>' then (copy st; copy st; self_closed := true; stop := true)
    else if c = '"' then copy_string st
    else if c = '(' && peek st 1 = '*' then copy_comment st
    else if c = '(' then copy_balanced st ~op:'(' ~cl:')'   (* name=(expr) attribute value *)
    else (match quoted_string_delim st with
          | Some delim -> copy_quoted_string st ~delim       (* name=quoted-string attr (rare; safe) *)
          | None ->
            if c = '{' then
              (* name={expr} attribute value — normalise the brace escape to a paren one, exactly like
                 a child escape, so attributes accept the JSX `{…}` form too. *)
              rewrite_brace_escape st
            else if try_copy_char st then ()
            else copy st)
  done;
  if !opened_children && not !self_closed then scan_children st

(* ════════════════════════════════════════════════════════════════════════════════════════
   CODE — the outermost driver. Copies OCaml verbatim, entering the shared literal scanners and, on a
   JSX open tag, [scan_tag]. There is no separate "Tag/Children" recursion here: scan_tag/scan_children
   own that sub-tree and return to Code when the element closes.
   ════════════════════════════════════════════════════════════════════════════════════════ *)
let scan_code st =
  while st.i < st.len do
    let c = cur st in
    if c = '"' then copy_string st
    else if c = '(' && peek st 1 = '*' then copy_comment st
    else if c = '<' && starts_ident (peek st 1) then scan_tag st
    else (match quoted_string_delim st with
          | Some delim -> copy_quoted_string st ~delim
          | None -> if try_copy_char st then () else copy st)
  done

(* Public entry: transform a whole `.mlx` source string. Pure, total, allocation-light. *)
let transform (src : string) : string =
  let st = { src; len = String.length src; buf = Buffer.create (String.length src + 64); i = 0 } in
  scan_code st;
  Buffer.contents st.buf
