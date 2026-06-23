(* ════════════════════════════════════════════════════════════════════════════════════════
   fennec .mlx PRE-PASS — bare JSX text for fennec .mlx, as a source-to-source transform.

   This is NOT a fork of the mlx OCaml parser. It is a small, fennec-owned LEXER that transforms a
   fennec-flavoured .mlx (React/Svelte-style: bare text children + brace interpolation) into the
   exact form the mlx grammar accepts (quoted-string children + paren escapes). `fennec mlx-pp`
   (Fennec_mlx_cli) runs THIS first, then hands the result to the VENDORED mlx parser (Fennec_mlx,
   in fennec/fur/prepass/vendor/) — so mlx does the real parse, entirely in-process.

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
                   bare text (a literal `(` included) becomes a quoted string; a brace-escape `{expr}`
                   — the SOLE value escape, JSX-identical — becomes a paren-escape; a string / nested
                   tag passes through; a `(* … *)` block comment stays a comment. A closing tag ends it.
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
   THE CHILD-TEXT CONTRACT (what bare text means)  — EXACTLY JSX, no fennec-added surprise
   ────────────────────────────────────────────────────────────────────────────────────────
   mlx parses JSX children as a list of simple_expr. That already accepts a paren-escape, a string, a
   nested tag, and even a lone identifier — but it CANNOT accept prose (Welcome to Fennec: the word
   "to" is a keyword, a syntax error; "a b": silently two idents). So in Children we adopt the React
   rule — and crucially the SAME escape surface as JSX, which has exactly one value escape (`{}`):

     - a run that does not start with a brace, double-quote or less-than is TEXT — and a literal `(`
       is just text (JSX's only escape is `{}`; we add none). It is collected up to the next brace /
       quote / tag, whitespace-collapsed (JSX-style: inner whitespace runs collapse to one space; a
       run touching an element edge is trimmed on that side; an all-whitespace run between two
       elements is dropped), and emitted as a quoted OCaml string — so `Call us (now) — free!` is one
       text run, parens and dash and bang and all;
     - a brace-escape `{expr}` is the SOLE value escape — JSX-identical — normalised to a paren-escape
       (a bare brace in mlx is a RECORD, never a child escape, so this rewrite is mandatory, not
       cosmetic; the downstream fur ppx then auto-wraps the `(expr)` in `node`);
     - a string and a nested tag pass through unchanged; a `(* … *)` block comment stays a comment.

   The ONLY things needing care in bare text are EXACTLY JSX's: a literal `{` or a `<letter` (escape
   them, or use the quoted form), and significant / double / edge whitespace (HTML-standard — whitespace
   collapses, so force it with the quoted form). Text needing any of those uses the explicit
   double-quoted-string escape hatch, copied verbatim. Zero fennec-specific surprise. This is the
   documented fur.mli contract: collapse = HTML, `{`/`<` = JSX, `(` = text.

   (Back-compat note: a pre-existing `(expr)` *child* — the old fennec syntax, before `{expr}` became
   the one escape — now reads as literal text. That is the intended contract change; userland migrated
   every child to `{expr}`. A `name=(expr)` ATTRIBUTE value is still a value escape: an attribute slot
   is unambiguously an expression, not prose, so it adds no surprise and stays a back-compat alias of
   the canonical `name={expr}`.)

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

(* A REPLACED text run — the ONLY length-changing edit the pre-pass makes. A bare-text child run at
   input bytes [in_off, in_off+in_len) is emitted as a quoted/collapsed OCaml string literal at output
   bytes [out_off, out_off+out_len). Everywhere else the transform is byte-for-byte length-preserving
   (a verbatim copy, a dropped-whitespace run re-emitted as-is, or a `{`→`(` / `}`→`)` swap — all 1:1),
   so the input↔output byte correspondence is the identity SHIFTED by the cumulative (in_len − out_len)
   of the replaced runs seen so far. {!Posmap} reconstructs the exact original byte offset (hence the
   exact original line/column) of any output offset from this list alone — see its docs. *)
type repl = { out_off : int; out_len : int; in_off : int; in_len : int }

type st = {
  src : string;
  len : int;
  buf : Buffer.t;
  mutable i : int;            (* read cursor *)
  mutable repls : repl list;  (* replaced text runs, in REVERSE order (newest first) *)
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
   CHILDREN — the rewriting heart — plus the two escape scanners, all mutually recursive with the
   tag scanner so the pre-pass is FULLY recursive: an escape holds OCaml, which may itself hold a
   nested JSX element (an `(each … <li>…</li>)` / `(match x with … -> <span>…</span>)`), whose
   children get the SAME bare-text treatment, whose children may hold more escapes, and so on. A
   `<ident` encountered while scanning an escape is therefore handed to {!scan_tag}; everything else
   in the escape (strings, comments, quoted-strings, records, operators) is copied verbatim and
   balanced with full literal awareness (so a close-brace or close-paren inside a string, or inside a
   CSS quoted-string extension, never closes the escape early).
   ════════════════════════════════════════════════════════════════════════════════════════ *)

(* JSX whitespace collapse for one bare-text run — the canonical React/Babel algorithm
   (`cleanJSXElementLiteralChild`), which is what the components' inline `let%test` HTML assertions
   check:

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

let is_ws c = is_hblank c || c = '\n' || c = '\r'

let collapse_text text =
  (* A run with NO non-whitespace char is pure inter-child trivia — and mlx makes NO whitespace child
     (its lexer treats blanks/newlines purely as token separators, even a single space between two
     elements: `<a/> <b/>` lexes to [a; b], never [a; " "; b]). So drop it (the caller re-emits the
     bytes verbatim, which mlx then skips — preserving source positions). This also covers the blank
     line between elements. Only a run with real text survives the Babel collapse below. *)
  if String.for_all is_ws text then None
  else
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
      | Some lit ->
        (* the SOLE length-changing edit: a bare-text run [in_off,in_off+in_len) → a quoted/collapsed
           literal. Record the in↔out byte spans for {!Posmap}. Every byte added to [pend] in the loop
           below advanced [st.i] by one (each `Buffer.add_char pend …` is paired with a `skip st`), so
           the run started at exactly [st.i − in_len]. The output literal starts at the current buffer
           length. (`{`→`(` rewrites are 1:1 and a dropped-whitespace run is copied verbatim — neither
           is recorded, because neither shifts a byte.) *)
        let in_len = String.length text in
        let in_off = st.i - in_len in
        let out_off = Buffer.length st.buf in
        emit_s st lit;
        st.repls <- { out_off; out_len = String.length lit; in_off; in_len } :: st.repls
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
    else if c = '(' && peek st 1 = '*' then begin
      (* an OCaml block comment in child position is trivia, not text — copy it verbatim (it is
         dropped by mlx as inter-child whitespace would be). Only a paren-star opens a comment; a
         bare `(` falls through to the text branch below. Pathological-but-safe: nobody writes a
         block comment as a JSX child, but if they do it stays a comment, exactly like in OCaml. *)
      flush ();
      copy_comment st
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

(* copy a balanced delimited region `op … cl` (literal-aware), assuming cursor on the opener. Used
   for an ATTRIBUTE `name=(expr)` value — the only place a `(…)` is still an escape (in CHILD position
   a `(` is plain text now; the child value escape is `{expr}`, handled by {!rewrite_brace_escape}).
   RECURSES into a nested JSX element (a `<ident` whose `<` is not an operator), so bare text inside an
   attribute expression that itself produces JSX is rewritten too. Returns with the cursor just past
   the matching close. The nested element consumes its OWN `(`/`)`/`{`/`}`, so they do not perturb
   this scanner's [depth]. *)
and copy_balanced st ~op ~cl =
  copy st (* opener *);
  let depth = ref 1 in
  while !depth > 0 && st.i < st.len do
    let c = cur st in
    if c = '"' then copy_string st
    else if c = '(' && peek st 1 = '*' then copy_comment st
    else if c = '<' && starts_ident (peek st 1) then scan_tag st   (* nested JSX element *)
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

(* copy a balanced `{ … }` but EMIT it as `( … )` (the value-escape rewrite). Literal-aware so a `}`
   inside a string / quoted-string in the expr does not close early; inner record-literal `{ }` are
   balanced and copied as-is (only the OUTERMOST pair is the escape). Also RECURSES into a nested JSX
   element inside the expression. *)
and rewrite_brace_escape st =
  skip st (* drop the opening { *);
  emit st '(';
  let depth = ref 1 in
  while !depth > 0 && st.i < st.len do
    let c = cur st in
    if c = '"' then copy_string st
    else if c = '(' && peek st 1 = '*' then copy_comment st
    else if c = '<' && starts_ident (peek st 1) then scan_tag st   (* nested JSX element *)
    else (match quoted_string_delim st with
          | Some delim -> copy_quoted_string st ~delim
          | None ->
            if try_copy_char st then ()
            else if c = '{' then (incr depth; copy st)
            else if c = '}' then (decr depth; if !depth = 0 then (skip st; emit st ')') else copy st)
            else copy st)
  done

(* ════════════════════════════════════════════════════════════════════════════════════════
   CODE — the outermost driver. Copies OCaml verbatim, entering the shared literal scanners and, on a
   JSX open tag, [scan_tag]. scan_tag/scan_children own that sub-tree and return to Code when the
   element closes.
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

(* ════════════════════════════════════════════════════════════════════════════════════════
   POSITION MAP — remap a byte offset in the PRE-PASSED (output) source back to the ORIGINAL .mlx.

   The pre-pass changes byte lengths in exactly ONE place: a bare-text child run becomes a quoted /
   whitespace-collapsed OCaml string literal (see {!repl}). Every other edit is length-preserving
   (`{`→`(`, `}`→`)`, verbatim copy, verbatim re-emit of dropped whitespace). So for any output byte
   offset [o], the corresponding input offset is [o] PLUS the cumulative (in_len − out_len) of every
   replaced run lying entirely before [o]; if [o] falls INSIDE a replaced run, it is clamped into that
   run's input span (the interior of a synthesized string literal has no exact pre-image — but its two
   ENDS map exactly, which is what matters: a real error never points strictly inside the quotes).

   Crucially this also repairs LINE numbers: a multi-line prose run collapses to one line, so output
   lines after it are shifted up; recomputing the line/col from the INPUT source at the remapped input
   offset restores the true original line AND column. (When there are no replaced runs — every non-JSX
   or already-quoted file — the map is the identity and remapping is skipped wholesale.) *)
module Posmap = struct
  (* replaced runs sorted ASCENDING by [out_off] (contiguous, non-overlapping by construction). *)
  type t = {
    repls : repl array;     (* ascending by out_off; empty ⇒ identity *)
    src : string;           (* the ORIGINAL .mlx, for the line/col recompute *)
  }

  let is_identity (t : t) = Array.length t.repls = 0

  (* output byte offset → input byte offset. Linear-ish: the array is tiny (one entry per prose run),
     so a forward scan accumulating the shift is both simplest and cache-friendly. *)
  let remap_cnum (t : t) (o : int) : int =
    let n = Array.length t.repls in
    let shift = ref 0 and k = ref 0 and result = ref o in
    let stop = ref false in
    while (not !stop) && !k < n do
      let r = t.repls.(!k) in
      if o < r.out_off then
        (* before this run — the accumulated shift from earlier runs is final *)
        (result := o + !shift; stop := true)
      else if o < r.out_off + r.out_len then begin
        (* inside the synthesized literal — clamp into the run's input span (ends map exactly) *)
        let d = o - r.out_off in
        result := r.in_off + (if d > r.in_len then r.in_len else d);
        stop := true
      end
      else begin
        (* past this run entirely — fold in its length delta and continue *)
        shift := !shift + (r.in_len - r.out_len);
        incr k;
        result := o + !shift   (* tentative: correct if no later run precedes o *)
      end
    done;
    if !result < 0 then 0 else !result

  (* recompute (pos_lnum, pos_bol) for an INPUT byte offset by counting newlines in [src] up to it.
     Returns (lnum, bol). Lines are 1-based; bol is the offset of the start of the line. Linear in the
     offset, but only ever called on the handful of positions an AST location carries on a shifted
     line — never on the whole file. *)
  let line_of (t : t) (icnum : int) : int * int =
    let s = t.src in
    let bound = if icnum > String.length s then String.length s else icnum in
    let lnum = ref 1 and bol = ref 0 in
    for j = 0 to bound - 1 do
      if String.unsafe_get s j = '\n' then (incr lnum; bol := j + 1)
    done;
    (!lnum, !bol)

  (* remap one Lexing.position. [pos_cnum] is authoritative (an absolute byte offset); recompute
     [pos_lnum]/[pos_bol] from the input so the column [pos_cnum − pos_bol] is the ORIGINAL column. *)
  let remap_pos (t : t) (p : Lexing.position) : Lexing.position =
    let cnum = remap_cnum t p.Lexing.pos_cnum in
    let (lnum, bol) = line_of t cnum in
    { p with Lexing.pos_cnum = cnum; pos_lnum = lnum; pos_bol = bol }

  (* ── FORWARD direction: ORIGINAL byte offset → TRANSFORMED (pre-passed) byte offset. The inverse of
     {!remap_cnum}; used by the merlin reader to translate a completion CURSOR position (which Merlin
     gives in original-buffer coordinates) into the transformed buffer the stock child reader parses.
     Same structure: accumulate (out_len − in_len) for every replaced run lying before the input offset;
     a position inside a run's INPUT span clamps into that run's OUTPUT span. *)
  let fwd_cnum (t : t) (i : int) : int =
    let n = Array.length t.repls in
    let shift = ref 0 and k = ref 0 and result = ref i and stop = ref false in
    while (not !stop) && !k < n do
      let r = t.repls.(!k) in
      if i < r.in_off then (result := i + !shift; stop := true)
      else if i < r.in_off + r.in_len then begin
        let d = i - r.in_off in
        result := r.out_off + (if d > r.out_len then r.out_len else d);
        stop := true
      end
      else (shift := !shift + (r.out_len - r.in_len); incr k; result := i + !shift)
    done;
    if !result < 0 then 0 else !result

  (* forward-remap a Lexing.position into the transformed buffer; only [pos_cnum] is consumed downstream
     (the stock reader recomputes its own line table), but we keep [pos_lnum]/[pos_bol] coherent too by
     recomputing them against the TRANSFORMED text the caller still holds — here we leave them as the
     caller's (the cnum is what the child uses), which is exact for any line not touched by a collapse. *)
  let fwd_pos (t : t) (p : Lexing.position) : Lexing.position =
    if is_identity t then p
    else { p with Lexing.pos_cnum = fwd_cnum t p.Lexing.pos_cnum }
end

(* shared scanner core — returns the output AND the replaced-run list (ascending). *)
let scan (src : string) : string * repl array =
  let st = { src; len = String.length src; buf = Buffer.create (String.length src + 64);
             i = 0; repls = [] } in
  scan_code st;
  (* [st.repls] is newest-first; the array is ascending by [out_off] ⇒ reverse. *)
  (Buffer.contents st.buf, Array.of_list (List.rev st.repls))

(* Public entry: transform a whole `.mlx` source string. Pure, total, allocation-light. The
   position-map is discarded — callers needing column-exact error remapping use {!transform_with_map}.*)
let transform (src : string) : string = fst (scan src)

(* Like {!transform}, but also returns a {!Posmap.t} that maps any byte offset in the produced output
   back to the original source — used by the driver / merlin reader to remap mlx's AST locations to
   column-exact ORIGINAL positions. The map is the identity (and {!Posmap.is_identity} is true) iff no
   bare-text run was rewritten, i.e. exactly when [transform src = src]. *)
let transform_with_map (src : string) : string * Posmap.t =
  let (out, repls) = scan src in
  (out, { Posmap.repls; src })
