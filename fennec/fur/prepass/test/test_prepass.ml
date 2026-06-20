(* ════════════════════════════════════════════════════════════════════════════════════════
   EXHAUSTIVE unit tests for the fennec-mlx-pp PRE-PASS (Fennec_mlx_prepass.transform).

   The transform is the correctness core of the whole feature — a bug here is a cryptic userland
   error or, worse, silently mangled code — so this suite is a large, explicit (input → expected
   output) table. Every assertion is the EXACT normalised string. Three families:

     A. POSITIVE — bare text / `{expr}` / mixed forms become the quoted + paren form mlx accepts.
     B. NEGATIVE / GUARD — OCaml code (strings, char literals, nested comments, quoted-string
        extensions, the scoped-style block payload, less-than / not-equal / left-arrow operators)
        passes through BYTE-FOR-BYTE. This is where a lexer bug would corrupt a valid file.
     C. IDEMPOTENCE / STABILITY — the transform is the identity (hence a fixed point) on inputs
        already in the output language with no bare text / value escape — the subset the driver's
        FAST PATH relies on. Lowering `{expr}` → `(expr)` is deliberately ONE-WAY (a literal `(` is
        now input-text), applied exactly once by the pipeline; the family pins both facts.

   A second pass also feeds every expected output through the REAL mlx-pp binary (when present on
   PATH) and asserts it parses — proving we emit syntactically-valid mlx, not just a string we like.
   ════════════════════════════════════════════════════════════════════════════════════════ *)

let pass = ref 0
let fail = ref 0

let q = "\""  (* a double-quote, to keep the table readable without backslash thickets *)

(* assert transform(input) = expected, exactly *)
let t ?(name = "") input expected =
  let got = Fennec_mlx_prepass.transform input in
  if got = expected then incr pass
  else begin
    incr fail;
    Printf.printf "FAIL %s\n  in : %S\n  exp: %S\n  got: %S\n" name input expected got
  end

(* assert transform is the identity on [input] (NEGATIVE / guard cases: code must be untouched) *)
let id ?(name = "") input = t ~name:("id:" ^ name) input input

(* assert idempotence: transform(transform(input)) = transform(input) *)
let idem ?(name = "") input =
  let once = Fennec_mlx_prepass.transform input in
  let twice = Fennec_mlx_prepass.transform once in
  if once = twice then incr pass
  else begin
    incr fail;
    Printf.printf "FAIL idem:%s\n  once : %S\n  twice: %S\n" name once twice
  end

let () =
  (* ──────────────────────────────────────────────────────────────────────────────────────
     A. POSITIVE — bare text becomes a quoted string
     ────────────────────────────────────────────────────────────────────────────────────── *)
  t ~name:"plain word"
    "let v = <h1>Welcome</h1>"
    ("let v = <h1>" ^ q ^ "Welcome" ^ q ^ "</h1>");
  t ~name:"multi-word prose (keeps inner spaces)"
    "let v = <h1>Welcome to Fennec</h1>"
    ("let v = <h1>" ^ q ^ "Welcome to Fennec" ^ q ^ "</h1>");
  t ~name:"keyword 'in' as text (the canonical fail-without-prepass case)"
    "let v = <a>Sign in</a>"
    ("let v = <a>" ^ q ^ "Sign in" ^ q ^ "</a>");
  t ~name:"keywords 'fun' 'match' as text"
    "let v = <p>use fun and match here</p>"
    ("let v = <p>" ^ q ^ "use fun and match here" ^ q ^ "</p>");
  t ~name:"punctuation . , : ! ? &"
    "let v = <p>Hi, there: ok! really? a & b</p>"
    ("let v = <p>" ^ q ^ "Hi, there: ok! really? a & b" ^ q ^ "</p>");
  t ~name:"em dash and percent"
    "let v = <p>50% — done</p>"
    ("let v = <p>" ^ q ^ "50% — done" ^ q ^ "</p>");
  t ~name:"emoji (UTF-8 bytes preserved)"
    "let v = <h1>Welcome 🦊!</h1>"
    ("let v = <h1>" ^ q ^ "Welcome 🦊!" ^ q ^ "</h1>");
  t ~name:"greater-than entity-ish text (the > stays in text)"
    "let v = <p>a &gt; b</p>"
    ("let v = <p>" ^ q ^ "a &gt; b" ^ q ^ "</p>");

  (* {expr} value escape → (expr) *)
  t ~name:"brace escape -> paren"
    "let v = <span>{x}</span>"
    "let v = <span>(x)</span>";
  t ~name:"brace escape with bang-read"
    "let v = <span>{!count}</span>"
    "let v = <span>(!count)</span>";
  t ~name:"brace escape with operator + string inside"
    ("let v = <span>{label ^ " ^ q ^ ":" ^ q ^ "}</span>")
    ("let v = <span>(label ^ " ^ q ^ ":" ^ q ^ ")</span>");
  t ~name:"brace escape with nested record (inner braces preserved)"
    "let v = <li>{f { a = 1; b = 2 }}</li>"
    "let v = <li>(f { a = 1; b = 2 })</li>";
  t ~name:"brace escape with a quoted-string inside (} inside the string ignored)"
    "let v = <p>{ {|a}b|} }</p>"
    "let v = <p>( {|a}b|} )</p>";
  t ~name:"brace escape whose string contains a close brace"
    ("let v = <p>{g " ^ q ^ "}" ^ q ^ "}</p>")
    ("let v = <p>(g " ^ q ^ "}" ^ q ^ ")</p>");

  (* ──────────────────────────────────────────────────────────────────────────────────────
     A literal `(` in CHILD TEXT is just text — the ONE fennec-specific surprise we are closing.
     JSX has exactly one escape (`{}`); a paren is never code in child position, so prose with
     parentheses needs no quoting. (Pre-this-change a `(` opened a paren-escape; now it is text.)
     ────────────────────────────────────────────────────────────────────────────────────── *)
  t ~name:"literal paren in prose"
    "let v = <p>Call (now)</p>"
    ("let v = <p>" ^ q ^ "Call (now)" ^ q ^ "</p>");
  t ~name:"function-call-looking text f(x) = y"
    "let v = <p>f(x) = y</p>"
    ("let v = <p>" ^ q ^ "f(x) = y" ^ q ^ "</p>");
  t ~name:"nested parens in text"
    "let v = <p>a (nested (deep)) b</p>"
    ("let v = <p>" ^ q ^ "a (nested (deep)) b" ^ q ^ "</p>");
  t ~name:"paren at the very start of a child run"
    "let v = <p>(at start)</p>"
    ("let v = <p>" ^ q ^ "(at start)" ^ q ^ "</p>");
  t ~name:"the migrated name_form.mlx shape (parens + words)"
    "let v = <h2>Say hello (typed client form)</h2>"
    ("let v = <h2>" ^ q ^ "Say hello (typed client form)" ^ q ^ "</h2>");
  t ~name:"em dash + parens + colon + apostrophe + emoji all bare"
    "let v = <p>Call us (now) — it's free: 🦊</p>"
    ("let v = <p>" ^ q ^ "Call us (now) — it's free: 🦊" ^ q ^ "</p>");
  t ~name:"mixed: text + {expr} + text with a literal paren after the interp"
    "let v = <p>Hi {name} (v2)!</p>"
    ("let v = <p>" ^ q ^ "Hi " ^ q ^ "(name)" ^ q ^ " (v2)!" ^ q ^ "</p>");
  t ~name:"the migrated visits.mlx shape (parens + trailing-space text + {expr})"
    "let v = <p>visits (localStorage): {!n}</p>"
    ("let v = <p>" ^ q ^ "visits (localStorage): " ^ q ^ "(!n)</p>");

  (* An OCaml block comment (a paren-star opener) is STILL a comment even in child position — only a
     `(` ALONE is text. A bare `(` not followed by a star is text; a paren-star opens a comment, which
     is inter-child trivia (copied verbatim, like inter-element whitespace). *)
  t ~name:"a bare paren is text; a real block comment in child position stays a comment"
    "let v = <p>Call (now) (* a note *)</p>"
    ("let v = <p>" ^ q ^ "Call (now) " ^ q ^ "(* a note *)</p>");
  id ~name:"a block comment alone in child position stays a comment (trivia, no text child)"
    "let v = <p>(* just a comment *)</p>";

  (* mixed text + interpolation, JSX whitespace *)
  t ~name:"text then interp keeps the inline space"
    "let v = <p>Hello {name}!</p>"
    ("let v = <p>" ^ q ^ "Hello " ^ q ^ "(name)" ^ q ^ "!" ^ q ^ "</p>");
  t ~name:"interp between two texts"
    "let v = <p>a {x} b</p>"
    ("let v = <p>" ^ q ^ "a " ^ q ^ "(x)" ^ q ^ " b" ^ q ^ "</p>");
  t ~name:"trailing-space text before interp (the stats.mlx shape)"
    "let v = <div>todos in store: {n}</div>"
    ("let v = <div>" ^ q ^ "todos in store: " ^ q ^ "(n)</div>");

  (* {expr} is the value escape; a bare "str" child passes through. A `(expr)` CHILD is now TEXT
     (the contract change) — the value escape is `{get s}`, which lowers to the paren form. *)
  t ~name:"value escape {get s} -> (get s)"
    "let v = <span>{get s}</span>"
    "let v = <span>(get s)</span>";
  t ~name:"a `(expr)` child is now literal text (the closed surprise)"
    "let v = <span>(get s)</span>"
    ("let v = <span>" ^ q ^ "(get s)" ^ q ^ "</span>");
  id ~name:"quoted string child unchanged" ("let v = <h1>" ^ q ^ "Welcome" ^ q ^ "</h1>");
  t ~name:"text then {expr} value escape (the stats.mlx shape)"
    "let v = <div>todos: {n}</div>"
    ("let v = <div>" ^ q ^ "todos: " ^ q ^ "(n)</div>");

  (* nesting + components + self-closing *)
  id ~name:"self-closing component unchanged"
    "let v = <div><Counter start=3 /></div>";
  id ~name:"dotted component path unchanged"
    "let v = <Foo.Bar x=1 />";
  t ~name:"nested tags with bare text in each"
    "let v = <div><p>one</p><span>two</span></div>"
    ("let v = <div><p>" ^ q ^ "one" ^ q ^ "</p><span>" ^ q ^ "two" ^ q ^ "</span></div>");
  t ~name:"text adjacent to self-closing sibling"
    "let v = <div><br/>after</div>"
    ("let v = <div><br/>" ^ q ^ "after" ^ q ^ "</div>");
  t ~name:"text before a component"
    "let v = <div>hi <Counter/></div>"
    ("let v = <div>" ^ q ^ "hi " ^ q ^ "<Counter/></div>");
  id ~name:"empty element unchanged" "let v = <span></span>";

  (* attributes: bare text children but attribute values are OCaml simple_expr *)
  t ~name:"attr string + paren value, bare child"
    ("let v = <a href=" ^ q ^ "/x" ^ q ^ " onClick=(go ())>label</a>")
    ("let v = <a href=" ^ q ^ "/x" ^ q ^ " onClick=(go ())>" ^ q ^ "label" ^ q ^ "</a>");
  t ~name:"attr brace value -> paren value"
    "let v = <input value={draft} disabled={not ok} />"
    "let v = <input value=(draft) disabled=(not ok) />";
  id ~name:"attr punned + optional unchanged"
    "let v = <input disabled ?value />";
  (* attribute (expr) values are UNAMBIGUOUS (a slot, not prose) so they STAY a value escape — the
     back-compat alias of the canonical name={expr}. Only the CHILD changed: here it is now {x}. *)
  t ~name:"attr (expr) values stay; child uses the {expr} escape"
    "let v = <li key=(t.id) className=(cls)>{x}</li>"
    "let v = <li key=(t.id) className=(cls)>(x)</li>";

  (* multi-line bare text — JSX collapse (indentation trimmed, lines joined by one space) *)
  t ~name:"multi-line indented text collapses"
    "let v =\n  <p>\n    line one\n    line two\n  </p>"
    ("let v =\n  <p>" ^ q ^ "line one line two" ^ q ^ "</p>");
  t ~name:"whitespace-only between elements is dropped (newlines re-emitted)"
    "let v =\n  <div>\n    <p>{a}</p>\n    <p>{b}</p>\n  </div>"
    "let v =\n  <div>\n    <p>(a)</p>\n    <p>(b)</p>\n  </div>";
  (* mlx makes NO whitespace child — `<a/> <b/>` lexes to [a; b], the space is a mere token
     separator. So the pre-pass leaves the inter-element space VERBATIM (mlx then drops it), never
     quoting it into a `" "` child. *)
  id ~name:"inter-element whitespace is left verbatim (mlx drops it, no space child)"
    "let v = <div><a/> <b/></div>";
  t ~name:"inter-element whitespace before a comment is left verbatim"
    "let v = <span>{a}   (* trailing trivia *)</span>"
    "let v = <span>(a)   (* trailing trivia *)</span>";
  id ~name:"comment between children is trivia (no stray text child)"
    ("let v = <span>" ^ q ^ "x" ^ q ^ "  (* c *)  " ^ q ^ "y" ^ q ^ "</span>");

  (* RECURSION — JSX nested inside a {…} escape gets the SAME bare-text treatment, to any depth. This
     is the common real shape: a list/conditional/match {…} escape whose body is JSX with bare
     children. The {…} escape lowers to (…), and a nested <tag> inside it recurses — so userland writes
     control flow as `{each …}` / `{if …}` / `{match …}` (JSX-identical: `{}` is the ONE escape), and a
     literal `(` anywhere in the nested bare text stays text. (A pre-existing `(each …)` CHILD — old
     syntax — would now be quoted as text; userland migrated all control flow to `{…}`.) *)
  t ~name:"each-escape: bare child + {expr} inside the lambda body"
    "let v = <nav>{each links (fun (href, label) -> <a href=href>{label}</a>)}</nav>"
    "let v = <nav>(each links (fun (href, label) -> <a href=href>(label)</a>))</nav>";
  t ~name:"match-escape: bare text in one arm, {expr} in another"
    ("let v = <span>{match x with None -> <b>none</b> | Some u -> <i>{name u}</i>}</span>")
    ("let v = <span>(match x with None -> <b>" ^ q ^ "none" ^ q ^ "</b> | Some u -> <i>(name u)</i>)</span>");
  t ~name:"if-escape: jsx branches with bare text"
    ("let v = <div>{if ok then <p>yes</p> else <p>no</p>}</div>")
    ("let v = <div>(if ok then <p>" ^ q ^ "yes" ^ q ^ "</p> else <p>" ^ q ^ "no" ^ q ^ "</p>)</div>");
  t ~name:"mixed bare text + {expr} inside an each body (with a literal paren in text)"
    "let v = <ul>{each xs (fun x -> <li>row {x.id} (#{x.id})</li>)}</ul>"
    ("let v = <ul>(each xs (fun x -> <li>" ^ q ^ "row " ^ q ^ "(x.id)" ^ q ^ " (#" ^ q ^ "(x.id)" ^ q ^ ")" ^ q ^ "</li>))</ul>");
  t ~name:"deeply nested escapes (each inside each)"
    "let v = <div>{each rows (fun r -> <tr>{each r.cells (fun c -> <td>{c}</td>)}</tr>)}</div>"
    "let v = <div>(each rows (fun r -> <tr>(each r.cells (fun c -> <td>(c)</td>))</tr>))</div>";
  t ~name:"brace-escape whose expression contains a nested element"
    "let v = <div>{wrap (<b>hi</b>)}</div>"
    ("let v = <div>(wrap (<b>" ^ q ^ "hi" ^ q ^ "</b>))</div>");

  (* ──────────────────────────────────────────────────────────────────────────────────────
     B. NEGATIVE / GUARD — OCaml code must pass through byte-for-byte
     ────────────────────────────────────────────────────────────────────────────────────── *)
  id ~name:"plain let, no jsx" "let x = 1 + 2";
  id ~name:"less-than operator (spaces)" "let f a b = a < b";
  id ~name:"not-equal operator" "let f a b = a <> b";
  id ~name:"left-arrow (mutable field set)" "let () = r.x <- 5";
  id ~name:"less-equal / shift-ish operators" "let f a b = a <= b || a lsl b > 0";
  id ~name:"string literal containing a fake tag"
    ("let s = " ^ q ^ "<div>{x}</div>" ^ q);
  id ~name:"string literal containing close tag and brace-escape"
    ("let s = " ^ q ^ "</p> and {y}" ^ q);
  id ~name:"string with escaped quote inside"
    ("let s = " ^ q ^ "a \\" ^ q ^ "b" ^ q ^ " c" ^ q);
  id ~name:"char literal of a less-than" "let c = '<'";
  id ~name:"char literal of a quote" ("let c = '" ^ q ^ "'");
  t ~name:"char literal guards a later real element (char not a string/tag opener)"
    "let c = '<' in ignore c; <p>x</p>"
    ("let c = '<' in ignore c; <p>" ^ q ^ "x" ^ q ^ "</p>");
  (* nested comments with fakes inside *)
  id ~name:"comment with fake tag + string + braces"
    ("(* a <div> with " ^ q ^ "a string" ^ q ^ " and {braces} *) let x = 1");
  id ~name:"NESTED comment with fakes"
    "(* outer (* inner <span> *) still outer *) let x = 1";
  id ~name:"comment containing a close-tag-looking thing"
    "(* this </div> is text *) let x = 1";
  (* quoted-string extensions *)
  id ~name:"default-delim quoted string {|...|}"
    "let s = {|raw <div> {x}|}";
  id ~name:"js-delim quoted string"
    "let s = {js|こんにちは <b>|js}";
  id ~name:"the scoped-style block (braces/colons/semis inside untouched)"
    "[%%style {scss|\n.todo { color: #c0392b; border: 1px solid #ccc }\n.x:hover { opacity: .5 }\n|scss}]";
  id ~name:"extension node [%fields ...] with <> inside"
    "let f = [%q title <> \"\"]" ;
  id ~name:"attribute extension [@deriving ...]"
    "type t = { a : int } [@@deriving model]";

  (* a whole realistic component shell: code + style + jsx mix; only the bare child changes *)
  t ~name:"realistic component (style untouched, child quoted, interp normalised)"
    ("[%%style {scss| .hero { color: red } |scss}]\n\
      let make ~title () =\n\
     \  <section className=" ^ q ^ "hero" ^ q ^ ">\n\
     \    <h1>{title}</h1>\n\
     \    <a href=" ^ q ^ "/about" ^ q ^ ">Learn more</a>\n\
     \  </section>")
    ("[%%style {scss| .hero { color: red } |scss}]\n\
      let make ~title () =\n\
     \  <section className=" ^ q ^ "hero" ^ q ^ ">\n\
     \    <h1>(title)</h1>\n\
     \    <a href=" ^ q ^ "/about" ^ q ^ ">" ^ q ^ "Learn more" ^ q ^ "</a>\n\
     \  </section>");

  (* ──────────────────────────────────────────────────────────────────────────────────────
     C. IDEMPOTENCE / STABILITY

     The pre-pass maps the INPUT language (bare text + `{expr}`) to a DIFFERENT output language (mlx:
     quoted text + `(expr)`). It is therefore the identity — hence trivially idempotent — on exactly
     the inputs that ALREADY are in the output language with no bare text and no value escape: pure
     OCaml, fully-quoted children, nested tags, attributes. That identity subset is what the driver's
     FAST PATH keys on (pp.ml streams mlx-pp's bytes through unchanged when `transform x = x`), so
     these cases pin the contract the fast path relies on.

     Lowering `{expr}` → `(expr)` is intentionally ONE-WAY (a literal `(` is input-text now), so a
     SECOND pass over a lowered child re-quotes it — proven explicitly below. The pipeline applies the
     pre-pass exactly once, so this is correct, not a bug; the round-trip we DO guarantee is that the
     OUTPUT parses as mlx (the `parses_through_mlx` proof) and that already-mlx files are untouched. *)
  idem ~name:"bare prose (no value escape) is a fixed point"
    "let v = <h1>Welcome to Fennec</h1>";
  idem ~name:"prose with a literal paren is a fixed point (quoted once, then stable)"
    "let v = <p>Call us (now) — free!</p>";
  idem ~name:"style block" "[%%style {scss| .x { a: b } |scss}]\nlet v = <p>hi</p>";
  idem ~name:"nested + attrs (no child escape)"
    "let v = <div><Counter start=3 /><a href=\"/x\">go</a></div>";
  idem ~name:"multi-line text" "let v =\n  <p>\n    one\n    two\n  </p>";
  idem ~name:"already-quoted child + attr (expr) is a fixed point"
    ("let v = <li key=(t.id)>" ^ q ^ "todos: " ^ q ^ "</li>");

  (* the deliberate ONE-WAY lowering: `{expr}` → `(expr)` on pass 1, then `(expr)` is text on pass 2.
     This documents WHY the value-escape forms are not in the idempotent set above. *)
  t ~name:"pass 1: {expr} lowers to (expr)"
    "let v = <span>{!count}</span>"
    "let v = <span>(!count)</span>";
  t ~name:"pass 2 over that lowered (expr) re-quotes it as text (one-way lowering)"
    "let v = <span>(!count)</span>"
    ("let v = <span>" ^ q ^ "(!count)" ^ q ^ "</span>");

  (* ──────────────────────────────────────────────────────────────────────────────────────
     D. POSITION MAP — remap a byte offset / column in the PRE-PASSED output back to the ORIGINAL .mlx.

     Quoting a bare-text run adds bytes, so any source position AFTER the run (on the same line) sits
     at a larger column in the pre-passed text than in the original. The build driver / merlin reader
     undo that with {!Fennec_mlx_prepass.Posmap}, so an OCaml/mlx error after a prose run points at the
     EXACT original column (and original line — a multi-line prose run collapses, shifting later lines).
     These assertions pin the map directly: they locate a marker in BOTH the original and the output and
     check that remapping the OUTPUT offset reproduces the ORIGINAL offset, line and column. *)
  let module PM = Fennec_mlx_prepass.Posmap in
  let find s sub =                                   (* first byte index of [sub] in [s], or -1 *)
    let n = String.length s and m = String.length sub in
    let r = ref (-1) and i = ref 0 in
    while !r < 0 && !i <= n - m do (if String.sub s !i m = sub then r := !i); incr i done; !r in
  let orig_lc s cnum =                               (* (line, col) of [cnum] in [s] *)
    let l = ref 1 and b = ref 0 in
    for j = 0 to cnum - 1 do if s.[j] = '\n' then (incr l; b := j + 1) done; (!l, cnum - !b) in
  let pos_ok ~name src marker =
    let out, pm = Fennec_mlx_prepass.transform_with_map src in
    let o = find out marker and i = find src marker in
    if o < 0 || i < 0 then (incr fail; Printf.printf "FAIL pos:%s — marker not found\n" name)
    else begin
      let mapped = PM.remap_cnum pm o in
      let (oln, ocol) = orig_lc src i in
      let (mln, mbol) = PM.line_of pm mapped in
      let mcol = mapped - mbol in
      if mapped = i && mln = oln && mcol = ocol then incr pass
      else begin
        incr fail;
        Printf.printf "FAIL pos:%s\n  out off %d → remap %d (want %d)  line %d/col %d (want %d/%d)\n"
          name o mapped i mln mcol oln ocol
      end
    end in
  (* same-line code after a quoted run: its column must come back EXACTLY (the canonical case). *)
  pos_ok ~name:"ident after same-line bare text remaps to original column"
    "let v = <div>todos in store: {List.length xs}</div>" "List.length";
  pos_ok ~name:"second ident further along the same line"
    "let v = <div>todos in store: {List.length zs}</div>" "zs";
  (* a DELIBERATE type error AFTER a bare-text run (the invariant's proof case): the offending ident's
     column is reported at the TRUE original column, not the byte-shifted one. *)
  pos_ok ~name:"type-error site after prose: bad_ident column is original"
    "let v = <p>Welcome aboard: </p> and bad_ident = ()" "bad_ident";
  (* multi-line prose collapses → later lines shift UP; the map repairs BOTH line and column. *)
  pos_ok ~name:"code after a multi-line collapsed run remaps to original LINE and column"
    "let v =\n  <p>\n    multi line\n    indented text\n  </p>\nlet after = 1" "let after";
  (* identity buffer (already-quoted / no rewrite): the map is the identity → remap is a no-op. *)
  (let _, pm = Fennec_mlx_prepass.transform_with_map "let v = <p>\"hi\"</p>" in
   if PM.is_identity pm then incr pass
   else (incr fail; Printf.printf "FAIL pos: identity buffer should yield identity posmap\n"));

  Printf.printf "\nprepass unit table: %d passed, %d failed\n" !pass !fail;
  if !fail > 0 then exit 1
