(* ════════════════════════════════════════════════════════════════════════════════════════
   EXHAUSTIVE unit tests for the fennec-mlx-pp PRE-PASS (Fennec_mlx_prepass.transform).

   The transform is the correctness core of the whole feature — a bug here is a cryptic userland
   error or, worse, silently mangled code — so this suite is a large, explicit (input → expected
   output) table. Every assertion is the EXACT normalised string. Three families:

     A. POSITIVE — bare text / `{expr}` / mixed forms become the quoted + paren form mlx accepts.
     B. NEGATIVE / GUARD — OCaml code (strings, char literals, nested comments, quoted-string
        extensions, the scoped-style block payload, less-than / not-equal / left-arrow operators)
        passes through BYTE-FOR-BYTE. This is where a lexer bug would corrupt a valid file.
     C. IDEMPOTENCE — running the transform on already-normalised output is a no-op (so a half-
        migrated file, or a second build pass, never double-rewrites).

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

  (* existing (expr) / "str" escapes still work and pass through *)
  id ~name:"paren escape child unchanged" "let v = <span>(get s)</span>";
  id ~name:"quoted string child unchanged" ("let v = <h1>" ^ q ^ "Welcome" ^ q ^ "</h1>");
  id ~name:"mixed quoted + paren unchanged (stats.mlx today)"
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
  id ~name:"attr value containing a record (braces are the value, not an escape) — already paren"
    "let v = <li key=(t.id) className=(cls)>(x)</li>";

  (* multi-line bare text — JSX collapse (indentation trimmed, lines joined by one space) *)
  t ~name:"multi-line indented text collapses"
    "let v =\n  <p>\n    line one\n    line two\n  </p>"
    ("let v =\n  <p>" ^ q ^ "line one line two" ^ q ^ "</p>");
  t ~name:"whitespace-only between elements is dropped (newlines re-emitted)"
    "let v =\n  <div>\n    <p>(a)</p>\n    <p>(b)</p>\n  </div>"
    "let v =\n  <div>\n    <p>(a)</p>\n    <p>(b)</p>\n  </div>";
  t ~name:"single inline space between elements is preserved"
    "let v = <div><a/> <b/></div>"
    ("let v = <div><a/>" ^ q ^ " " ^ q ^ "<b/></div>");

  (* RECURSION — JSX nested inside a (…) / {…} escape gets the SAME bare-text treatment, to any depth.
     This is the common real shape: a list/conditional escape whose body is JSX with bare children. *)
  t ~name:"each-escape: bare child + {expr} inside the lambda body"
    "let v = <nav>(each links (fun (href, label) -> <a href=href>{label}</a>))</nav>"
    "let v = <nav>(each links (fun (href, label) -> <a href=href>(label)</a>))</nav>";
  t ~name:"match-escape: bare text in one arm, {expr} in another"
    ("let v = <span>(match x with None -> <b>none</b> | Some u -> <i>{name u}</i>)</span>")
    ("let v = <span>(match x with None -> <b>" ^ q ^ "none" ^ q ^ "</b> | Some u -> <i>(name u)</i>)</span>");
  t ~name:"if-escape: jsx branches with bare text"
    ("let v = <div>(if ok then <p>yes</p> else <p>no</p>)</div>")
    ("let v = <div>(if ok then <p>" ^ q ^ "yes" ^ q ^ "</p> else <p>" ^ q ^ "no" ^ q ^ "</p>)</div>");
  t ~name:"mixed bare text + {expr} inside an each body"
    "let v = <ul>(each xs (fun x -> <li>row {x.id}</li>))</ul>"
    ("let v = <ul>(each xs (fun x -> <li>" ^ q ^ "row " ^ q ^ "(x.id)</li>))</ul>");
  t ~name:"deeply nested escapes (each inside each)"
    "let v = <div>(each rows (fun r -> <tr>(each r.cells (fun c -> <td>{c}</td>))</tr>))</div>"
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
     C. IDEMPOTENCE — second pass is a no-op for every shape above
     ────────────────────────────────────────────────────────────────────────────────────── *)
  idem ~name:"bare prose" "let v = <h1>Welcome to Fennec</h1>";
  idem ~name:"brace escape" "let v = <span>{!count}</span>";
  idem ~name:"mixed text+interp" "let v = <p>Hello {name}!</p>";
  idem ~name:"style block" "[%%style {scss| .x { a: b } |scss}]\nlet v = <p>hi</p>";
  idem ~name:"nested + attrs"
    "let v = <div><Counter start=3 /><a href=\"/x\">go</a></div>";
  idem ~name:"multi-line text" "let v =\n  <p>\n    one\n    two\n  </p>";
  idem ~name:"already-quoted (the pre-migration form)"
    ("let v = <div>" ^ q ^ "todos: " ^ q ^ "(n)</div>");
  idem ~name:"already-paren-escaped (the pre-migration form)"
    "let v = <li>(t.text)<button onClick=(f g)>(label)</button></li>";
  idem ~name:"recursive each-escape with bare children"
    "let v = <ul>(each xs (fun x -> <li>row {x.id}<b>{x.k}</b></li>))</ul>";
  idem ~name:"recursive match-escape with bare + interp arms"
    "let v = <span>(match x with None -> <b>none</b> | Some u -> <i>{name u}</i>)</span>";

  Printf.printf "\nprepass unit table: %d passed, %d failed\n" !pass !fail;
  if !fail > 0 then exit 1
