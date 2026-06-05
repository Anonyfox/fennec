(* Diagnostics: parse dune's OCaml diagnostic text into placed problems; degrade to [] on an
   unrecognised format (so the UI falls back to the raw text). *)

module D = Fennec_dev.Diagnostics

let fails = ref 0
let check name c = if c then Printf.printf "  ok   %s\n" name else (incr fails; Printf.printf "  FAIL %s\n" name)

let sample =
  "File \"frontend/apps/web/index.mlx\", line 11, characters 8-16:\n\
  \  11 |     <h1>{greeting}</h1>\n\
  \             ^^^^^^^^\n\
   Error: Unbound value greeting\n"

let contains hay needle =
  let lh = String.length hay and ln = String.length needle in
  let rec go i = i + ln <= lh && (String.sub hay i ln = needle || go (i + 1)) in
  ln = 0 || go 0

let () =
  print_endline "Diagnostics.parse:";
  let ps = D.parse sample in
  check "one problem parsed" (List.length ps = 1);
  (match ps with
  | [ p ] ->
    check "file" (p.D.file = "frontend/apps/web/index.mlx");
    check "line" (p.D.line = 11);
    check "col is 1-based (characters 8 -> col 9)" (p.D.col = 9);
    check "severity Error" (p.D.severity = D.Error);
    check "message captured" (contains p.D.message "Unbound value greeting");
    check "excerpt kept" (List.length p.D.excerpt >= 1)
  | _ -> incr fails);
  check "count = (1 error, 0 warnings)" (D.count ps = (1, 0));
  (* ANSI-coloured diagnostics still parse *)
  let coloured = "\027[1mFile \"a.ml\", line 3, characters 0-1:\027[0m\nError: oops\n" in
  check "ANSI-wrapped diagnostic parses" (match D.parse coloured with [ p ] -> p.D.file = "a.ml" && p.D.line = 3 | _ -> false);
  (* a warning (warnings-as-errors in dev still come through as text) *)
  let warn = "File \"b.ml\", line 1, characters 0-1:\nWarning 26 [unused-var]: unused variable x\n" in
  check "warning severity" (match D.parse warn with [ p ] -> p.D.severity = D.Warning | _ -> false);
  (* a SYNTAX error: a second File block ("might be unmatched") is a hint for the SAME error, so
     it must fold into [related] and count as ONE, not two *)
  let syntax =
    "File \"layout.mlx\", line 13, characters 5-7:\n\
     Error: Syntax error: ']' expected\n\
     File \"layout.mlx\", line 7, characters 15-16:\n\
    \  This '[' might be unmatched\n"
  in
  let sp = D.parse syntax in
  check "a multi-location syntax error is ONE problem" (List.length sp = 1);
  check "and counts as ONE error (not two)" (D.count sp = (1, 0));
  (match sp with
  | [ p ] ->
    check "primary location is line 13" (p.D.line = 13);
    check "the secondary location is folded into related" (List.exists (fun s -> contains s "might be unmatched") p.D.related)
  | _ -> incr fails);
  (* two genuinely-distinct errors → two problems *)
  let two = "File \"a.ml\", line 1, characters 0-1:\nError: one\nFile \"b.ml\", line 2, characters 0-1:\nError: two\n" in
  check "two real errors -> count (2,0)" (D.count (D.parse two) = (2, 0));
  (* a path that ITSELF contains "line"/"characters" must not corrupt the location: the fields are
     read from after the path, not the whole line. (Regression: timeline2 -> line 2, char3 -> col 4.) *)
  let trapline = "File \"frontend/timeline2/x.ml\", line 7, characters 9-16:\nError: boom\n" in
  (match D.parse trapline with
  | [ p ] ->
    check "path with 'line' substring -> real line 7" (p.D.line = 7);
    check "path with 'line' substring -> real col 10" (p.D.col = 10);
    check "path with 'line' substring -> file intact" (p.D.file = "frontend/timeline2/x.ml")
  | _ -> incr fails);
  let trapchars = "File \"src/characters3/y.ml\", line 4, characters 2-3:\nError: boom\n" in
  (match D.parse trapchars with
  | [ p ] ->
    check "path with 'characters' substring -> real line 4" (p.D.line = 4);
    check "path with 'characters' substring -> real col 3" (p.D.col = 3)
  | _ -> incr fails);
  (* unrecognised text → no structured problems *)
  check "unrecognised text -> []" (D.parse "ld: symbol not found\nmake: *** error" = []);
  check "empty -> []" (D.parse "" = []);
  if !fails = 0 then print_endline "all Diagnostics tests passed." else (Printf.printf "%d FAILED\n" !fails; exit 1)
