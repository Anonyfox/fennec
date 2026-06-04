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
  (* unrecognised text → no structured problems *)
  check "unrecognised text -> []" (D.parse "ld: symbol not found\nmake: *** error" = []);
  check "empty -> []" (D.parse "" = []);
  if !fails = 0 then print_endline "all Diagnostics tests passed." else (Printf.printf "%d FAILED\n" !fails; exit 1)
