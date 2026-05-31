(* Unit tests for Fennec_core.Head — the pure merge/dedup/escape core. Heavy on
   edge cases: precedence (last/innermost wins), per-key dedup, repeatable links,
   XSS escaping (a title/value must not break out of its element/attribute), and
   ordering stability. *)

module Head = Fennec_head.Head

let fails = ref 0

let check name cond =
  if cond then Printf.printf "  ok   %s\n" name
  else (
    incr fails;
    Printf.printf "  FAIL %s\n" name)

let eq name a b = check name (a = b)

let contains hay needle =
  let nl = String.length needle and hl = String.length hay in
  let rec go i = i + nl <= hl && (String.sub hay i nl = needle || go (i + 1)) in
  nl = 0 || go 0

let () =
  print_endline "merge — precedence (last/innermost wins):";
  (* two titles: the later one wins *)
  eq "title last wins"
    (Head.merge [ Head.Title "outer"; Head.Title "inner" ])
    [ Head.Title "inner" ];
  (* same meta name dedups to last *)
  eq "meta name dedup"
    (Head.merge [ Head.Meta_name ("description", "a"); Head.Meta_name ("description", "b") ])
    [ Head.Meta_name ("description", "b") ];
  (* same og property dedups *)
  eq "og property dedup"
    (Head.merge [ Head.Meta_property ("og:title", "a"); Head.Meta_property ("og:title", "b") ])
    [ Head.Meta_property ("og:title", "b") ];
  (* canonical single *)
  eq "canonical dedup"
    (Head.merge [ Head.Canonical "/a"; Head.Canonical "/b" ])
    [ Head.Canonical "/b" ];
  (* charset single *)
  eq "charset dedup"
    (Head.merge [ Head.Charset "utf-8"; Head.Charset "latin1" ])
    [ Head.Charset "latin1" ];

  print_endline "merge — distinct keys coexist:";
  let m = Head.merge [
    Head.Title "T";
    Head.Meta_name ("description", "d");
    Head.Meta_property ("og:image", "/og.png");
    Head.Meta_name ("keywords", "k");
  ] in
  check "4 distinct tags kept" (List.length m = 4);

  print_endline "merge — repeatable links keyed by rel+href:";
  let links = Head.merge [
    Head.Link [ ("rel", "alternate"); ("href", "/en") ];
    Head.Link [ ("rel", "alternate"); ("href", "/de") ];
    Head.Link [ ("rel", "alternate"); ("href", "/en") ]; (* exact dup of #1 *)
  ] in
  check "two distinct alternates, dup collapsed" (List.length links = 2);

  print_endline "merge — ordering stability (first-appearance order, last value):";
  let ordered = Head.merge [
    Head.Title "first";
    Head.Meta_name ("description", "d1");
    Head.Title "second"; (* updates title in place, doesn't move it *)
  ] in
  eq "title stays first, value updated" (List.hd ordered) (Head.Title "second");
  check "description still present" (List.mem (Head.Meta_name ("description", "d1")) ordered);

  print_endline "to_html — escaping (XSS):";
  let evil = Head.to_html [ Head.Title "</title><script>alert(1)</script>" ] in
  check "title cannot break out" (not (contains evil "<script>"));
  check "title escapes < >" (contains evil "&lt;" && contains evil "&gt;");
  let evil_attr = Head.to_html [ Head.Meta_name ("description", "\"><img src=x onerror=alert(1)>") ] in
  check "attr cannot break out of quotes" (not (contains evil_attr "<img"));
  check "attr escapes quote" (contains evil_attr "&quot;");
  let evil_amp = Head.to_html [ Head.Meta_name ("description", "a & b") ] in
  check "ampersand escaped" (contains evil_amp "&amp;");

  print_endline "to_html — well-formed output:";
  let html = Head.to_html [
    Head.Charset "utf-8";
    Head.Title "Page";
    Head.Meta_name ("description", "A page");
    Head.Meta_property ("og:image", "/og.png");
    Head.Canonical "https://x.example/p";
  ] in
  check "has title element" (contains html "<title>Page</title>");
  check "has charset" (contains html "charset=\"utf-8\"");
  check "has meta name" (contains html "name=\"description\"");
  check "has og property" (contains html "property=\"og:image\"");
  check "has canonical link" (contains html "rel=\"canonical\"");

  print_endline "title_of:";
  eq "picks last title" (Head.title_of [ Head.Title "a"; Head.Title "b" ]) (Some "b");
  eq "none when no title" (Head.title_of [ Head.Charset "utf-8" ]) None;

  print_endline "edge — empty:";
  eq "empty merge" (Head.merge []) [];
  eq "empty to_html" (Head.to_html []) "";

  if !fails = 0 then print_endline "all Head tests passed."
  else (
    Printf.printf "%d FAILED\n" !fails;
    exit 1)
