(* SSR <Head> via context sink: proves nested <Head> registration in document
   order and inside-out precedence (deepest title wins), with NO global state. *)

module HK = Fennec_headkit.Headkit
module Head = Fennec_head.Head
module C = Fennec_headkit.Head_component

let fails = ref 0

let check name cond =
  if cond then Printf.printf "  ok   %s\n" name
  else (
    incr fails;
    Printf.printf "  FAIL %s\n" name)

let contains h n =
  let nh = String.length h and nn = String.length n in
  let rec g i = i + nn <= nh && (String.sub h i nn = n || g (i + 1)) in
  nn = 0 || g 0

(* a nested component setting <Head> deep in the tree *)
let nested () =
  React.createElement "section" []
    [ C.make ~title:"deep" ~description:"from nested" (); React.string "nested-body" ]

let page () =
  React.createElement "div" [] [ C.make ~title:"shell" (); React.string "body"; nested () ]

let () =
  let html, tags = HK.render_collect (page ()) in
  print_endline "SSR head via context:";
  check "body rendered" (contains html "nested-body");
  check "collected tags" (List.length tags > 0);
  check "innermost title wins (deep)" (Head.title_of tags = Some "deep");
  check "nested description present"
    (List.exists (function Head.Meta_name ("description", "from nested") -> true | _ -> false) tags);
  check "head html has deep title" (contains (Head.to_html tags) "<title>deep</title>");
  (* fresh per render: a second render starts clean *)
  let _, tags2 = HK.render_collect (React.createElement "div" [] [ C.make ~title:"only" () ]) in
  check "second render isolated" (Head.title_of tags2 = Some "only");

  if !fails = 0 then print_endline "all headkit tests passed."
  else (
    Printf.printf "%d FAILED\n" !fails;
    exit 1)
