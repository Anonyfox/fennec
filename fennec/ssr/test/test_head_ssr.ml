(* Native SSR collector test — the load-bearing one. Proves that <Head> tags
   registered both at TOP level and inside a NESTED [@react.component] body are
   collected (the nested case is the one that's easy to get wrong), with
   inside-out precedence (deeper title wins), and that render_collect returns the
   correct body HTML alongside the merged tags. *)

module H = Fennec_ssr.Head
module Head = Fennec_head.Head

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

(* a nested component whose body mounts a <Head> (registration at RENDER time) *)
let nested () =
  React.createElement "section" []
    [ H.make ~title:"deep" ~description:"from nested" (); React.string "nested-body" ]

(* the page: a top-level <Head> (registration at CONSTRUCTION time) + a nested
   component that also sets <Head> *)
let page () =
  React.createElement "div" [] [ H.make ~title:"shell" (); React.string "body"; nested () ]

let () =
  let html, tags = H.render_collect page in
  print_endline "SSR collect:";
  check "body rendered" (String.length html > 0);
  check "body has nested content" (contains html "nested-body");
  check "collected some tags" (List.length tags > 0);
  (* inside-out: the nested (deeper) title "deep" wins over the shell title *)
  check "innermost title wins" (Head.title_of tags = Some "deep");
  check "nested description collected"
    (List.exists (function Head.Meta_name ("description", "from nested") -> true | _ -> false) tags);
  let head_html = Head.to_html tags in
  check "head html has deep title" (contains head_html "<title>deep</title>");

  if !fails = 0 then print_endline "all SSR head tests passed."
  else (
    Printf.printf "%d FAILED\n" !fails;
    exit 1)
