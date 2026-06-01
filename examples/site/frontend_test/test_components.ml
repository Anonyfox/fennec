(* Unit tests for the mlx components: render each to HTML (server-reason-react)
   and assert on the output. Lives in its own dir, NOT inside frontend/, because
   that library uses (include_subdirs qualified) which would absorb any sub-dune. *)

let fails = ref 0
let check name c = if c then Printf.printf "  ok   %s\n" name else (incr fails; Printf.printf "  FAIL %s\n" name)

(* substring search, stdlib only *)
let has hay needle =
  let nh = String.length hay and nn = String.length needle in
  let rec go i = i + nn <= nh && (String.sub hay i nn = needle || go (i + 1)) in
  nn = 0 || go 0

let render el = ReactDOM.renderToString el

let () =
  let hero = render (Frontend.Components.Hero.make ~title:"Pricing" ~subtitle:"Free in beta" ()) in
  check "hero: section class" (has hero "class=\"hero\"");
  check "hero: title text" (has hero "Pricing");
  check "hero: subtitle text" (has hero "Free in beta");
  check "hero: CTA link" (has hero "hero-cta");

  let counter = render (Frontend.Components.Counter.make ~label:"seats" ()) in
  check "counter: class" (has counter "class=\"counter\"");
  check "counter: initial label+count" (has counter "seats: 0");

  let nav = render (Frontend.Components.Nav.make ~links:[ ("/", "Home"); ("/about", "About") ] ()) in
  check "nav: home link" (has nav "href=\"/\"");
  check "nav: about label" (has nav "About");

  if !fails = 0 then print_endline "all component tests passed."
  else (Printf.printf "%d FAILED\n" !fails; exit 1)

