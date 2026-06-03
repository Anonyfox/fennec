(* Unit tests for the mlx components: render each to an HTML string (Fur.to_html, the
   native SSR path) and assert on the output. Lives in its own dir, NOT inside
   frontend/, whose route_gen rule + data_only_dirs would otherwise absorb it.

   A Fur component is [props -> unit -> (unit -> vnode)]: applying the props + unit
   runs setup and returns the render thunk; calling it yields the vnode to render. *)

let fails = ref 0
let check name c = if c then Printf.printf "  ok   %s\n" name else (incr fails; Printf.printf "  FAIL %s\n" name)

(* substring search, stdlib only *)
let has hay needle =
  let nh = String.length hay and nn = String.length needle in
  let rec go i = i + nn <= nh && (String.sub hay i nn = needle || go (i + 1)) in
  nn = 0 || go 0

let render (thunk : unit -> Fur.vnode) = Fur.to_html (thunk ())

let () =
  let hero = render (Hero.make ~title:"Pricing" ~subtitle:"Free in beta" ()) in
  check "hero: section class" (has hero "class=\"hero\"");
  check "hero: title text" (has hero "Pricing");
  check "hero: subtitle text" (has hero "Free in beta");
  check "hero: CTA link" (has hero "hero-cta");

  let counter = render (Counter.make ~label:"seats" ()) in
  check "counter: class" (has counter "class=\"counter\"");
  check "counter: label" (has counter "seats:");
  check "counter: initial count 0" (has counter ">0<");
  check "counter: inline scope attr" (has counter "data-fur=");

  let nav = render (Nav.make ~links:[ ("/", "Home"); ("/about", "About") ] ()) in
  check "nav: home link" (has nav "href=\"/\"");
  check "nav: about label" (has nav "About");

  (* data: with no SSR source set, Data renders the fallback + loading + refetch button *)
  let greeting = render (Greeting.make ()) in
  check "greeting: fallback shown" (has greeting "…");
  check "greeting: refetch button" (has greeting "id=\"refetch\"");

  (* global store: empty at start, Stats reflects it *)
  let stats = render (Stats.make ()) in
  check "stats: reads store count" (has stats "todos in store: 0");

  (* forms: controlled input + add button render server-side *)
  let todos = render (Todo_list.make ()) in
  check "todo_list: input present" (has todos "id=\"todo-input\"");
  check "todo_list: add button" (has todos "id=\"add\"");

  if !fails = 0 then print_endline "all component tests passed."
  else (Printf.printf "%d FAILED\n" !fails; exit 1)
