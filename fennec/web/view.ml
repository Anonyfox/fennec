(* Server-rendered HTML views — a "dead view" is just a {!Fur} component tree rendered to a string
   with NO client bundle, no hydration seed, no JS (Fur.to_html strips handlers and runs each
   component's setup once). The SAME component language as the live isomorphic apps, so a dead view
   later "upgrades" to interactive by adding a client boot — no rewrite, just more capability. *)

module Conn = Fennec_paw.Conn

(* respond with a vnode FRAGMENT as the body (no doctype/shell) — for partials / HTMX-style swaps *)
let html ?(status = 200) (conn : Conn.t) (v : Fur.vnode) : Conn.t = Conn.html ~status conn (Fur.to_html v)

(* respond with a FULL document (prepends <!doctype html>) — for a top-level page *)
let document ?(status = 200) (conn : Conn.t) (v : Fur.vnode) : Conn.t = Conn.html ~status conn (Fur.document v)

(* a conventional <html>/<head>/<body> shell: charset + responsive viewport + title, optional inlined
   [styles] and extra [head] nodes, then [body]. Pass the result to {!document}. *)
let page ?(lang = "en") ?(title = "") ?(head = []) ?(styles = "") (body : Fur.vnode list) : Fur.vnode =
  Fur.h "html"
    [ Fur.attr "lang" lang ]
    [ Fur.h "head" []
        ([ Fur.h "meta" [ Fur.attr "charset" "utf-8" ] [];
           Fur.h "meta" [ Fur.attr "name" "viewport"; Fur.attr "content" "width=device-width, initial-scale=1" ] [];
           Fur.h "title" [] [ Fur.text title ] ]
        @ (if styles = "" then [] else [ Fur.h "style" [] [ Fur.raw styles ] ])
        @ head);
      Fur.h "body" [] body ]

(* ──────────────────────────── tests ──────────────────────────── *)

(* substring search without pulling in Str/Re for a test helper *)
let contains hay needle =
  let nh = String.length hay and nn = String.length needle in
  let rec go i = if i + nn > nh then false else if String.sub hay i nn = needle then true else go (i + 1) in
  nn = 0 || go 0

let%test "page renders a full document shell with title + body" =
  let html = Fur.document (page ~title:"Hi" [ Fur.h "h1" [] [ Fur.text "Welcome" ] ]) in
  contains html "<!doctype html>" && contains html "<title>Hi</title>" && contains html "<h1>Welcome</h1>"
  && contains html "charset=\"utf-8\""

let%test "view escapes interpolated text (no markup injection)" =
  let html = Fur.to_html (Fur.h "p" [] [ Fur.text "<script>x</script>" ]) in
  not (contains html "<script>")
