(* The handler RESPONSE side: render a Fur component tree to an HTML response through the SAME SSR
   engine that the SPA apps use (Head metadata + scoped [%%style] + a document template) but STATIC —
   no fast-render seed, no client bundle, no hydration. Plus the response niceties (redirect + flash,
   fragment, status) and the hidden-field helpers (CSRF, method override) a hand-authored .mlx form
   needs. A handler is a [Conn.t -> Conn.t] that reads typed inputs ({!Form}/{!Action}), runs its
   Pulse/Accounts logic, and answers with one of these. *)

module Conn = Fennec_paw.Conn
module Session = Fennec_server.Session
module Csrf = Fennec_server.Csrf

(* a minimal STATIC document shell — head + scoped styles + body, NO seed, NO client bundle (so the
   page is server-rendered HTML with zero JS). A handler that wants the app's look passes its own
   template (an .mlx [Doc.ctx -> vnode]) that simply omits [Doc.data]/[Doc.scripts]. *)
let default_template (ctx : Fur.Doc.ctx) : Fur.vnode =
  Fur.h "html"
    [ Fur.attr "lang" "en" ]
    [ Fur.h "head" [] [ Fur.Doc.head ctx; Fur.Doc.styles ctx ]; Fur.h "body" [] [ Fur.Doc.outlet ctx ] ]

(* render a component tree to a full HTML response via the SSR engine, STATIC (no JS/hydration).
   [~head] appends to the resolved Head metadata; [~styles] is inlined scoped CSS; [~template] is the
   document shell (default: {!default_template}). *)
let html ?(status = 200) ?(styles = "") ?(head = "") ?(template = default_template) (conn : Conn.t) (view : Fur.vnode) : Conn.t =
  let body = Fur.to_html view in
  let ctx = { Fur.Doc.head = Fur.Head.to_ssr () ^ head; data = ""; body; styles; client_js = "" } in
  Conn.html ~status conn (Fur.document (template ctx))

(* answer a vnode FRAGMENT (no document shell) — partials / progressive swaps *)
let fragment ?(status = 200) (conn : Conn.t) (view : Fur.vnode) : Conn.t = Conn.html ~status conn (Fur.to_html view)

(* redirect, optionally flashing a message into the session for the next page (post-redirect-get) *)
let redirect ?flash (conn : Conn.t) (url : string) : Conn.t =
  let conn = match flash with Some m -> Session.set conn "flash" m | None -> conn in
  Conn.redirect conn url

(* read + clear the flash set by a prior {!redirect} *)
let flash (conn : Conn.t) : Conn.t * string option =
  match Session.get conn "flash" with Some m -> (Session.delete conn "flash", Some m) | None -> (conn, None)

(* a CSRF hidden field for a hand-authored form — the token comes from the conn (pair with
   [Paw.Csrf.make]); empty when CSRF isn't active so it's harmless to include *)
let csrf_field (conn : Conn.t) : Fur.vnode =
  match Csrf.token_opt conn with
  | Some tok -> Fur.h "input" [ Fur.attr "type" "hidden"; Fur.attr "name" "_csrf_token"; Fur.attr "value" tok ] []
  | None -> Fur.frag []

(* a method-override hidden field so an HTML form (GET/POST only) can drive PUT/PATCH/DELETE (pair
   with [Paw.Method_override]) *)
let method_field (verb : string) : Fur.vnode =
  Fur.h "input" [ Fur.attr "type" "hidden"; Fur.attr "name" "_method"; Fur.attr "value" verb ] []

(* ──────────────────────────── tests ──────────────────────────── *)

module H = Fennec_core.Http

let contains hay needle =
  let nh = String.length hay and nn = String.length needle in
  let rec go i = if i + nn > nh then false else if String.sub hay i nn = needle then true else go (i + 1) in
  nn = 0 || go 0

let resp_body c = (Option.get (Conn.resp c)).H.body

let%test "html renders a static document: body present, NO seed/bundle script" =
  let c = Conn.make (H.make_request ~meth:H.GET ~path:"/" ()) in
  let out = resp_body (html c (Fur.h "main" [] [ Fur.h "h1" [] [ Fur.text "Hello" ] ])) in
  contains out "<!doctype html>" && contains out "<h1>Hello</h1>" && (not (contains out "<script")) && not (contains out "__FUR")

let%test "html escapes interpolated text (no markup injection)" =
  let c = Conn.make (H.make_request ~meth:H.GET ~path:"/" ()) in
  not (contains (resp_body (html c (Fur.h "p" [] [ Fur.text "<script>x</script>" ]))) "<script>x")

let%test "method_field emits the override input; csrf_field is empty without the paw" =
  contains (Fur.to_html (method_field "DELETE")) {|name="_method"|}
  && Fur.to_html (csrf_field (Conn.make (H.make_request ~meth:H.GET ~path:"/" ()))) = ""
