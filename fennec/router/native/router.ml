(* The universal router — NATIVE (SSR) side. Maps a path pattern to a page (a
   component of its route params). Metadata is set INSIDE pages via <Head>
   (Fennec_headkit), collected during render — not threaded as a return value.

   Rendering a request is two phases, so the document <head> can reflect what the
   body declared (head precedes body in the document, but its content depends on
   the body):
     1. render the matched page through the headkit context -> body HTML + the
        merged <Head> tags (document order, inside-out wins);
     2. render the app's TEMPLATE (an .mlx component) with the head tags spliced
        as elements, the body injected as raw HTML, and the asset URLs derived
        from the app name. server-reason-react emits the <!DOCTYPE> itself.

   The SAME router type/value compiles to JS (melange/) where it drives client
   navigation + hydration. [hydrate] is a no-op here (SSR), real on the client,
   so one [main.mlx] entry works on both targets. *)

module HK = Fennec_headkit.Headkit

type page = Fennec_matcher.Matcher.params -> React.element

(* A template is an .mlx document shell: it receives the collected head (as
   elements), the rendered body (as raw HTML), and the app's asset URLs, and
   returns the full <html> element. Matches a [[@react.component] make]'s type. *)
type template =
  ?key:string ->
  head:React.element ->
  body:string ->
  runtime:string ->
  js:string ->
  css:string ->
  unit ->
  React.element

type t = { routes : (string * page) list; not_found : page option }

let make ?not_found () : t = { routes = []; not_found }
let page (pattern : string) (p : page) (t : t) : t = { t with routes = t.routes @ [ (pattern, p) ] }

(* expose the route table (shared with the client router for identical matching) *)
let routes (t : t) = t.routes

(* On the server, hydrate is a no-op: it exists only so a shared [main.mlx] can
   call [Router.hydrate router] unconditionally (it mounts on the client). *)
let hydrate (_ : t) : unit = ()

(* Build a request renderer for an app: [name] is the app's folder name, which
   fixes its predictable asset URLs (/_apps/<name>/main.{js,css}) and the shared
   runtime (/react.js). Returns [path -> full HTML document option] (None when no
   route matches and there is no not_found page — the caller then 404s). *)
let render ~(name : string) ~(template : template) (t : t) : string -> string option =
  let runtime = "/react.js" in
  let js = "/_apps/" ^ name ^ "/main.js" in
  let css = "/_apps/" ^ name ^ "/main.css" in
  fun path ->
    let chosen =
      match Fennec_matcher.Matcher.find t.routes path with
      | Some (p, params) -> Some (p params)
      | None -> ( match t.not_found with Some p -> Some (p [ ("*", path) ]) | None -> None)
    in
    match chosen with
    | None -> None
    | Some element ->
      let body, tags = HK.render_collect element in
      let head = HK.head_element tags in
      Some (ReactDOM.renderToString (template ~head ~body ~runtime ~js ~css ()))
