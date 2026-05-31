(* The universal router — NATIVE (SSR) side. Maps a path pattern to a page (a
   component of its route params), with a default document layout used only during
   SSR. Rendering: match the path, render the page through the headkit context
   (collecting its <Head> tags in document order), then hand the body HTML + merged
   head to the layout to produce the full document. The SAME router type compiles
   to JS (melange/) where it drives client-side navigation + hydration.

   A [page] is [Fennec_matcher.Matcher.params -> React.element]; metadata is set INSIDE the page
   via <Head> (Fennec_headkit), so head is just part of rendering — not a separate
   return value to thread. *)

module Head = Fennec_head.Head
module HK = Fennec_headkit.Headkit

type page = Fennec_matcher.Matcher.params -> React.element

(* a layout turns the collected head + rendered body into a full HTML document.
   It is a plain function (often an .mlx component rendered to a string). *)
type layout = head_html:string -> body_html:string -> string

type t = { routes : (string * page) list; layout : layout; not_found : page option }

let make ?layout ?not_found () : t =
  let default_layout ~head_html ~body_html =
    Printf.sprintf
      "<!DOCTYPE html><html lang=\"en\"><head><meta charset=\"utf-8\"/><meta \
       name=\"viewport\" content=\"width=device-width, initial-scale=1\"/>%s</head><body><div \
       id=\"root\">%s</div></body></html>"
      head_html body_html
  in
  { routes = []; layout = Option.value layout ~default:default_layout; not_found }

let page (pattern : string) (p : page) (t : t) : t = { t with routes = t.routes @ [ (pattern, p) ] }

(* render a path to a full HTML document, or None if no route matches (and no
   not_found is set) — letting the caller 404 *)
let render (t : t) (path : string) : string option =
  let chosen =
    match Fennec_matcher.Matcher.find t.routes path with
    | Some (p, params) -> Some (p, params)
    | None -> ( match t.not_found with Some p -> Some (p, [ ("*", path) ]) | None -> None)
  in
  match chosen with
  | None -> None
  | Some (p, params) ->
    let body_html, tags = HK.render_collect (p params) in
    Some (t.layout ~head_html:(Head.to_html tags) ~body_html)

(* expose the route table (the client router consumes the same data) *)
let routes (t : t) = t.routes
