(* Fennec — the userland facade. A fennec app is a short pipe over intent; this
   module owns ALL the operational plumbing that is identical in every app: the
   Eio entry point, the dev/prod mode flip, static serving of the web root,
   livereload (the websocket endpoint + watching the built bundles), and the
   server lifecycle. Userland never writes Eio, switches, fibers, or watchers.

     let render _req =
       ReactDOM.renderToString (App.make ()) |> Fennec.Page.html ~title:"Hi"

     let () =
       Fennec.app ~assets:Webroot_assets.lookup ()
       |> Fennec.get "/" render
       |> Fennec.serve ~port:8200

   The one thing that can't be hidden is [~assets] — the prod embed module is
   generated into the app's own binary, so the app hands it to the framework. *)

module Http = Fennec_core.Http

(* re-export the request/response types and the verb plugs so userland needs only
   `open Fennec` (or `Fennec.get …`) — never the internal module paths *)
type request = Http.request
type response = Http.response

(* dev unless FENNEC_ENV=production. The sole runtime mode switch. *)
let is_dev = try Sys.getenv "FENNEC_ENV" <> "production" with Not_found -> true

(* ---- the app: a plug pipeline plus the framework's own conventions ---- *)

type t = {
  mutable pipeline : Fennec_core.App.t;
  assets : (string -> string option) option; (* prod embed lookup, if any *)
}

(* Create an app. [assets] is the prod embed module's [lookup] (omit for a pure
   dev/dynamic app). Static serving of the web root + livereload are wired in by
   [serve]; userland only adds its own routes. *)
let app ?assets () : t = { pipeline = Fennec_core.App.empty; assets }

(* pipe a plug onto the app *)
let use (p : Fennec_core.App.plug) (a : t) : t =
  a.pipeline <- Fennec_core.App.use p a.pipeline;
  a

(* the userland verbs — thin wrappers so apps say [Fennec.get] not [App.get] *)
let get path h a = use (Fennec_core.App.get path h) a
let post path h a = use (Fennec_core.App.post path h) a
let put path h a = use (Fennec_core.App.put path h) a
let delete path h a = use (Fennec_core.App.delete path h) a
let filter f a = use (Fennec_core.App.filter f) a
let pages routes a = use (Fennec_core.App.pages routes) a
let page = Fennec_core.App.page

(* response helpers, re-exported *)
let text = Http.text
let html = Http.html
let json = Http.json

module Head = Fennec_head.Head

(* A document template: given the merged <head> HTML, the SSR'd body HTML, and the
   framework bits (scripts, props, dev flag), produce the full document string.
   The DEFAULT is provided below; an app supplies its own to own the shell (an MLX
   component rendered to a string is a perfectly good [template]). *)
type template =
  head_html:string ->
  body_html:string ->
  css_href:string ->
  scripts:string list ->
  props_json:string ->
  dev:bool ->
  string

(* the framework default template — a sensible HTML5 shell. Apps override via
   [~template] to fully control doctype/lang/includes. *)
let default_template : template =
 fun ~head_html ~body_html ~css_href ~scripts ~props_json ~dev ->
  let link =
    if css_href = "" then "" else Printf.sprintf {|<link rel="stylesheet" href="%s"/>|} css_href
  in
  let script_tags =
    String.concat "" (List.map (fun s -> Printf.sprintf {|<script src="%s" defer></script>|} s) scripts)
  in
  let doc =
    Printf.sprintf
      {|<!DOCTYPE html>
<html lang="en">
<head>
<meta charset="utf-8"/>
<meta name="viewport" content="width=device-width, initial-scale=1"/>
%s
%s
</head>
<body>
<div id="root">%s</div>
<script id="fennec-props" type="application/json">%s</script>
%s
</body>
</html>|}
      head_html link body_html (Head.text_escape props_json) script_tags
  in
  if dev then Fennec_core.Dev.inject_html doc else doc

(* Render a page to a full document response. [body] is a thunk that builds the
   page React element (mounting <Head> components anywhere in its tree); SSR
   collects those tags and the template renders them into <head>. [title]/
   [description]/[canonical] are handler-level DEFAULTS — a <Head> deeper in the
   tree overrides them (inside-out wins). *)
let render ?(template = default_template) ?title ?description ?canonical ?(css_href = "/app.css")
    ?(scripts = [ "/react.js"; "/app.js" ]) ?(props_json = "null") ~body () : response =
  let body_html, tree_tags = Fennec_ssr.Head.render_collect body in
  (* handler defaults first (lowest precedence), then tree tags (win) *)
  let base = Head.of_props ?title ?description ?canonical () in
  let head_html = Head.to_html (Head.merge (base @ tree_tags)) in
  Http.html (template ~head_html ~body_html ~css_href ~scripts ~props_json ~dev:is_dev)

(* ---- the web root (bundles + public), dev-from-disk / prod-embedded ---- *)

let webroot_dir () = Filename.concat (Filename.dirname Sys.executable_name) "webroot"

let static_source (a : t) : Fennec_server.Static.source =
  match a.assets with
  | Some lookup when not is_dev -> Fennec_server.Static.Embedded lookup
  | _ -> Fennec_server.Static.Dir (webroot_dir ())

(* ---- serve: owns Eio, the dev livereload, and the lifecycle ---- *)

(* watch every built bundle next to the exe: *.css hot-swaps, everything else
   reloads. Globbing the exe dir means userland never names its bundles. *)
let dev_watch ~sw ~clock lr =
  let dir = Filename.dirname Sys.executable_name in
  let entries = try Sys.readdir dir with _ -> [||] in
  Array.iter
    (fun name ->
      let path = Filename.concat dir name in
      match Filename.extension name with
      | ".css" ->
        Eio.Fiber.fork ~sw (fun () ->
            Fennec_server.Livereload.watch lr ~clock ~kind:Fennec_server.Livereload.Css path)
      | ".js" | ".mjs" ->
        Eio.Fiber.fork ~sw (fun () ->
            Fennec_server.Livereload.watch lr ~clock ~kind:Fennec_server.Livereload.Reload path)
      | _ -> ())
    entries

(* Run the app on [port], blocking. Static serving of the web root is appended to
   the pipeline (after the user's routes), so explicit routes win and unmatched
   paths fall to files then 404. In dev, the livereload websocket + bundle
   watchers are wired automatically. *)
let serve ?(port = 8200) ?timeout ?max_conns (a : t) : unit =
  (* static is the tail of the pipeline (user routes first, then files) *)
  let a = use (Fennec_core.App.fallthrough (Fennec_server.Static.handler (static_source a))) a in
  Eio_main.run @@ fun env ->
  let lr = Fennec_server.Livereload.create () in
  let on_ws (req : request) (ws : Fennec_server.Server.ws) =
    if is_dev && req.Http.path = Fennec_core.Dev.endpoint then
      ws.Fennec_server.Server.on_close <- Fennec_server.Livereload.register lr ws.Fennec_server.Server.send
  in
  Eio.Switch.run @@ fun sw ->
  if is_dev then dev_watch ~sw ~clock:(Eio.Stdenv.clock env) lr;
  Printf.eprintf "[fennec] serving http://localhost:%d%s\n%!" port
    (if is_dev then " (dev: livereload on)" else "");
  Fennec_server.Server.run ?timeout ?max_conns ~env ~on_ws ~port a.pipeline
