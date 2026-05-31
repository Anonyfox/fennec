(* Fennec — the userland facade over the Paw/Endpoint/Server core. It owns the
   operational plumbing that's identical in every app: the Eio entry point, the
   dev/prod web-root flip (disk vs embedded), dev livereload (the websocket paw +
   bundle watchers), and starting the server. Userland writes endpoints as paw
   pipelines and hands them to [serve].

   Re-exports the pieces an app needs (Endpoint, Plug, Conn, Router, Head) so a
   userland file opens one module. *)

module Conn = Fennec_paw.Conn
module Paw = Fennec_paw.Paw
module Endpoint = Fennec_server.Endpoint
module Plug = Fennec_server.Plug
module Static = Fennec_server.Static
module Livereload = Fennec_server.Livereload
module Http = Fennec_core.Http

let is_dev = try Sys.getenv "FENNEC_ENV" <> "production" with Not_found -> true

(* A web root for an app: dev reads the assembled webroot/ dir next to the exe
   (the per-app dune assembly), prod serves the embedded map. [name] disambiguates
   per-app dev webroots ("webroot_web", "webroot_admin"). *)
let web_source ~name ~assets : Static.source =
  if is_dev then Static.Dir (Filename.concat (Filename.dirname Sys.executable_name) name)
  else Static.Embedded assets

(* the static-serving paw for an app's web root *)
let static ~name ~assets : Paw.t = Plug.static (web_source ~name ~assets)

(* watch built bundles next to the exe (dev): *.css hot-swap, *.js reload *)
let dev_watch ~sw ~clock lr ~dir =
  let entries = try Sys.readdir dir with _ -> [||] in
  Array.iter
    (fun f ->
      let path = Filename.concat dir f in
      match Filename.extension f with
      | ".css" -> Eio.Fiber.fork ~sw (fun () -> Livereload.watch lr ~clock ~kind:Livereload.Css path)
      | ".js" | ".mjs" ->
        Eio.Fiber.fork ~sw (fun () -> Livereload.watch lr ~clock ~kind:Livereload.Reload path)
      | _ -> ())
    entries

(* Serve a list of endpoints, blocking. In dev, a livereload paw is prepended to
   every endpoint and the bundle dirs are watched. Owns Eio + the lifecycle. *)
let serve ?(timeout = 30.0) ?(max_conns = 10_000) ?(webroots = []) (endpoints : Endpoint.t list) :
    unit =
  Eio_main.run @@ fun env ->
  let lr = Livereload.create () in
  let endpoints =
    if is_dev then List.map (fun e -> Endpoint.prepend (Livereload.paw lr) e) endpoints
    else endpoints
  in
  Eio.Switch.run @@ fun sw ->
  if is_dev then
    List.iter
      (fun name ->
        let dir = Filename.concat (Filename.dirname Sys.executable_name) name in
        dev_watch ~sw ~clock:(Eio.Stdenv.clock env) lr ~dir)
      webroots;
  Printf.eprintf "[fennec] serving %d endpoint(s)%s\n%!" (List.length endpoints)
    (if is_dev then " (dev: livereload on)" else "");
  Fennec_server.Server.run ~timeout ~max_conns ~dev:is_dev ~env endpoints
