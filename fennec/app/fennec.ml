(* Fennec — the userland facade over the Paw/Endpoint/Server core. It owns the
   operational plumbing that's identical in every app: the Eio entry point, the
   dev/prod web-root flip (disk vs embedded), dev livereload (the websocket paw +
   a dev control socket the CLI pings), and starting the server. Userland writes
   endpoints as paw pipelines and hands them to [serve].

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

(* The dev control socket. The CLI owns ALL filesystem watching (it's the one
   process that links the native fs-event watcher); the framework watches nothing.
   When a served asset's content changes, the CLI connects to this unix-domain
   socket — its path is handed in via [FENNEC_LIVERELOAD] — and sends one line:
   "css" (stylesheet hot-swap) or anything else (full reload). We relay that to
   every connected browser. Loopback by nature (a unix socket), dev-only, and
   absent in prod. [reuse_addr] clears a stale socket from a prior run; Eio removes
   the path again when the switch finishes. *)
let dev_control ~sw ~net (lr : Livereload.t) : unit =
  match Sys.getenv_opt "FENNEC_LIVERELOAD" with
  | None | Some "" -> ()
  | Some path -> (
    match try Some (Eio.Net.listen ~sw ~backlog:8 ~reuse_addr:true net (`Unix path)) with _ -> None with
    | None -> () (* couldn't bind (e.g. path too long): CSS hot-swap is lost, but
                    backend reload via the reconnect loop still works *)
    | Some socket ->
      Eio.Fiber.fork ~sw (fun () ->
          Eio.Net.run_server socket
            (fun flow _addr ->
              let r = Eio.Buf_read.of_flow flow ~max_size:64 in
              match try Some (String.trim (Eio.Buf_read.line r)) with _ -> None with
              | Some "css" -> Livereload.broadcast lr "css"
              | Some _ -> Livereload.broadcast lr "reload"
              | None -> ())
            ~on_error:(fun _ -> ())))

(* Serve a list of endpoints, blocking. In dev, a livereload paw is prepended to
   every endpoint and a dev control socket is opened for the CLI to ping on a
   frontend edit (the framework itself watches nothing). Owns Eio + the lifecycle. *)
let serve ?(timeout = 30.0) ?(max_conns = 10_000) (endpoints : Endpoint.t list) : unit =
  Eio_main.run @@ fun env ->
  let lr = Livereload.create () in
  (* Livereload is a dev convenience; it reloads the page on a frontend edit. For an e2e or
     any controlled run it is pure nondeterminism (spontaneous navigations), so it can be
     turned off while still serving the dev (on-disk) web root: set FENNEC_DEV_LIVERELOAD=0. *)
  let livereload_on =
    is_dev && (match Sys.getenv_opt "FENNEC_DEV_LIVERELOAD" with Some ("0" | "off" | "false" | "no") -> false | _ -> true)
  in
  let endpoints =
    if livereload_on then List.map (fun e -> Endpoint.prepend (Livereload.paw lr) e) endpoints
    else endpoints
  in
  Eio.Switch.run @@ fun sw ->
  if livereload_on then dev_control ~sw ~net:(Eio.Stdenv.net env) lr;
  Printf.eprintf "[fennec] serving %d endpoint(s)%s\n%!" (List.length endpoints)
    (if livereload_on then " (dev: livereload on)" else "");
  Fennec_server.Server.run ~timeout ~max_conns ~dev:is_dev ~env endpoints
