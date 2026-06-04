(* Fennec — the userland facade over the Paw/Endpoint/Server core. It owns the
   operational plumbing that's identical in every app: the Eio entry point, the
   dev/prod web-root flip (disk vs embedded), dev livereload (the websocket paw +
   a dev control socket the CLI pings), and starting the server. Userland writes
   endpoints as paw pipelines and hands them to [serve].

   Re-exports the pieces an app needs (Endpoint, Conn, Http, …) so a userland file
   opens one module. The prebuilt batteries live under {!Paw} as submodules
   ([Paw.Logger], [Paw.Session], [Paw.Csrf], …), each a [make] returning a paw. *)

module Conn = Fennec_paw.Conn
module Endpoint = Fennec_server.Endpoint
module Livereload = Fennec_server.Livereload
module Http = Fennec_core.Http
module Cookie = Fennec_core.Cookie

(* The verb namespace: the primitive + its algebra + the route verbs (from
   [Fennec_paw.Paw]) plus every prebuilt battery as a submodule. So userland reaches
   for [Paw.seq], [Paw.get], and [Paw.Logger.make ()] / [Paw.Session.make ~secret ()]
   from one place — each battery [make] returns a plain [Paw.t]. *)
module Paw = struct
  include Fennec_paw.Paw
  module Logger = Fennec_server.Logger
  module Security_headers = Fennec_server.Security_headers
  module Request_id = Fennec_server.Request_id
  module Method_override = Fennec_server.Method_override
  module Basic_auth = Fennec_server.Basic_auth
  module Force_https = Fennec_server.Force_https
  module Metrics = Fennec_server.Metrics
  module Websocket = Fennec_server.Websocket
  module Static = Fennec_server.Static
  module Session = Fennec_server.Session
  module Csrf = Fennec_server.Csrf
end

let is_dev = try Sys.getenv "FENNEC_ENV" <> "production" with Not_found -> true

(* Structured-concurrency helpers for handlers. A handler runs inside an Eio fiber, so it
   can fan out concurrent work (parallel DB queries / HTTP calls): the sub-fibers overlap
   their waits, and if the request's deadline fires or the client goes away, the whole tree
   is cancelled together. No threads, no manual cancellation tokens. *)

(* run thunks concurrently, returning their results in order *)
let parallel (thunks : (unit -> 'a) list) : 'a list =
  let out = Array.make (List.length thunks) None in
  Eio.Fiber.all (List.mapi (fun i t () -> out.(i) <- Some (t ())) thunks);
  Array.to_list out |> List.map Option.get

(* run two thunks (of different types) concurrently *)
let both (f : unit -> 'a) (g : unit -> 'b) : 'a * 'b =
  let a = ref None and b = ref None in
  Eio.Fiber.both (fun () -> a := Some (f ())) (fun () -> b := Some (g ()));
  (Option.get !a, Option.get !b)

(* A web root for an app: dev reads the assembled webroot/ dir next to the exe
   (the per-app dune assembly), prod serves the embedded map. [name] disambiguates
   per-app dev webroots ("webroot_web", "webroot_admin"). *)
let web_source ~name ~assets : Fennec_server.Static.source =
  if is_dev then Fennec_server.Static.Dir (Filename.concat (Filename.dirname Sys.executable_name) name)
  else Fennec_server.Static.Embedded (name, assets)

(* the static-serving paw for an app's web root *)
let static ~name ~assets : Paw.t = Fennec_server.Static.make (web_source ~name ~assets)

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
(* exactly one place starts the server. Many modules may LINK fennec, but a second [serve]
   call (a stray entrypoint in a library, a copy-pasted main) is a bug — fail loudly rather
   than half-start a second server. This is the runtime counterpart to the CLI's discovery,
   which finds the single [serve] site. *)
let started = Atomic.make false

let serve ?(timeout = 30.0) ?(max_conns = 10_000) (endpoints : Endpoint.t list) : unit =
  if not (Atomic.compare_and_set started false true) then
    failwith "Fennec.serve: a server is already running in this process — start the server in exactly one place";
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
