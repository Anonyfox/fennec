(* Livereload server glue (dev only). A pure RELAY: it holds the set of connected
   livereload browsers and broadcasts a frame to them on demand. It watches
   nothing — all filesystem watching lives in the CLI (the one process that links
   the native fs-event watcher). On a served-asset change the CLI pings the dev
   control socket and the framework relays the frame here via [broadcast]:

     - "css"    -> stylesheet hot-swap (no reload)
     - anything -> full reload

   Backend reloads need no frame at all: when the CLI restarts the server the
   socket simply drops, and the client script reloads on reconnect. So this module
   exists only to (a) hold the browser sockets ([register]) and (b) deliver the
   CLI's frontend-edit signal to them ([broadcast]).

   Wire it by pointing the [Fennec_core.Dev.endpoint] websocket at [register]; the
   dev control listener (see Fennec.serve) calls [broadcast]. *)

type t = {
  clients : (int, string -> unit) Hashtbl.t;
  mutable next : int;
}

let create () = { clients = Hashtbl.create 16; next = 0 }

(* register a browser's livereload socket; returns an unregister thunk *)
let register t (send : string -> unit) : unit -> unit =
  t.next <- t.next + 1;
  let id = t.next in
  Hashtbl.replace t.clients id send;
  fun () -> Hashtbl.remove t.clients id

let broadcast t (msg : string) =
  Hashtbl.iter (fun _ send -> try send msg with _ -> ()) t.clients

let count t = Hashtbl.length t.clients

(* The dev livereload paw. Mounted FIRST in every endpoint's pipeline (see
   Fennec.serve), it does two jobs with one paw — both built on existing
   primitives, no special server hook:
     - the livereload socket itself: a ws upgrade on [Dev.endpoint] registers the
       browser's channel (so the dev control listener can push "reload"/"css"
       frames) and unregisters on close;
     - every other request: registers a before_send that injects the tiny client
       script into HTML responses in memory (never on disk), so the browser opens
       that socket. It passes the conn through (the app still answers).
   Because it runs before the answering paws, the injection hook is registered even
   though the app short-circuits the pipeline. *)
let is_html_response (r : Fennec_core.Http.response) : bool =
  match Fennec_core.Http_semantics.header r.Fennec_core.Http.headers "content-type" with
  | Some ct -> String.length ct >= 9 && String.sub ct 0 9 = "text/html"
  | None -> false

let paw (t : t) : Fennec_paw.Paw.t =
 fun c ->
  if Fennec_paw.Conn.path c = Fennec_core.Dev.endpoint then
    Fennec_paw.Conn.upgrade c (fun (ch : Fennec_core.Ws_channel.t) ->
        let unregister = register t ch.Fennec_core.Ws_channel.send in
        ch.Fennec_core.Ws_channel.on_close <- unregister)
  else
    Fennec_paw.Conn.before_send c (fun r ->
        if is_html_response r then
          { r with Fennec_core.Http.body = Fennec_core.Dev.inject_html r.Fennec_core.Http.body }
        else r)
