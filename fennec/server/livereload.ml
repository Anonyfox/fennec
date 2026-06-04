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

(* the server's boot id: a per-process nonce, stable for this process's life and DIFFERENT after
   a restart, so the client can tell "reconnected to the same server" (a blip — don't reload)
   from "the server was replaced" (a real backend rebuild — reload once). pid ALONE is not enough
   — the OS recycles pids, so a restart could land on the same number and the client would miss
   the reload; pairing it with the start time makes a genuine collision impossible. *)
let boot_id = Printf.sprintf "%d-%d" (Unix.getpid ()) (int_of_float (Unix.gettimeofday () *. 1000.))

(* register a browser's livereload socket; returns an unregister thunk. We greet the client
   with our boot id so it only reloads on a genuine restart, not on every reconnect. *)
let register t (send : string -> unit) : unit -> unit =
  t.next <- t.next + 1;
  let id = t.next in
  Hashtbl.replace t.clients id send;
  (try send ("boot:" ^ boot_id) with _ -> ());
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
  (* tolerate casing/whitespace in the value: "Text/HTML", " text/html; charset=utf-8", … all
     count — otherwise such a page would silently get no script AND no no-cache override *)
  | Some ct ->
    let ct = String.lowercase_ascii (String.trim ct) in
    String.length ct >= 9 && String.sub ct 0 9 = "text/html"
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
          (* inject the client script AND force the dev page to revalidate (no-cache): a reload
             after a restart must fetch the fresh SSR, not a heuristically-cached old page. The
             strong ETag downstream keeps this cheap (304 when unchanged). *)
          let headers =
            ("cache-control", "no-cache")
            :: List.filter
                 (fun (k, _) -> String.lowercase_ascii k <> "cache-control")
                 r.Fennec_core.Http.headers
          in
          { r with Fennec_core.Http.body = Fennec_core.Dev.inject_html r.Fennec_core.Http.body; headers }
        else r)
