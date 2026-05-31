(* Livereload server glue (dev only). Holds the set of connected livereload
   browsers and a background fiber that watches build *outputs* (not source) and
   pushes a frame when they change:

     - a watched asset file (CSS/JS bundle) changes -> push "css" (hot-swap) or
       "reload" depending on the asset kind.

   Backend reloads need NO push from here: when the CLI restarts the server the
   socket simply drops, and the client script reloads on reconnect. So this
   module only exists to make frontend-only edits reload without a restart.

   Wire it by pointing the [Fennec_core.Dev.endpoint] websocket at [register],
   and forking [watch] for each output you want to live-reload on. *)

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
       browser's channel (so asset watchers can push "reload"/"css" frames) and
       unregisters on close;
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

type kind = Css | Reload

(* poll [path]'s mtime every [interval] seconds; on change, [on_change ()] then
   broadcast the frame for [kind]. Run inside an Eio switch via Fiber.fork. *)
let watch t ~clock ?(interval = 0.3) ~kind ?(on_change = fun () -> ()) (path : string) : unit =
  let mtime () = try (Unix.stat path).Unix.st_mtime with _ -> 0.0 in
  let frame = match kind with Css -> "css" | Reload -> "reload" in
  let rec loop last =
    Eio.Time.sleep clock interval;
    let m = mtime () in
    if m > last && m > 0.0 then begin
      (try on_change () with _ -> ());
      broadcast t frame;
      loop m
    end
    else loop last
  in
  loop (mtime ())
