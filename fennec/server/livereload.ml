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
