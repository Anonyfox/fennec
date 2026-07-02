(* Livereload server glue (dev only) — a pure RELAY: it holds the connected livereload browsers and
   broadcasts frames on demand ("css" = stylesheet hot-swap, anything else = full reload). It watches
   nothing; the CLI owns filesystem watching and pings the dev control socket, which calls
   {!broadcast}. See the .ml header for the boot-id reconnect protocol. *)

type t

val create : unit -> t

val register : t -> (string -> unit) -> unit -> unit
(** Register a browser's livereload socket ([send]); greets it with this process's boot id (so it
    reloads only on a genuine restart, not on a reconnect blip). Returns the unregister thunk. *)

val broadcast : t -> string -> unit
(** Send a frame to every connected browser (["css"] hot-swaps stylesheets; anything else reloads). *)

val count : t -> int
(** Connected browsers (for the dev banner / diagnostics). *)

val paw : t -> Pipeline.t
(** The dev livereload paw, mounted FIRST in every endpoint: answers the livereload websocket
    upgrade on the dev endpoint (registering the channel), and on every other request installs a
    before_send hook that injects the client script into HTML responses (in memory) and marks the
    page [no-cache] (the strong ETag downstream keeps revalidation a 304). *)
