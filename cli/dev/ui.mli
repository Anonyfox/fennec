(** The dev-server terminal: a quiet event log + one live "current problems" region.

    Two zones. The {b event log} appends one calm line per real event (a rebuild, a reload) and
    flows into real scrollback. The {b problem region} is a single block pinned at the bottom that
    always shows the CURRENT outstanding compile problems — repainted in place, so an error from
    file A survives an edit to file B and only clears when A is actually fixed (not when a newer
    event scrolls it away). On a non-interactive sink (pipe/CI) the same information is printed as
    plain append lines with no cursor control.

    The supervisor is the sole writer to the terminal (it captures the server's output and relays
    it through {!app}), so the live region is never corrupted by interleaving. *)

type t

type level = Info | Warn | Error

(** Create a UI. [out] defaults to stdout (flushed); [caps] defaults to {!Tty.detect}. Both are
    injectable so the renderer is unit-testable against a buffer with fixed capabilities. *)
val create : ?out:(string -> unit) -> ?caps:Tty.t -> unit -> t

(** Print the startup header and record the watched [dir] (shown by {!ready}). *)
val start : t -> dir:string -> unit

(** First server up: print the named clickable dev URL(s) ([(name, url)] pairs) and "ready in <ms>".
    Idempotent — only the first call prints (later calls just refresh the stored URLs), so a server
    restart doesn't reprint the banner. *)
val ready : t -> urls:(string * string) list -> ms:float option -> unit

(** A backend rebuild that restarted the server (the page will full-reload). *)
val rebuilt : t -> trigger:string list -> ms:float option -> unit

(** A frontend change that triggers a full page reload. *)
val reloaded : t -> trigger:string list -> ms:float option -> unit

(** A CSS-only change, hot-swapped without a reload. *)
val restyled : t -> trigger:string list -> ms:float option -> unit

(** A failed build: parse [raw] (dune's diagnostic text) into the persistent problem region.
    [serving] indicates a last-good server is still up. Any successful build ({!rebuilt} /
    {!reloaded} / {!restyled} / {!ready} / {!resolved}) clears the region. *)
val failed : t -> raw:string -> trigger:string list -> serving:bool -> unit

(** A green build that produced no server/asset change — e.g. reverting a typo to a byte-identical
    artifact. If a problem region was showing it's now fixed: clear it and print a brief "resolved"
    line. A no-op when there was nothing outstanding (so genuine no-op builds stay silent). This is
    what keeps the panel from sticking after a fix that dune rebuilds to the same bytes. *)
val resolved : t -> ms:float option -> unit

(** A one-off notice (worker fallback, dune respawn, port-in-use, give-up). *)
val notice : t -> level -> string -> unit

(** Relay a line the dev server itself printed (the user's app logs), kept above the region. *)
val app : t -> string -> unit

(** Clean-shutdown sign-off with a terse session summary. *)
val stopped : t -> unit
