(** Terminal capabilities + low-level escape helpers for the dev UI.

    Detection runs once at startup; the rest is pure string-building, so the renderer (and its
    tests) are deterministic given a [t]. *)

type t = {
  color : bool;  (** emit ANSI SGR colour *)
  hyperlinks : bool;  (** emit OSC 8 hyperlinks (clickable URLs) *)
  interactive : bool;  (** stdout is a TTY — the live status region may repaint in place *)
  width : int;  (** terminal columns (for truncation); 80 when unknown *)
}

(** Probe the real terminal. Colour follows [NO_COLOR] > [FORCE_COLOR] > is-a-tty; hyperlinks
    require an allow-listed terminal (iTerm2/WezTerm/kitty/VS Code/Windows Terminal/VTE/Ghostty)
    and a TTY, and are off in CI. *)
val detect : unit -> t

(** A fully plain capability set (no colour, no links, non-interactive) — for pipes/CI/tests. *)
val plain : t

(** Wrap [s] in an SGR colour [code] (e.g. ["32"]); identity when colour is off. *)
val sgr : t -> string -> string -> string

(** Render [text] as a clickable [url] (OSC 8); falls back to plain [text] when unsupported, so
    the URL is always at least copy-pasteable. *)
val hyperlink : t -> url:string -> text:string -> string

(** [cursor_up n]: move the cursor up [n] lines (empty for [n<=0]). *)
val cursor_up : int -> string

(** Erase from the cursor to the end of the screen. *)
val erase_below : string

(** Carriage return. *)
val cr : string

(** Number of visible columns in [s], ignoring ANSI SGR / OSC 8 sequences and counting each UTF-8
    codepoint as width 1 (sufficient for our glyphs). *)
val visible_width : string -> int
