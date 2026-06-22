(** Generated project guidance for humans and coding agents. *)

val render : unit -> string
(** The canonical Markdown guide — a SKILL.md-style reader. What [fennec skill] prints and what an
    agent consumes; raw, stable, machine-friendly. *)

val render_human : unit -> string
(** The same guide for a person at a terminal: brand-coloured headers, tinted commands, dimmed
    comments, and no raw Markdown noise — but only when stdout is a colour TTY. Falls back to {!render}
    (raw Markdown) when colour is off (piped, [NO_COLOR], not a TTY). What bare [fennec] prints. *)
