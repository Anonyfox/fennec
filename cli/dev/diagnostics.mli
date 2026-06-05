(** Structured view of a failed build's diagnostic text.

    The supervisor already captures dune's full diagnostic output per cycle (the [messages] field
    of a {!Dune_watch.event}); this turns the OCaml/dune format into placed problems so the UI can
    render a clean, persistent code-frame. It is a best-effort scanner: a format it doesn't
    recognise yields [[]], and the caller falls back to showing the raw text — so information is
    never dropped, only upgraded when recognised. *)

type severity = Error | Warning

type problem = {
  severity : severity;
  file : string;  (** path as dune reported it (workspace-relative) *)
  line : int;
  col : int;  (** 1-based start column; 0 if unknown *)
  message : string;  (** the Error:/Warning: text, flattened to one line *)
  excerpt : string list;  (** the source-frame lines dune printed, verbatim (often empty) *)
  related : string list;  (** secondary locations/hints for this SAME problem (folded in, not counted) *)
}

(** Parse captured diagnostic text into problems; [[]] if nothing recognisable was found. A
    secondary location with no Error/Warning of its own (e.g. a syntax error's "might be
    unmatched" hint) is folded into the preceding problem's [related], so the count stays honest. *)
val parse : string -> problem list

(** [(errors, warnings)] counts — one per primary problem. *)
val count : problem list -> int * int
