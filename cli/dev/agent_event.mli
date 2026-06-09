(** Machine-readable dev-loop events for agent hooks.

    This is deliberately separate from {!Ui}: humans keep the terminal UI, while
    agents wait on a small append-only JSONL journal. *)

type t

val default_dir : root:string -> string
(** Default state directory for [root], using [FENNEC_AGENT_DIR] when set, then
    [XDG_STATE_HOME], then [$HOME/.local/state]. *)

val start : ?dir:string -> ?port:int -> root:string -> unit -> t
(** Create the state directory, truncate the event journal for this dev session,
    and write status metadata. *)

val dir : t -> string
val events_path : dir:string -> string
val status_path : dir:string -> string
val marker_dir : dir:string -> string

val emit :
  t ->
  kind:string ->
  ?summary:string ->
  ?trigger:string list ->
  ?ms:float option ->
  ?fields:(string * string) list ->
unit ->
unit
(** Append one event. [fields] values are already scalar strings. *)

val emit_verdict : t -> Verdict.t -> unit
(** Append the canonical agent event for a dev-loop verdict. *)

val latest_id : dir:string -> int option
(** Latest event id in the journal, if any. *)

val wait_next : ?after:int -> dir:string -> timeout:float -> unit -> (int * string, string) result
(** Block until an event with [id > after]. Without [after], snapshots the
    current latest id first. Returns the event id and a short summary. *)

val mark : dir:string -> input:string -> int
(** Snapshot the current latest id and, when possible, store it under the
    harness tool/session key found in [input]. *)

val hook_json : dir:string -> timeout:float -> event:string -> input:string -> string
(** Wait and render hook JSON carrying [hookSpecificOutput.additionalContext]. *)

val status : dir:string -> string

val json_escape : string -> string
val find_string_field : string -> string -> string option
val unescape_json_string : string -> string
val find_int_field : string -> string -> int option
