(** Machine-readable dev-loop events for agent hooks.

    This is deliberately separate from {!Ui}: humans keep the terminal UI, while
    agents wait on a small append-only JSONL journal. *)

type t

val default_dir : root:string -> string
(** Default state directory for [root], using [FENNEC_AGENT_DIR] when set, then
    [XDG_STATE_HOME], then [$HOME/.local/state]. *)

val start : ?dir:string -> root:string -> unit -> t
(** Create the state directory, truncate the event journal for this dev session,
    and write status metadata. *)

val dir : t -> string
val events_path : dir:string -> string
val status_path : dir:string -> string

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

val wait_next : dir:string -> timeout:float -> (string, string) result
(** Block until the next event after the current journal end and return a short
    human-readable summary. *)

val hook_json : dir:string -> timeout:float -> event:string -> string
(** Wait and render hook JSON carrying [hookSpecificOutput.additionalContext]. *)

val status : dir:string -> string

val json_escape : string -> string
val find_string_field : string -> string -> string option
val unescape_json_string : string -> string
