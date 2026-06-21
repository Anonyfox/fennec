(** Machine-readable dev-loop events for agent hooks.

    This is deliberately separate from {!Ui}: humans keep the terminal UI, while
    agents wait on a small append-only JSONL journal. *)

type t
(** Handle for the append-only agent event journal of one [fennec dev] session.

    A value owns paths and sequencing for the current workspace root; callers use
    it to emit build/test/verdict events without knowing the on-disk layout. *)

val default_dir : root:string -> string
(** Default state directory for [root], using [FENNEC_AGENT_DIR] when set, then
    [XDG_STATE_HOME], then [$HOME/.local/state]. *)

val start : ?dir:string -> ?port:int -> root:string -> unit -> t
(** Create the state directory, truncate the event journal for this dev session,
    and write status metadata. *)

val dir : t -> string
(** State directory used by this journal. Useful for printing attach hints and
    for handing the same directory to hook/wait helpers. *)

val events_path : dir:string -> string
(** Path to the JSONL event journal inside [dir]. Agents wait on this file; it is
    truncated on each fresh dev-session start so ids are session-local. *)

val errors_path : dir:string -> string
(** Path to the SEPARATE runtime-HTTP-error JSONL stream inside [dir]. Deliberately distinct from
    {!events_path}: a flood of async request failures must never disturb the edit→verdict id sequence
    that mark/wait/hook depend on. Also truncated on each fresh dev-session start. *)

val status_path : dir:string -> string
(** Path to the small status JSON file inside [dir]. This gives humans and
    agents a cheap way to see whether a dev server is attached and where events
    are being written. *)

val marker_dir : dir:string -> string
(** Directory for per-harness markers. A marker records the latest event id seen
    by a tool invocation so hooks can block for strictly newer feedback. *)

val emit :
  t ->
  kind:string ->
  ?summary:string ->
  ?trigger:string list ->
  ?ms:float option ->
  ?fields:(string * string) list ->
unit ->
unit
(** Append one event. [fields] values are already scalar strings. The dev supervisor encodes its
    [Verdict.t] into these scalar fields (the lib stays free of the verdict type). *)

val emit_error : t -> Paw.Access.t -> unit
(** Append one failed request ({!Paw.Access.is_error}) to the runtime-error stream ({!errors_path}).
    Independent id space, so the edit→verdict journal is untouched. *)

val latest_id : dir:string -> int option
(** Latest event id in the journal, if any. *)

val wait_next : ?after:int -> dir:string -> timeout:float -> unit -> (int * string, string) result
(** Block until an event with [id > after]. Without [after], snapshots the
    current latest id first. Returns the event id and a short summary. *)

val mark : dir:string -> input:string -> int
(** Snapshot the current latest id and, when possible, store it under the
    harness tool/session key found in [input]. *)

val feedback_text : dir:string -> timeout:float -> input:string -> string
(** The harness-AGNOSTIC post-tool feedback for one edit: block (up to [timeout]) for the next settled
    verdict past the relevant lower bound (an explicit override in [input] / the pre-tool mark / the
    read cursor), advance the cursor on delivery, then append any new runtime-HTTP-error rows. A
    harness adapter wraps the returned text in its own injection JSON — see {!Harness.render_feedback}. *)

val status : dir:string -> string
(** Render the current attach status JSON for [dir]. Missing files produce a
    conservative detached/dead status instead of raising. *)

val errors_report : dir:string -> ?after:int -> unit -> string
(** A terse human listing of recorded runtime HTTP errors for [dir] (one row per failure: status,
    method, path, timing, message). Stateless — pass [~after] to page past ids already seen. Powers
    [fennec agent errors]. *)

val json_escape : string -> string
(** Escape one string for the tiny JSON writer used by agent events. *)

val find_string_field : string -> string -> string option
(** Find a string field in a small flat JSON object. This intentionally supports
    only the hook payload shape used by agent harnesses. *)

val unescape_json_string : string -> string
(** Decode the escape sequences emitted by {!json_escape}. *)

val find_int_field : string -> string -> int option
(** Find an integer field in a small flat JSON object. Used for event ids and
    persisted hook markers. *)
