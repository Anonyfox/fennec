(** The harness ADAPTER and the shared "kit" adapters are built from.

    One coding harness (Claude Code, Codex, Mistral Vibe) is one {!t} value registered in
    {!Registry}; adding a harness is a new value, never a change to the core or to this kit. An
    adapter owns only the load-bearing deltas — where its hook config lives, the matcher/event for an
    edit, the extra steps it needs (a trust hash, an experimental flag), and the exact JSON shape it
    wants the feedback wrapped in. Everything mechanical lives here. *)

(** The outcome of installing/removing one harness's hook, for the human report. *)
type result = { harness : string; path : string; changed : bool; message : string }

(** A harness adapter.

    [install]/[uninstall] write or remove ONLY our marked hook, preserving the user's other settings;
    [render_feedback] wraps {!Journal.feedback_text} in this harness's injection JSON — the single
    per-harness output difference. *)
type t = {
  id : string;                                          (** stable key: ["claude"] | ["codex"] | ["vibe"] *)
  detect : unit -> bool;                                (** env sniff so [dev --attach] self-registers *)
  install : root:string -> agent_dir:string -> result;
  uninstall : root:string -> result;
  render_feedback : event:string -> text:string -> string;
}

(* ── the kit (used by adapters in this library) ── *)

val home : unit -> string
val getenv : string -> string option
val read_file : string -> string option
val write_file : string -> string -> unit

(** JSON string escaper, shared with {!Journal}; doubles as a TOML basic-string escaper. *)
val json_escape : string -> string

val toml_basic_escape : string -> string
val contains : string -> string -> bool
val find_sub : string -> string -> int option

(** A stable per-project marker baked as a trailing comment into our hook command, so install is
    idempotent, merge-safe, and uninstall precise. *)
val marker : root:string -> string

(** The guarded hook command: a short python that runs [fennec agent hook --harness <id>] only when
    the tool fired inside [root], then execs the SAME fennec binary that installed it. *)
val guarded_command : harness_id:string -> root:string -> agent_dir:string -> string

(** Idempotent install of our PostToolUse entry into a nested-JSON hooks file (Claude + Codex share
    this), preserving every other key and hook. *)
val install_json_file : harness_id:string -> path:string -> matcher:string -> root:string -> agent_dir:string -> result

(** Remove our marked PostToolUse entry from a JSON hooks file, leaving the rest untouched. *)
val uninstall_json_file : harness_id:string -> path:string -> root:string -> result

(** The camelCase injection shape Claude Code and Codex share: [hookSpecificOutput.additionalContext]. *)
val render_camel : event:string -> text:string -> string
