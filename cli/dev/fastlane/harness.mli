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
  install : unit -> result;                             (** install ONE universal, project-agnostic entry *)
  uninstall : unit -> result;
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

(** The one constant marker baked as a trailing comment into our hook command, so install is
    idempotent, merge-safe, and uninstall precise. Constant because the entry is now universal (one
    per harness, project-agnostic). *)
val marker : string

(** The POST-tool hook command: re-exec the installing fennec binary as [fennec agent hook --harness
    <id>]. Project-agnostic — it derives the journal from the editing cwd and stays silent unless a dev
    server is live there (see {!Journal.feedback_text}). No python guard, no baked root/dir. *)
val agent_command : harness_id:string -> string

(** The PRE-tool mark command: [fennec agent mark --quiet], run just before an edit so the post-tool
    hook reports exactly that edit's verdict (skipping leftover/background settles). Silent + project-
    agnostic. *)
val mark_command : unit -> string

(** Idempotently install ONE (event, command) entry into a nested-JSON hooks file (Claude + Codex share
    this), preserving every other key and hook. Adapters call it once per event — [PreToolUse] mark,
    [PostToolUse] hook — on the same file. *)
val install_json_file : harness_id:string -> path:string -> event:string -> matcher:string -> command:string -> result

(** Remove our marked entry from EVERY event array of a JSON hooks file, leaving the rest untouched. *)
val uninstall_json_file : harness_id:string -> path:string -> result

(** The camelCase injection shape Claude Code and Codex share: [hookSpecificOutput.additionalContext]. *)
val render_camel : event:string -> text:string -> string
