(** User-level hook attachment for coding agents.

    The dev loop itself is still {!Supervisor}: humans run [fennec dev] and get
    the normal terminal UI. This module adds the smallest bridge needed for
    agents: install a guarded PostToolUse hook in the active harness so native
    edit tools can receive {!Agent_event} verdicts without a follow-up
    build/test/poll command.

    The installed command is always project-root guarded. It reads the harness
    hook payload, checks that the tool call cwd is inside the project root, and
    only then execs [fennec agent hook --dir <agent-dir> --timeout 12]. That
    makes user-level hook files safe across repositories. *)

(** Supported harness families. *)
type harness = Claude | Codex

(** Outcome of attempting to install one harness hook. [changed] means a
    user-level config file was written; [message] is intentionally short because
    it is printed before the devserver UI starts. *)
type result = { harness : harness; path : string; changed : bool; message : string }

(** Install hooks for [harnesses], or auto-detect the active harness from the
    current environment. Unknown/plain terminals install every known harness so
    "start devserver first, then chat with an agent" still just works. No repo
    files are created.

    Codex additionally needs a trusted hook fingerprint. The installer writes
    the matching entry in [~/.codex/config.toml] next to [~/.codex/hooks.json],
    so a loaded hook can run without manual trust prompts. *)
val install : ?harnesses:harness list -> root:string -> agent_dir:string -> unit -> result list

(** Human/agent-readable summary of {!install}. The text deliberately does not
    include the exact hook feedback marker
    ["Fennec dev feedback after this tool"], so an agent cannot confuse setup
    status with a real post-edit verdict. *)
val report : result list -> string
