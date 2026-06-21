(* Claude Code adapter. A PostToolUse hook matching the edit tools, written into the user-global
   ~/.claude/settings.json (merge-safe). The reference harness: its camelCase
   hookSpecificOutput.additionalContext is the injection shape Codex copied, so both reuse it. *)

let settings_path () = Filename.concat (Filename.concat (Harness.home ()) ".claude") "settings.json"

let adapter : Harness.t =
  { id = "claude";
    detect =
      (fun () ->
        Harness.getenv "CLAUDECODE" <> None
        || Harness.getenv "CLAUDE_CODE" <> None
        || Harness.getenv "CLAUDE_CODE_ENTRYPOINT" <> None
        || Harness.getenv "CLAUDE_SESSION_ID" <> None);
    install =
      (fun ~root ~agent_dir ->
        Harness.install_json_file ~harness_id:"claude" ~path:(settings_path ()) ~matcher:"Edit|Write|MultiEdit" ~root ~agent_dir);
    uninstall = (fun ~root -> Harness.uninstall_json_file ~harness_id:"claude" ~path:(settings_path ()) ~root);
    render_feedback = Harness.render_camel
  }
