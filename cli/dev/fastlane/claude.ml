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
      (fun () ->
        let path = settings_path () and matcher = "Edit|Write|MultiEdit" in
        let pre = Harness.install_json_file ~harness_id:"claude" ~path ~event:"PreToolUse" ~matcher ~command:(Harness.mark_command ()) in
        let post = Harness.install_json_file ~harness_id:"claude" ~path ~event:"PostToolUse" ~matcher ~command:(Harness.agent_command ~harness_id:"claude") in
        { post with changed = pre.changed || post.changed });
    uninstall = (fun () -> Harness.uninstall_json_file ~harness_id:"claude" ~path:(settings_path ()));
    render_feedback = Harness.render_camel
  }
