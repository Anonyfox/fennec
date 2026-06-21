(* Google Gemini CLI adapter. Gemini's hooks mirror Claude Code's settings.json shape almost exactly —
   a `hooks.<event>[]` array of `{matcher, hooks:[{type,command,timeout}]}` — so the install + the
   camelCase output ({!Harness.render_camel}) are reused. Two Gemini-specific facts: the events are
   `BeforeTool` / `AfterTool` (not Pre/PostToolUse), and `timeout` is MILLISECONDS (so we pass 15000,
   not 15). The injected `hookSpecificOutput.additionalContext` is appended to the tool result as
   `<hook_context>…</hook_context>` — verified in google-gemini/gemini-cli coreToolHookTriggers.ts.
   Stable since v0.26.0 (hooks enabled by default); user-global settings.json needs no trust step. *)

let settings_path () = Filename.concat (Filename.concat (Harness.home ()) ".gemini") "settings.json"

(* Gemini's built-in file-edit tools are `write_file` (whole file) and `replace` (in-place edit). *)
let matcher = "write_file|replace"
let timeout_ms = 15000

let adapter : Harness.t =
  { id = "gemini";
    detect = (fun () -> Harness.getenv "GEMINI_CLI" <> None || Harness.getenv "GEMINI_CLI_VERSION" <> None);
    installed = (fun () -> Sys.file_exists (Filename.concat (Harness.home ()) ".gemini"));
    install =
      (fun () ->
        let path = settings_path () in
        let pre = Harness.install_json_file ~timeout:timeout_ms ~harness_id:"gemini" ~path ~event:"BeforeTool" ~matcher ~command:(Harness.mark_command ()) () in
        let post = Harness.install_json_file ~timeout:timeout_ms ~harness_id:"gemini" ~path ~event:"AfterTool" ~matcher ~command:(Harness.agent_command ~harness_id:"gemini") () in
        { post with changed = pre.changed || post.changed });
    uninstall = (fun () -> Harness.uninstall_json_file ~harness_id:"gemini" ~path:(settings_path ()));
    render_feedback = Harness.render_camel
  }
