(* Cursor adapter. Cursor's ~/.cursor/hooks.json is `{version:1, hooks:{<event>:[{command, matcher}]}}`
   — a FLATTER entry than Claude's nested shape — so it gets its own installer
   ({!Harness.install_cursor_json}); removal reuses the generic strip ({!Harness.uninstall_json_file}).
   Events: `preToolUse` (mark) + `postToolUse` (hook), matched against the tool TYPE (`Write` for file
   edits). The injected field is flat snake_case `additional_context` ({!Harness.render_cursor}).
   User-global hooks need no per-workspace trust step, and Cursor's payload carries `cwd`, so multi-
   project resolution works out of the box.

   ⚠️ As of 2026-06 Cursor ACCEPTS + logs `additional_context` from postToolUse but does NOT surface it
   to the model (confirmed Cursor bug, ticket reopened). We install the documented shape anyway so the
   fastlane lights up the day they fix it — no adapter change will be needed. *)

let hooks_path () = Filename.concat (Filename.concat (Harness.home ()) ".cursor") "hooks.json"

(* Cursor's file-edit tool type is `Write`; the matcher is a regex over the tool type name. *)
let matcher = "Write"

let adapter : Harness.t =
  { id = "cursor";
    detect = (fun () -> Harness.getenv "CURSOR_TRACE_ID" <> None || Harness.getenv "CURSOR" <> None);
    installed = (fun () -> Sys.file_exists (Filename.concat (Harness.home ()) ".cursor"));
    install =
      (fun () ->
        let path = hooks_path () in
        let pre = Harness.install_cursor_json ~harness_id:"cursor" ~path ~event:"preToolUse" ~matcher ~command:(Harness.mark_command ()) in
        let post = Harness.install_cursor_json ~harness_id:"cursor" ~path ~event:"postToolUse" ~matcher ~command:(Harness.agent_command ~harness_id:"cursor") in
        { post with changed = pre.changed || post.changed });
    uninstall = (fun () -> Harness.uninstall_json_file ~harness_id:"cursor" ~path:(hooks_path ()));
    render_feedback = Harness.render_cursor
  }
