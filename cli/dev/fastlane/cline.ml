(* Cline adapter (the VS Code agent). Cline diverges from the JSON/TOML harnesses in two ways the
   adapter owns: (1) hooks are EXECUTABLE SCRIPT FILES named by the event — `PreToolUse` / `PostToolUse`
   — in a hooks dir, not entries in a config file; we install a tiny exec-wrapper per event
   ({!Harness.install_script_file}). (2) the injected field is `contextModification` (not
   additionalContext), with `{}` as the no-op ({!Harness.render_cline}). The hook fires on EVERY tool,
   so the OCaml side filters to file-edits (see {!Journal.feedback_text}); resolution leans on the
   editing cwd / `filePath` / `workspaceRoots` (Cline's payload carries no `cwd`).

   We install into ~/Documents/Cline/Hooks/, which both the VS Code extension and the SDK read (macOS/
   Linux only — Cline's hooks aren't supported on Windows). Shipped in Cline v3.36. *)

let hooks_dir () = Filename.concat (Harness.home ()) "Documents/Cline/Hooks"
let pre_path () = Filename.concat (hooks_dir ()) "PreToolUse"
let post_path () = Filename.concat (hooks_dir ()) "PostToolUse"

let adapter : Harness.t =
  { id = "cline";
    detect = (fun () -> Harness.getenv "CLINE" <> None || Harness.getenv "CLINE_ACTIVE" <> None);
    installed =
      (fun () ->
        Sys.file_exists (Filename.concat (Harness.home ()) "Documents/Cline")
        || Sys.file_exists (Filename.concat (Harness.home ()) ".cline"));
    install =
      (fun () ->
        let pre = Harness.install_script_file ~harness_id:"cline" ~path:(pre_path ()) ~command:(Harness.mark_command ()) in
        let post = Harness.install_script_file ~harness_id:"cline" ~path:(post_path ()) ~command:(Harness.agent_command ~harness_id:"cline") in
        { post with changed = pre.changed || post.changed });
    uninstall =
      (fun () ->
        let _ = Harness.uninstall_script_file ~harness_id:"cline" ~path:(pre_path ()) in
        Harness.uninstall_script_file ~harness_id:"cline" ~path:(post_path ()));
    render_feedback = Harness.render_cline
  }
