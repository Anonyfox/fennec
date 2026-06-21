(* The post-tool hook command (`fennec agent hook --harness <id>`). It computes the harness-agnostic
   feedback for the edit ({!Journal.feedback_text}) and wraps it in the calling harness's injection
   JSON ({!Harness.render_feedback}) — the single per-harness output step. The harness id is baked
   into each adapter's installed command, so a plain `fennec agent hook` (no id) defaults to Claude
   Code's shape, which Codex shares. *)

let event_of_input input =
  match Journal.find_string_field input "hook_event_name" with
  | Some s -> Journal.unescape_json_string s
  | None -> (
    match Journal.find_string_field input "hookEventName" with
    | Some s -> Journal.unescape_json_string s
    | None -> "PostToolUse")

let run ~(harness : Harness.t) ~dir ~timeout ~input =
  (* Each harness's [render_feedback] decides what EMPTY feedback (no live dev server / inert edit)
     looks like — "" for the ones that treat empty stdout as a no-op (Claude/Codex/Gemini/Vibe/Cursor),
     or "{}" for Cline which needs a JSON object. The always-on hook is invisible unless there's a verdict. *)
  harness.render_feedback ~event:(event_of_input input) ~text:(Journal.feedback_text ~dir ~timeout ~input)

let%test_unit "hook wraps the settled verdict in the harness injection shape" =
  let chk = Fennec_hunt_unit.check in
  let dir =
    Filename.concat (Filename.get_temp_dir_name ())
      ("fennec-hook-test-" ^ string_of_int (Unix.getpid ()) ^ "-" ^ string_of_float (Unix.gettimeofday ()))
  in
  Fun.protect
    ~finally:(fun () -> ignore (Sys.command (Printf.sprintf "rm -rf %s" (Filename.quote dir))))
    (fun () ->
      let t = Journal.start ~dir ~root:"/tmp/fennec-hook-test" () in
      Journal.emit t ~kind:"ready" ~summary:"ready" ();
      Journal.emit t ~kind:"reload" ~summary:"post-edit reload" ();
      let json = run ~harness:Claude.adapter ~dir ~timeout:0.1 ~input:"{}" in
      chk "carries the verdict" (Fennec_hunt_unit.str_contains json "post-edit reload");
      chk "Claude shape is camelCase additionalContext" (Fennec_hunt_unit.str_contains json "\"additionalContext\""))
