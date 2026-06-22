open Fennec_discover_core.Discover_model

let snapshot = Snapshot_data.snapshot

(* The tool's own how-to, printed when `fennec discover` is run with nothing to discover. So the
   discover tool teaches itself from inside the CLI — you don't need to have read a README to learn
   what it is or how to drive it. Kept terminal-shaped (one screen, plain text). *)
let usage =
  String.concat "\n"
    [
      "fennec discover — find the Fennec way for a task, before you edit.";
      "";
      "This is NOT symbol lookup (your editor, LSP, and grep do that once you know the name). It";
      "answers the earlier question — \"what does Fennec already provide for this, and which public";
      "path do I use?\" — from the shipped, source-generated snapshot, with a source-backed example";
      "and a guided next step. Run it before guessing an API from memory.";
      "";
      "Plan a task (the default — describe what you want to do):";
      "  fennec discover \"protect a route with basic auth\"";
      "  fennec discover \"add signed cookie-backed sessions\"";
      "  fennec discover \"build an SSR page with a local counter\"";
      "  fennec discover \"write an HTTP test\"";
      "";
      "Explore a module surface:";
      "  fennec discover --browse Paw";
      "  fennec discover --browse Fennec.Accounts";
      "";
      "Explain one id (copy it from a card's Next: line):";
      "  fennec discover --why api:Paw.Session.make";
      "";
      "Flags: --more expands a card · --json emits the same card as schema-versioned JSON (for agents).";
      "";
    ]

let run opts =
  match (opts.query, opts.why, opts.browse) with
  | None, None, None when not opts.json -> usage
  | _ ->
    let card = Fennec_discover_core.Query.run (snapshot ()) opts in
    if opts.json then Fennec_discover_core.Render_json.render card ^ "\n"
    else Fennec_discover_core.Render_text.render card

let check () = Fennec_discover_core.Golden.check (snapshot ())

let%test "embedded snapshot parses" =
  let s = snapshot () in
  s.schema_version = Fennec_discover_core.Snapshot.schema_version

let%test "bare discover (no task) teaches the tool, not a browse card" =
  let u = run { json = false; more = false; query = None; why = None; browse = None } in
  Fennec_hunt_unit.str_contains u "find the Fennec way for a task"
  && Fennec_hunt_unit.str_contains u "--browse"
  && Fennec_hunt_unit.str_contains u "--why"
  && not (Fennec_hunt_unit.str_contains u "Public surface")
