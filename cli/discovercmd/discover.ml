open Fennec_discover_core.Discover_model

let snapshot = Snapshot_data.snapshot

let run opts =
  let card = Fennec_discover_core.Query.run (snapshot ()) opts in
  if opts.json then Fennec_discover_core.Render_json.render card ^ "\n"
  else Fennec_discover_core.Render_text.render card

let check () = Fennec_discover_core.Golden.check (snapshot ())

let%test "embedded snapshot parses" =
  let s = snapshot () in
  s.schema_version = Fennec_discover_core.Snapshot.schema_version
