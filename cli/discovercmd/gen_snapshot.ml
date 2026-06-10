let () =
  let root = match Array.to_list Sys.argv with _ :: root :: _ -> root | _ -> "." in
  Fennec_discover_core.Snapshot_gen.build ~root |> Fennec_discover_core.Snapshot_gen.emit_ocaml
