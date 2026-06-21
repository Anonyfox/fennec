(* Install / remove the fastlane hook across harnesses. Install defaults to the harnesses actually
   PRESENT on the machine ({!Registry.installed}) — so `dev --agent` never litters configs for tools
   you don't use; pass an explicit list to override. `--detach` removes our marked hook from EVERY
   known harness so a stale config can never linger. Both are idempotent and preserve other settings. *)

let install ?harnesses () =
  let hs = match harnesses with Some hs -> hs | None -> Registry.installed () in
  List.map (fun (h : Harness.t) -> h.install ()) hs

let uninstall ?harnesses () =
  let hs = match harnesses with Some hs -> hs | None -> Registry.all in
  List.map (fun (h : Harness.t) -> h.uninstall ()) hs

let lines verb results =
  String.concat "\n"
    (List.map (fun (r : Harness.result) -> Printf.sprintf "agent %s %s: %s (%s)" verb r.harness r.message r.path) results)

let report results =
  lines "attach" results
  ^ "\nagent attach: ready; edit normally and consume the next post-edit Fennec feedback block"

let detach_report results = lines "detach" results
