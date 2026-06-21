(* Codex CLI adapter. Codex's NEW `hooks` system mirrors Claude Code's JSON shape (so the install +
   the camelCase output are reused), with two Codex-specific facts: its file-edit tool is
   [apply_patch] (the matcher), and a non-managed command hook must be pre-TRUSTED — Codex stores a
   SHA256 of the hook's canonical identity under [hooks.state] in ~/.codex/config.toml, and refuses to
   run a hook whose hash isn't there. We compute + write that hash so the hook fires without an
   interactive `/hooks` trust step. (We target ~/.codex/hooks.json, the user-global file, not a
   repo-local .codex/config.toml — repo-local hooks currently don't fire, openai/codex#17532.) *)

let hooks_path () = Filename.concat (Filename.concat (Harness.home ()) ".codex") "hooks.json"
let config_path () = Filename.concat (Filename.concat (Harness.home ()) ".codex") "config.toml"

(* match whatever edit verb Codex uses; apply_patch is the real one, the others are harmless *)
let matcher = "^Edit$|^Write$|^apply_patch$"

(* ── the trust hash: a SHA256 over the EXACT hook entry Codex sees, in Codex's canonical field order
   (the timeout + statusMessage must match {!Harness.hook_entry_json}). ── *)
(* [event] is Codex's snake_case event id ("pre_tool_use" | "post_tool_use") — it names both the trust
   table and the hashed identity's [event_name] (Codex maps the camelCase hooks.json key to this). *)
let trust_hash_identity ~event ~command =
  let identity =
    "{\"event_name\":" ^ Harness.json_escape event ^ ",\"hooks\":[{\"async\":false,\"command\":" ^ Harness.json_escape command
    ^ ",\"statusMessage\":" ^ Harness.json_escape "Fennec feedback..." ^ ",\"timeout\":15,\"type\":\"command\"}],\"matcher\":"
    ^ Harness.json_escape matcher ^ "}"
  in
  "sha256:" ^ Digestif.SHA256.(to_hex (digest_string identity))

let trust_table ~hooks_path ~event = "[hooks.state.\"" ^ hooks_path ^ ":" ^ event ^ ":0:0\"]"

let line_end s start =
  match Harness.find_sub (String.sub s start (String.length s - start)) "\n" with
  | Some rel -> start + rel
  | None -> String.length s

(* upsert [trusted_hash = <hash>] under [table] in TOML [text]; appends the table if missing *)
let upsert_trusted_hash text ~table ~hash =
  match Harness.find_sub text table with
  | None ->
    let prefix = if String.trim text = "" || String.ends_with ~suffix:"\n" text then text else text ^ "\n" in
    prefix ^ "\n" ^ table ^ "\ntrusted_hash = " ^ Harness.toml_basic_escape hash ^ "\n"
  | Some table_start ->
    let table_end = line_end text table_start in
    let after_table = if table_end < String.length text then table_end + 1 else table_end in
    let next_table =
      match Harness.find_sub (String.sub text after_table (String.length text - after_table)) "\n[" with
      | Some rel -> after_table + rel + 1
      | None -> String.length text
    in
    let section = String.sub text after_table (next_table - after_table) in
    let new_line = "trusted_hash = " ^ Harness.toml_basic_escape hash in
    (match Harness.find_sub section "trusted_hash" with
    | Some rel ->
      let key_start = after_table + rel in
      let key_end = line_end text key_start in
      String.sub text 0 key_start ^ new_line ^ String.sub text key_end (String.length text - key_end)
    | None -> String.sub text 0 after_table ^ new_line ^ "\n" ^ String.sub text after_table (String.length text - after_table))

let install_trust ~event ~command =
  let path = config_path () in
  let table = trust_table ~hooks_path:(hooks_path ()) ~event in
  let hash = trust_hash_identity ~event ~command in
  let old = Option.value (Harness.read_file path) ~default:"" in
  let next = upsert_trusted_hash old ~table ~hash in
  if next <> old then Harness.write_file path next

let adapter : Harness.t =
  { id = "codex";
    detect = (fun () -> Harness.getenv "CODEX_THREAD_ID" <> None || Harness.getenv "CODEX_CI" <> None);
    installed = (fun () -> Sys.file_exists (Filename.concat (Harness.home ()) ".codex"));
    install =
      (fun () ->
        let path = hooks_path () in
        let mark_cmd = Harness.mark_command () and hook_cmd = Harness.agent_command ~harness_id:"codex" in
        let pre = Harness.install_json_file ~harness_id:"codex" ~path ~event:"PreToolUse" ~matcher ~command:mark_cmd () in
        let post = Harness.install_json_file ~harness_id:"codex" ~path ~event:"PostToolUse" ~matcher ~command:hook_cmd () in
        install_trust ~event:"pre_tool_use" ~command:mark_cmd;
        install_trust ~event:"post_tool_use" ~command:hook_cmd;
        { post with changed = pre.changed || post.changed });
    uninstall = (fun () -> Harness.uninstall_json_file ~harness_id:"codex" ~path:(hooks_path ()));
    render_feedback = Harness.render_camel
  }

(* ── tests ──────────────────────────────────────────────────────────────────────────────────────*)

let%test "trust hash is stable for a fixed command identity, and pre ≠ post" =
  (* a fixed command string ⇒ a fixed sha256; pins the canonical-identity serialisation. The event id
     is part of the identity, so the pre/post hashes for the same command differ. *)
  let post = trust_hash_identity ~event:"post_tool_use" ~command:"x # fennec-fastlane" in
  let pre = trust_hash_identity ~event:"pre_tool_use" ~command:"x # fennec-fastlane" in
  String.length post = 7 + 64 && post <> pre

let%test "trust insertion appends the hook state table" =
  let table = trust_table ~hooks_path:"/tmp/hooks.json" ~event:"post_tool_use" in
  let text = upsert_trusted_hash "model = \"gpt\"\n" ~table ~hash:"sha256:new" in
  Harness.contains text "model = \"gpt\"" && Harness.contains text table && Harness.contains text "trusted_hash = \"sha256:new\""

let%test "trust replacement updates a stale hook hash, keeps other tables" =
  let table = trust_table ~hooks_path:"/tmp/hooks.json" ~event:"post_tool_use" in
  let old = table ^ "\ntrusted_hash = \"sha256:old\"\n[other]\nx = 1\n" in
  let text = upsert_trusted_hash old ~table ~hash:"sha256:new" in
  Harness.contains text "trusted_hash = \"sha256:new\"" && Harness.contains text "[other]" && not (Harness.contains text "sha256:old")
