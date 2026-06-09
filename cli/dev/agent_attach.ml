type harness = Claude | Codex

type result = { harness : harness; path : string; changed : bool; message : string }

let harness_name = function Claude -> "claude" | Codex -> "codex"

let getenv name = match Sys.getenv_opt name with Some "" | None -> None | Some v -> Some v

let home () =
  match getenv "HOME" with
  | Some h -> h
  | None -> "."

let mkdir_p dir =
  let rec go d =
    if d = "" || d = "." || d = "/" || Sys.file_exists d then ()
    else (go (Filename.dirname d); try Unix.mkdir d 0o755 with Unix.Unix_error (Unix.EEXIST, _, _) -> ())
  in
  go dir

let read_file path =
  if Sys.file_exists path then Some (In_channel.with_open_text path In_channel.input_all) else None

let write_file path s =
  mkdir_p (Filename.dirname path);
  let tmp = path ^ ".tmp" in
  Fun.protect
    ~finally:(fun () -> if Sys.file_exists tmp then try Sys.remove tmp with _ -> ())
    (fun () ->
      Out_channel.with_open_text tmp (fun oc -> output_string oc s);
      Sys.rename tmp path)

let json_escape s =
  let b = Buffer.create (String.length s + 16) in
  Buffer.add_char b '"';
  String.iter
    (function
      | '"' -> Buffer.add_string b "\\\""
      | '\\' -> Buffer.add_string b "\\\\"
      | '\n' -> Buffer.add_string b "\\n"
      | '\r' -> Buffer.add_string b "\\r"
      | '\t' -> Buffer.add_string b "\\t"
      | c when Char.code c < 0x20 -> Buffer.add_string b (Printf.sprintf "\\u%04x" (Char.code c))
      | c -> Buffer.add_char b c)
    s;
  Buffer.add_char b '"';
  Buffer.contents b

let toml_basic_escape = json_escape

let contains hay needle =
  let lh = String.length hay and ln = String.length needle in
  let rec go i = i + ln <= lh && (String.sub hay i ln = needle || go (i + 1)) in
  ln = 0 || go 0

let find_sub hay needle =
  let lh = String.length hay and ln = String.length needle in
  let rec go i =
    if ln = 0 then Some 0
    else if i + ln > lh then None
    else if String.sub hay i ln = needle then Some i
    else go (i + 1)
  in
  go 0

let absolute_exe () =
  let exe = Sys.executable_name in
  if Filename.is_relative exe then Filename.concat (Sys.getcwd ()) exe else exe

let marker ~root = "fennec-fastlane:" ^ Digest.to_hex (Digest.string root)

let guarded_command ~root ~agent_dir =
  let py =
    Printf.sprintf
      "import json, os, subprocess, sys\n\
       root=%S\n\
       agent_dir=%S\n\
       exe=%S\n\
       try:\n\
       \    data=json.load(sys.stdin)\n\
       except Exception:\n\
       \    data={}\n\
       cwd=data.get('cwd') or os.getcwd()\n\
       try:\n\
       \    active=os.path.commonpath([os.path.realpath(root), os.path.realpath(cwd)])==os.path.realpath(root)\n\
       except Exception:\n\
       \    active=False\n\
       if not active:\n\
       \    sys.exit(0)\n\
       os.execv(exe, [exe, 'agent', 'hook', '--dir', agent_dir, '--timeout', '12'])"
      root agent_dir (absolute_exe ())
  in
  "python3 -c " ^ Filename.quote py ^ " # " ^ marker ~root

let codex_trust_hash_identity ~matcher ~command ~timeout ~status_message =
  let identity =
    "{\"event_name\":\"post_tool_use\",\"hooks\":[{\"async\":false,\"command\":"
    ^ json_escape command
    ^ ",\"statusMessage\":"
    ^ json_escape status_message
    ^ ",\"timeout\":"
    ^ string_of_int timeout
    ^ ",\"type\":\"command\"}],\"matcher\":"
    ^ json_escape matcher
    ^ "}"
  in
  "sha256:" ^ Digestif.SHA256.(to_hex (digest_string identity))

let codex_trust_hash ~matcher ~command =
  codex_trust_hash_identity ~matcher ~command ~timeout:15 ~status_message:"Fennec feedback..."

let codex_trust_key ~hooks_path = hooks_path ^ ":post_tool_use:0:0"

let line_end s start =
  match find_sub (String.sub s start (String.length s - start)) "\n" with
  | Some rel -> start + rel
  | None -> String.length s

let replace_or_insert_trusted_hash text ~table ~hash =
  match find_sub text table with
  | None ->
    let prefix = if String.trim text = "" || String.ends_with ~suffix:"\n" text then text else text ^ "\n" in
    prefix ^ "\n" ^ table ^ "\ntrusted_hash = " ^ toml_basic_escape hash ^ "\n"
  | Some table_start ->
    let table_end = line_end text table_start in
    let after_table = if table_end < String.length text then table_end + 1 else table_end in
    let next_table =
      match find_sub (String.sub text after_table (String.length text - after_table)) "\n[" with
      | Some rel -> after_table + rel + 1
      | None -> String.length text
    in
    let section = String.sub text after_table (next_table - after_table) in
    let new_line = "trusted_hash = " ^ toml_basic_escape hash in
    (match find_sub section "trusted_hash" with
    | Some rel ->
      let key_start = after_table + rel in
      let key_end = line_end text key_start in
      String.sub text 0 key_start ^ new_line ^ String.sub text key_end (String.length text - key_end)
    | None ->
      String.sub text 0 after_table ^ new_line ^ "\n"
      ^ String.sub text after_table (String.length text - after_table))

let install_codex_trust ~hooks_path ~matcher ~command =
  let path = Filename.concat (Filename.concat (home ()) ".codex") "config.toml" in
  let table = "[hooks.state." ^ toml_basic_escape (codex_trust_key ~hooks_path) ^ "]" in
  let hash = codex_trust_hash ~matcher ~command in
  let old = Option.value (read_file path) ~default:"" in
  let next = replace_or_insert_trusted_hash old ~table ~hash in
  if next <> old then write_file path next

let hook_entry_json ~matcher ~command =
  `Assoc
    [ ("matcher", `String matcher);
      ( "hooks",
        `List
          [ `Assoc
              [ ("type", `String "command");
                ("command", `String command);
                ("timeout", `Int 15);
                ("statusMessage", `String "Fennec feedback...") ] ] ) ]

let hook_json ~matcher ~command =
  Yojson.Basic.pretty_to_string
    (`Assoc [ ("hooks", `Assoc [ ("PostToolUse", `List [ hook_entry_json ~matcher ~command ]) ]) ])
  ^ "\n"

let assoc_replace key value fields =
  let rec go acc = function
    | [] -> List.rev ((key, value) :: acc)
    | (k, _) :: rest when k = key -> List.rev_append acc ((key, value) :: rest)
    | field :: rest -> go (field :: acc) rest
  in
  go [] fields

let merge_hook_json text ~matcher ~command ~marker =
  let open Yojson.Basic in
  let value = from_string text in
  match value with
  | `Assoc fields ->
    let hooks_fields =
      match List.assoc_opt "hooks" fields with
      | None -> []
      | Some (`Assoc xs) -> xs
      | Some _ -> raise (Failure "existing hooks field is not a JSON object")
    in
    let current =
      match List.assoc_opt "PostToolUse" hooks_fields with
      | None -> []
      | Some (`List xs) -> xs
      | Some _ -> raise (Failure "existing PostToolUse hooks field is not a JSON array")
    in
    let keep entry = not (contains (to_string entry) marker) in
    let post = `List (List.filter keep current @ [ hook_entry_json ~matcher ~command ]) in
    let hooks = `Assoc (assoc_replace "PostToolUse" post hooks_fields) in
    Ok (pretty_to_string (`Assoc (assoc_replace "hooks" hooks fields)) ^ "\n")
  | _ -> Error "existing config is not a JSON object; left unchanged"
  | exception Yojson.Json_error msg -> Error ("existing config is not valid JSON: " ^ msg)
  | exception Failure msg -> Error (msg ^ "; left unchanged")

let install_json_file ~harness ~path ~matcher ~root ~agent_dir =
  let command = guarded_command ~root ~agent_dir in
  let marker = marker ~root in
  let expected = hook_json ~matcher ~command in
  match read_file path with
  | Some text when contains text marker ->
    if String.trim text = String.trim expected then
      { harness; path; changed = false; message = "hook config already installed" }
    else
      (match merge_hook_json text ~matcher ~command ~marker with
      | Ok merged ->
        write_file path merged;
        { harness; path; changed = true; message = "hook config refreshed; preserved existing settings" }
      | Error message -> { harness; path; changed = false; message })
  | Some text when String.trim text <> "" ->
    (match merge_hook_json text ~matcher ~command ~marker with
    | Ok merged ->
      write_file path merged;
      { harness; path; changed = true; message = "hook config installed; preserved existing settings" }
    | Error message -> { harness; path; changed = false; message })
  | _ ->
    write_file path expected;
    { harness; path; changed = true; message = "hook config installed" }

let install_claude ~root ~agent_dir =
  let path = Filename.concat (Filename.concat (home ()) ".claude") "settings.json" in
  install_json_file ~harness:Claude ~path ~matcher:"Edit|Write|MultiEdit" ~root ~agent_dir

let install_codex ~root ~agent_dir =
  let path = Filename.concat (Filename.concat (home ()) ".codex") "hooks.json" in
  let matcher = "^Edit$|^Write$|^apply_patch$" in
  let command = guarded_command ~root ~agent_dir in
  let result = install_json_file ~harness:Codex ~path ~matcher ~root ~agent_dir in
  install_codex_trust ~hooks_path:path ~matcher ~command;
  result

let choose_harnesses ~is_claude ~is_codex =
  if is_claude then [ Claude ] else if is_codex then [ Codex ] else [ Claude; Codex ]

let active_harnesses () =
  choose_harnesses
    ~is_claude:
      (getenv "CLAUDECODE" <> None || getenv "CLAUDE_CODE" <> None
     || getenv "CLAUDE_CODE_ENTRYPOINT" <> None || getenv "CLAUDE_SESSION_ID" <> None)
    ~is_codex:(getenv "CODEX_THREAD_ID" <> None || getenv "CODEX_CI" <> None)

let install ?harnesses ~root ~agent_dir () =
  let harnesses = Option.value harnesses ~default:(active_harnesses ()) in
  List.map
    (function
      | Claude -> install_claude ~root ~agent_dir
      | Codex -> install_codex ~root ~agent_dir)
    harnesses

let report results =
  String.concat "\n"
    (List.map
       (fun r ->
         Printf.sprintf "agent attach %s: %s (%s)" (harness_name r.harness) r.message r.path)
       results)
  ^ "\nagent attach: ready; edit normally and consume the next post-edit Fennec feedback block"

(* inline tests *)
let%test "guarded command carries stable fennec marker" =
  contains (guarded_command ~root:"/tmp/project" ~agent_dir:"/tmp/agent") "fennec-fastlane:"

let%test "hook json includes post-tool matcher and command" =
  let json = hook_json ~matcher:"Edit|Write" ~command:"fennec agent hook" in
  contains json "\"PostToolUse\"" && contains json "fennec agent hook"

let%test "codex trust hash matches codex canonical identity" =
  let command =
    "python3 -c 'import json,sys; open(\"/tmp/fennec-codex-hook-fired.log\",\"a\").write(sys.stdin.read()); print(\"{\\\"hookSpecificOutput\\\":{\\\"hookEventName\\\":\\\"PostToolUse\\\",\\\"additionalContext\\\":\\\"CODEX HOOK FIRED\\\"}}\")'"
  in
  codex_trust_hash_identity ~matcher:"Bash" ~command ~timeout:5 ~status_message:"Codex hook smoke"
  = "sha256:58b9e0321fecc60b43c9d61001ef97ef2bedcd385cda248b117608bfff2b9db8"

let%test "codex trust insertion appends hook state" =
  let table = "[hooks.state.\"/tmp/hooks.json:post_tool_use:0:0\"]" in
  let text = replace_or_insert_trusted_hash "model = \"gpt\"\n" ~table ~hash:"sha256:new" in
  contains text "model = \"gpt\"" && contains text table && contains text "trusted_hash = \"sha256:new\""

let%test "codex trust replacement updates stale hook hash" =
  let table = "[hooks.state.\"/tmp/hooks.json:post_tool_use:0:0\"]" in
  let old = table ^ "\ntrusted_hash = \"sha256:old\"\n[other]\nx = 1\n" in
  let text = replace_or_insert_trusted_hash old ~table ~hash:"sha256:new" in
  contains text "trusted_hash = \"sha256:new\"" && contains text "[other]"
  && not (contains text "sha256:old")

let%test "merge preserves existing top-level settings" =
  match merge_hook_json "{\n  \"model\": \"haiku\"\n}\n" ~matcher:"Edit" ~command:"fennec agent hook" ~marker:"fennec-fastlane:abc" with
  | Ok json -> contains json "\"model\": \"haiku\"" && contains json "\"hooks\""
  | Error _ -> false

let%test "merge refuses non-object settings" =
  match merge_hook_json "[]" ~matcher:"Edit" ~command:"fennec agent hook" ~marker:"fennec-fastlane:abc" with
  | Ok _ -> false
  | Error msg -> contains msg "not a JSON object"

let%test "merge replaces marked fennec hook and keeps unrelated hooks" =
  let old =
    "{\n\
    \  \"hooks\": {\n\
    \    \"PostToolUse\": [\n\
    \      {\"matcher\":\"Bash\",\"hooks\":[{\"type\":\"command\",\"command\":\"echo old # fennec-fastlane:abc\"}]},\n\
    \      {\"matcher\":\"Bash\",\"hooks\":[{\"type\":\"command\",\"command\":\"echo keep\"}]}\n\
    \    ],\n\
    \    \"Stop\": [{\"hooks\":[{\"type\":\"command\",\"command\":\"echo stop\"}]}]\n\
    \  }\n\
     }"
  in
  match merge_hook_json old ~matcher:"Edit" ~command:"fennec agent hook # fennec-fastlane:abc" ~marker:"fennec-fastlane:abc" with
  | Error _ -> false
  | Ok json ->
    contains json "echo keep" && contains json "echo stop" && contains json "fennec agent hook"
    && not (contains json "echo old")

let%test "merge preserves unrelated hooks in hooks-only config" =
  let old =
    "{\n\
    \  \"hooks\": {\n\
    \    \"PostToolUse\": [\n\
    \      {\"matcher\":\"Bash\",\"hooks\":[{\"type\":\"command\",\"command\":\"echo old # fennec-fastlane:abc\"}]},\n\
    \      {\"matcher\":\"Write\",\"hooks\":[{\"type\":\"command\",\"command\":\"echo keep\"}]}\n\
    \    ]\n\
    \  }\n\
     }"
  in
  match merge_hook_json old ~matcher:"Edit" ~command:"fennec agent hook # fennec-fastlane:abc" ~marker:"fennec-fastlane:abc" with
  | Error _ -> false
  | Ok json -> contains json "echo keep" && contains json "fennec agent hook" && not (contains json "echo old")

let with_temp_path f =
  let dir =
    Filename.concat (Filename.get_temp_dir_name ())
      ("fennec-agent-attach-test-" ^ string_of_int (Unix.getpid ()) ^ "-" ^ string_of_float (Unix.gettimeofday ()))
  in
  Fun.protect
    ~finally:(fun () -> ignore (Sys.command (Printf.sprintf "rm -rf %s" (Filename.quote dir))))
    (fun () ->
      mkdir_p dir;
      f (Filename.concat dir "hooks.json"))

let%test_unit "installer preserves existing hooks while replacing stale fennec entry" =
  let chk = Fennec_hunt_unit.check in
  with_temp_path (fun path ->
      let root = "/tmp/root" in
      let stale_marker = marker ~root in
      write_file path
        ("{\n\
         \  \"model\": \"gpt\",\n\
         \  \"hooks\": {\n\
         \    \"PostToolUse\": [\n\
         \      {\"matcher\":\"Bash\",\"hooks\":[{\"type\":\"command\",\"command\":\"echo keep\"}]},\n\
         \      {\"matcher\":\"Edit\",\"hooks\":[{\"type\":\"command\",\"command\":\"echo stale # "
        ^ stale_marker
        ^ "\"}]}\n\
         \    ]\n\
         \  }\n\
          }\n");
      let result = install_json_file ~harness:Codex ~path ~matcher:"Edit" ~root ~agent_dir:"/tmp/agent" in
      let text = Option.value (read_file path) ~default:"" in
      chk "installer wrote merged config" result.changed;
      chk "model setting preserved" (contains text "\"model\": \"gpt\"");
      chk "unrelated hook preserved" (contains text "echo keep");
      chk "stale fennec hook removed" (not (contains text "echo stale"));
      chk "fresh guarded fennec command installed" (contains text "os.execv" && contains text stale_marker))

let%test_unit "installer is idempotent for identical generated config" =
  let chk = Fennec_hunt_unit.check in
  with_temp_path (fun path ->
      let first = install_json_file ~harness:Codex ~path ~matcher:"Edit" ~root:"/tmp/root" ~agent_dir:"/tmp/agent" in
      let second = install_json_file ~harness:Codex ~path ~matcher:"Edit" ~root:"/tmp/root" ~agent_dir:"/tmp/agent" in
      chk "first install writes" first.changed;
      chk "second install is no-op" (not second.changed))

let%test "report does not include hook proof phrase" =
  not
    (contains
       (report [ { harness = Codex; path = "/tmp/hooks.json"; changed = true; message = "hook config installed" } ])
       "Fennec dev feedback after this tool")

let%test "claude environment wins when nested under codex" =
  choose_harnesses ~is_claude:true ~is_codex:true = [ Claude ]

let%test "codex environment attaches codex when not nested" =
  choose_harnesses ~is_claude:false ~is_codex:true = [ Codex ]

let%test "unknown environment installs both known harnesses" =
  choose_harnesses ~is_claude:false ~is_codex:false = [ Claude; Codex ]
