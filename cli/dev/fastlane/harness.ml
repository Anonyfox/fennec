(* The HARNESS ADAPTER and the shared "kit" every adapter is built from. One coding harness (Claude
   Code, Codex, Mistral Vibe) = one {!t} value registered in {!Registry}; adding a harness is a new
   value, never a change to the core or to this kit. An adapter owns only the LOAD-BEARING DELTAS:
   where its hook config lives, the matcher/event for an edit, and the exact JSON shape it wants the
   feedback wrapped in (the only thing the core can't be generic about). Everything mechanical —
   resolving the running fennec exe, the root guard, idempotent install/merge, removal — lives here. *)

type result = { harness : string; path : string; changed : bool; message : string }

(* A harness adapter. [install]/[uninstall] write or remove ONLY our marked hook, preserving the
   user's other settings. [render_feedback] wraps the harness-agnostic feedback text ({!Journal.
   feedback_text}) in this harness's injection JSON — the single per-harness output difference. *)
type t = {
  id : string;                                              (* stable key: "claude" | "codex" | "vibe" *)
  detect : unit -> bool;                                    (* env sniff so `--attach` self-registers *)
  install : root:string -> agent_dir:string -> result;
  uninstall : root:string -> result;
  render_feedback : event:string -> text:string -> string;
}

(* ── tiny IO + string helpers (Stdlib + Unix only; no dep back on the supervisor) ───────────────── *)

let getenv name = match Sys.getenv_opt name with Some "" | None -> None | Some v -> Some v
let home () = match getenv "HOME" with Some h -> h | None -> "."

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

let json_escape = Journal.json_escape (* reuse the core's escaper — one JSON writer for the whole lib *)
let toml_basic_escape = json_escape

let contains hay needle =
  let lh = String.length hay and ln = String.length needle in
  let rec go i = i + ln <= lh && (String.sub hay i ln = needle || go (i + 1)) in
  ln = 0 || go 0

let find_sub hay needle =
  let lh = String.length hay and ln = String.length needle in
  let rec go i =
    if ln = 0 then Some 0 else if i + ln > lh then None else if String.sub hay i ln = needle then Some i else go (i + 1)
  in
  go 0

let absolute_exe () =
  let exe = Sys.executable_name in
  if Filename.is_relative exe then Filename.concat (Sys.getcwd ()) exe else exe

(* a stable per-project marker, baked as a trailing comment into our hook command. It makes install
   idempotent (we recognise our own entry), merge-safe (we replace only it), and uninstall precise. *)
let marker ~root = "fennec-fastlane:" ^ Digest.to_hex (Digest.string root)

(* The hook command every harness installs: a short python guard that runs `fennec agent hook` ONLY
   when the tool fired inside this project root (so one global settings file serves many repos), then
   execs the SAME fennec binary that installed it, telling it which harness called so the feedback is
   wrapped in the right shape (`--harness <id>`). The trailing `# fennec-fastlane:<hash>` is the marker. *)
let guarded_command ~harness_id ~root ~agent_dir =
  let py =
    Printf.sprintf
      "import json, os, sys\n\
       root=%S\n\
       agent_dir=%S\n\
       exe=%S\n\
       harness=%S\n\
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
       os.execv(exe, [exe, 'agent', 'hook', '--harness', harness, '--dir', agent_dir, '--timeout', '12'])"
      root agent_dir (absolute_exe ()) harness_id
  in
  "python3 -c " ^ Filename.quote py ^ " # " ^ marker ~root

(* ── the JSON-hooks kit: Claude Code + Codex both use a `hooks.PostToolUse[]` JSON file ─────────────
   (Codex's `hooks.json` mirrors Claude's `settings.json` shape; only the path, matcher and a trust
   step differ — those live in the adapters.) *)

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

(* parse an existing JSON config, drop our previously-marked PostToolUse entry, append the fresh one,
   and re-serialise — leaving every other key and hook untouched. *)
let merge_hook_json text ~matcher ~command ~marker =
  let open Yojson.Basic in
  match from_string text with
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

(* remove ONLY our marked PostToolUse entry from an existing JSON config (the detach side of merge);
   leaves the file otherwise byte-for-byte and reports whether anything changed. *)
let strip_hook_json text ~marker =
  let open Yojson.Basic in
  match from_string text with
  | `Assoc fields -> (
    match List.assoc_opt "hooks" fields with
    | Some (`Assoc hooks_fields) -> (
      match List.assoc_opt "PostToolUse" hooks_fields with
      | Some (`List current) ->
        let kept = List.filter (fun e -> not (contains (to_string e) marker)) current in
        if List.length kept = List.length current then Ok (text, false)
        else
          (* drop the PostToolUse key (and then the hooks key) entirely if our removal empties it, so a
             detach leaves no residual empty block — a fully pristine config. *)
          let hooks_fields = if kept = [] then List.remove_assoc "PostToolUse" hooks_fields else assoc_replace "PostToolUse" (`List kept) hooks_fields in
          let fields = if hooks_fields = [] then List.remove_assoc "hooks" fields else assoc_replace "hooks" (`Assoc hooks_fields) fields in
          Ok (pretty_to_string (`Assoc fields) ^ "\n", true)
      | _ -> Ok (text, false))
    | _ -> Ok (text, false))
  | _ -> Ok (text, false)
  | exception Yojson.Json_error msg -> Error ("existing config is not valid JSON: " ^ msg)

(* the idempotent installer Claude + Codex share. Writes a fresh file when none exists; otherwise
   merges (replacing a stale fennec entry, preserving everything else). *)
let install_json_file ~harness_id ~path ~matcher ~root ~agent_dir =
  let command = guarded_command ~harness_id ~root ~agent_dir in
  let marker = marker ~root in
  let expected = hook_json ~matcher ~command in
  match read_file path with
  | Some text when contains text marker ->
    if String.trim text = String.trim expected then
      { harness = harness_id; path; changed = false; message = "hook config already installed" }
    else (
      match merge_hook_json text ~matcher ~command ~marker with
      | Ok merged ->
        write_file path merged;
        { harness = harness_id; path; changed = true; message = "hook config refreshed; preserved existing settings" }
      | Error message -> { harness = harness_id; path; changed = false; message })
  | Some text when String.trim text <> "" -> (
    match merge_hook_json text ~matcher ~command ~marker with
    | Ok merged ->
      write_file path merged;
      { harness = harness_id; path; changed = true; message = "hook config installed; preserved existing settings" }
    | Error message -> { harness = harness_id; path; changed = false; message })
  | _ ->
    write_file path expected;
    { harness = harness_id; path; changed = true; message = "hook config installed" }

let uninstall_json_file ~harness_id ~path ~root =
  let marker = marker ~root in
  match read_file path with
  | None -> { harness = harness_id; path; changed = false; message = "nothing to remove (no config)" }
  | Some text -> (
    match strip_hook_json text ~marker with
    | Error message -> { harness = harness_id; path; changed = false; message }
    | Ok (_, false) -> { harness = harness_id; path; changed = false; message = "no fennec hook present" }
    | Ok (next, true) ->
      write_file path next;
      { harness = harness_id; path; changed = true; message = "fennec hook removed; preserved existing settings" })

(* the two camelCase harnesses (Claude, Codex) share one injection shape *)
let render_camel ~event ~text =
  "{\"hookSpecificOutput\":{\"hookEventName\":" ^ json_escape event ^ ",\"additionalContext\":" ^ json_escape text ^ "}}"

(* ── tests: the harness-agnostic kit ─────────────────────────────────────────────────────────────*)

let%test "guarded command carries the stable fennec marker + the baked harness id" =
  (* the python body is shell-quoted, so assert the parts that survive: the trailing marker comment and
     the baked `harness="<id>"` literal (double quotes survive single-quoting; a distinctive id proves
     it is threaded through to the `--harness` exec) *)
  let c = guarded_command ~harness_id:"vibe" ~root:"/tmp/project" ~agent_dir:"/tmp/agent" in
  contains c "fennec-fastlane:" && contains c "harness=\"vibe\""

let%test "hook json includes the post-tool matcher and command" =
  let json = hook_json ~matcher:"Edit|Write" ~command:"fennec agent hook" in
  contains json "\"PostToolUse\"" && contains json "fennec agent hook"

let%test "merge preserves existing top-level settings" =
  match merge_hook_json "{\n  \"model\": \"haiku\"\n}\n" ~matcher:"Edit" ~command:"fennec agent hook" ~marker:"fennec-fastlane:abc" with
  | Ok json -> contains json "\"model\": \"haiku\"" && contains json "\"hooks\""
  | Error _ -> false

let%test "merge refuses non-object settings" =
  match merge_hook_json "[]" ~matcher:"Edit" ~command:"fennec agent hook" ~marker:"fennec-fastlane:abc" with
  | Ok _ -> false
  | Error msg -> contains msg "not a JSON object"

let%test "merge replaces the marked fennec hook and keeps unrelated hooks" =
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
    contains json "echo keep" && contains json "echo stop" && contains json "fennec agent hook" && not (contains json "echo old")

let%test "strip removes our hook and reports the change, leaving others" =
  let old =
    "{\n\
    \  \"hooks\": {\n\
    \    \"PostToolUse\": [\n\
    \      {\"matcher\":\"Edit\",\"hooks\":[{\"type\":\"command\",\"command\":\"echo ours # fennec-fastlane:abc\"}]},\n\
    \      {\"matcher\":\"Bash\",\"hooks\":[{\"type\":\"command\",\"command\":\"echo keep\"}]}\n\
    \    ]\n\
    \  }\n\
     }"
  in
  match strip_hook_json old ~marker:"fennec-fastlane:abc" with
  | Ok (json, true) -> contains json "echo keep" && not (contains json "echo ours")
  | _ -> false

let%test "strip on a config without our hook is a no-op" =
  match strip_hook_json "{\n  \"model\": \"x\"\n}\n" ~marker:"fennec-fastlane:abc" with
  | Ok (_, false) -> true
  | _ -> false

let%test "strip the ONLY hook leaves no empty hooks/PostToolUse residue" =
  let only = "{\n  \"model\": \"x\",\n  \"hooks\": {\"PostToolUse\": [{\"matcher\":\"Edit\",\"hooks\":[{\"type\":\"command\",\"command\":\"c # fennec-fastlane:abc\"}]}]}\n}\n" in
  match strip_hook_json only ~marker:"fennec-fastlane:abc" with
  | Ok (json, true) -> contains json "\"model\"" && not (contains json "PostToolUse") && not (contains json "\"hooks\"")
  | _ -> false

let%test "render_camel wraps text as hookSpecificOutput.additionalContext" =
  let j = render_camel ~event:"PostToolUse" ~text:"build ok" in
  contains j "\"hookSpecificOutput\"" && contains j "\"additionalContext\":\"build ok\"" && contains j "\"PostToolUse\""
