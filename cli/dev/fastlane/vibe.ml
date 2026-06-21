(* Mistral Vibe adapter. Vibe modelled its hooks on Claude Code but DIVERGED in three ways the adapter
   must own: (1) config is a FLAT `[[hooks]]` TOML array in a separate ~/.vibe/hooks.toml (not nested
   matcher groups, not the main config); (2) the edit event is `after_tool` matching Vibe's edit tools
   (`edit` / `write_file`); (3) the injected feedback is snake_case `hook_specific_output.
   additional_context` (vs Claude/Codex camelCase). Hooks are also behind an experimental flag, which we
   enable in ~/.vibe/config.toml. A user-global hooks.toml needs no trusted-folder (only project-local
   does), and the command's own root-guard scopes it — same model as the other harnesses. *)

let hooks_path () = Filename.concat (Filename.concat (Harness.home ()) ".vibe") "hooks.toml"
let config_path () = Filename.concat (Filename.concat (Harness.home ()) ".vibe") "config.toml"
let edit_match = "re:^(edit|write_file)$"

(* ── tiny TOML line/block helpers (Vibe's flat array can't reuse the JSON kit) ──────────────────── *)

let line_start_of text i =
  let rec back j = if j <= 0 then 0 else if text.[j - 1] = '\n' then j else back (j - 1) in
  back i

let line_end_of text i =
  match Harness.find_sub (String.sub text i (String.length text - i)) "\n" with
  | Some rel -> i + rel
  | None -> String.length text

(* the `[[hooks]]` table that immediately precedes [pos] (our block's start), or 0 *)
let block_start text pos =
  let needle = "[[hooks]]" in
  let rec last i best = match Harness.find_sub (String.sub text i (String.length text - i)) needle with
    | Some rel when i + rel < pos -> last (i + rel + 1) (i + rel)
    | _ -> best
  in
  last 0 0

(* the next `[[hooks]]` table at or after [pos], or end-of-text *)
let block_end text pos =
  match Harness.find_sub (String.sub text pos (String.length text - pos)) "[[hooks]]" with
  | Some rel -> pos + rel
  | None -> String.length text

(* remove the single `[[hooks]]` block carrying our marker, if present *)
let strip_block ~marker text =
  match Harness.find_sub text marker with
  | None -> (text, false)
  | Some mpos ->
    let bstart = block_start text mpos in
    let bend = block_end text (bstart + String.length "[[hooks]]") in
    let kept = String.sub text 0 bstart ^ String.sub text bend (String.length text - bend) in
    (kept, true)

let block ~root ~agent_dir =
  let command = Harness.guarded_command ~harness_id:"vibe" ~root ~agent_dir in
  Printf.sprintf
    "[[hooks]]\nname = \"fennec-fastlane\"\ntype = \"after_tool\"\nmatch = %s\ncommand = %s\ntimeout = 15.0\ndescription = \"Fennec dev verdict after each edit\"\n"
    (Harness.toml_basic_escape edit_match) (Harness.toml_basic_escape command)

(* TOML top-level keys must precede any table, so set/prepend the flag rather than append *)
let enable_experimental text =
  let key = "enable_experimental_hooks" in
  match Harness.find_sub text key with
  | Some i ->
    let ls = line_start_of text i and le = line_end_of text i in
    String.sub text 0 ls ^ key ^ " = true" ^ String.sub text le (String.length text - le)
  | None -> key ^ " = true\n" ^ text

let install ~root ~agent_dir =
  let path = hooks_path () in
  let marker = Harness.marker ~root in
  let old = Option.value (Harness.read_file path) ~default:"" in
  let stripped, had = strip_block ~marker old in
  let sep = if String.trim stripped = "" then "" else (if String.ends_with ~suffix:"\n" stripped then "\n" else "\n\n") in
  let next = stripped ^ sep ^ block ~root ~agent_dir in
  Harness.write_file path next;
  (* enable the experimental flag (persistent) — detach never removes it (it gates ALL hooks) *)
  let cfg = config_path () in
  let oldc = Option.value (Harness.read_file cfg) ~default:"" in
  let nextc = enable_experimental oldc in
  if nextc <> oldc then Harness.write_file cfg nextc;
  { Harness.harness = "vibe"; path; changed = next <> old;
    message = (if had then "hook config refreshed; preserved existing hooks" else "hook config installed") }

let uninstall ~root =
  let path = hooks_path () in
  let marker = Harness.marker ~root in
  match Harness.read_file path with
  | None -> { Harness.harness = "vibe"; path; changed = false; message = "nothing to remove (no config)" }
  | Some text -> (
    match strip_block ~marker text with
    | _, false -> { Harness.harness = "vibe"; path; changed = false; message = "no fennec hook present" }
    | next, true ->
      Harness.write_file path next;
      { Harness.harness = "vibe"; path; changed = true; message = "fennec hook removed; preserved existing hooks" })

let render_feedback ~event:_ ~text =
  "{\"hook_specific_output\":{\"additional_context\":" ^ Harness.json_escape text ^ "}}"

let adapter : Harness.t =
  { id = "vibe";
    detect = (fun () -> Harness.getenv "VIBE_SESSION_ID" <> None || Harness.getenv "MISTRAL_VIBE" <> None);
    install;
    uninstall;
    render_feedback
  }

(* ── tests ──────────────────────────────────────────────────────────────────────────────────────*)

let%test "the hooks block is a flat [[hooks]] after_tool entry with the edit matcher" =
  let b = block ~root:"/tmp/p" ~agent_dir:"/tmp/a" in
  Harness.contains b "[[hooks]]" && Harness.contains b "type = \"after_tool\"" && Harness.contains b "edit|write_file"
  && Harness.contains b "fennec-fastlane:"

let%test "render uses snake_case hook_specific_output.additional_context" =
  let j = render_feedback ~event:"after_tool" ~text:"build ok" in
  Harness.contains j "\"hook_specific_output\"" && Harness.contains j "\"additional_context\":\"build ok\"" && not (Harness.contains j "additionalContext")

let%test "strip removes only our marked block, keeps an unrelated one" =
  let other = "[[hooks]]\nname = \"keep\"\ntype = \"before_tool\"\ncommand = \"echo keep\"\n" in
  let ours = "[[hooks]]\nname = \"fennec-fastlane\"\ncommand = \"x # fennec-fastlane:abc\"\n" in
  let kept, had = strip_block ~marker:"fennec-fastlane:abc" (other ^ ours) in
  had && Harness.contains kept "echo keep" && not (Harness.contains kept "fennec-fastlane:abc")

let%test "install then strip round-trips to no fennec block" =
  let marker = Harness.marker ~root:"/tmp/p" in
  let installed = "x = 1\n" ^ block ~root:"/tmp/p" ~agent_dir:"/tmp/a" in
  let kept, had = strip_block ~marker installed in
  had && Harness.contains kept "x = 1" && not (Harness.contains kept marker)

let%test "enable_experimental flips an existing false and prepends when absent" =
  Harness.contains (enable_experimental "enable_experimental_hooks = false\n") "enable_experimental_hooks = true"
  && not (Harness.contains (enable_experimental "enable_experimental_hooks = false\n") "false")
  && String.length (enable_experimental "[model]\nx = 1\n") > 0
  && Harness.contains (enable_experimental "[model]\nx = 1\n") "enable_experimental_hooks = true"
