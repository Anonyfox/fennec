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

(* remove EVERY `[[hooks]]` block carrying our marker (we install two: a before_tool mark + an
   after_tool hook), leaving any non-fennec blocks untouched *)
let strip_block ~marker text =
  let rec go text removed =
    match Harness.find_sub text marker with
    | None -> (text, removed)
    | Some mpos ->
      let bstart = block_start text mpos in
      let bend = block_end text (bstart + String.length "[[hooks]]") in
      go (String.sub text 0 bstart ^ String.sub text bend (String.length text - bend)) true
  in
  go text false

let block ~name ~hook_type ~command =
  Printf.sprintf
    "[[hooks]]\nname = %s\ntype = %s\nmatch = %s\ncommand = %s\ntimeout = 15.0\ndescription = \"Fennec dev fastlane\"\n"
    (Harness.toml_basic_escape name) (Harness.toml_basic_escape hook_type) (Harness.toml_basic_escape edit_match) (Harness.toml_basic_escape command)

(* TOML top-level keys must precede any table, so set/prepend the flag rather than append *)
let enable_experimental text =
  let key = "enable_experimental_hooks" in
  match Harness.find_sub text key with
  | Some i ->
    let ls = line_start_of text i and le = line_end_of text i in
    String.sub text 0 ls ^ key ^ " = true" ^ String.sub text le (String.length text - le)
  | None -> key ^ " = true\n" ^ text

let install () =
  let path = hooks_path () in
  let marker = Harness.marker in
  let old = Option.value (Harness.read_file path) ~default:"" in
  let stripped, had = strip_block ~marker old in
  let sep = if String.trim stripped = "" then "" else (if String.ends_with ~suffix:"\n" stripped then "\n" else "\n\n") in
  (* a before_tool MARK (snapshot the journal pre-edit) + an after_tool HOOK (report this edit's verdict) *)
  let blocks =
    block ~name:"fennec-fastlane-mark" ~hook_type:"before_tool" ~command:(Harness.mark_command ())
    ^ "\n" ^ block ~name:"fennec-fastlane" ~hook_type:"after_tool" ~command:(Harness.agent_command ~harness_id:"vibe")
  in
  let next = stripped ^ sep ^ blocks in
  Harness.write_file path next;
  (* enable the experimental flag (persistent) — detach never removes it (it gates ALL hooks) *)
  let cfg = config_path () in
  let oldc = Option.value (Harness.read_file cfg) ~default:"" in
  let nextc = enable_experimental oldc in
  if nextc <> oldc then Harness.write_file cfg nextc;
  { Harness.harness = "vibe"; path; changed = next <> old;
    message = (if had then "hook config refreshed; preserved existing hooks" else "hook config installed") }

let uninstall () =
  let path = hooks_path () in
  let marker = Harness.marker in
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

let%test "block builds a flat [[hooks]] entry of the requested type with the edit matcher + marker" =
  let after = block ~name:"fennec-fastlane" ~hook_type:"after_tool" ~command:"c # fennec-fastlane" in
  let before = block ~name:"fennec-fastlane-mark" ~hook_type:"before_tool" ~command:"m # fennec-fastlane" in
  Harness.contains after "[[hooks]]" && Harness.contains after "type = \"after_tool\"" && Harness.contains after "edit|write_file"
  && Harness.contains after "fennec-fastlane" && Harness.contains before "type = \"before_tool\""

let%test "render uses snake_case hook_specific_output.additional_context" =
  let j = render_feedback ~event:"after_tool" ~text:"build ok" in
  Harness.contains j "\"hook_specific_output\"" && Harness.contains j "\"additional_context\":\"build ok\"" && not (Harness.contains j "additionalContext")

let%test "strip removes only our marked block, keeps an unrelated one" =
  let other = "[[hooks]]\nname = \"keep\"\ntype = \"before_tool\"\ncommand = \"echo keep\"\n" in
  let ours = "[[hooks]]\nname = \"fennec-fastlane\"\ncommand = \"x # fennec-fastlane:abc\"\n" in
  let kept, had = strip_block ~marker:"fennec-fastlane:abc" (other ^ ours) in
  had && Harness.contains kept "echo keep" && not (Harness.contains kept "fennec-fastlane:abc")

let%test "strip removes BOTH our blocks (before mark + after hook), keeping unrelated config" =
  let marker = Harness.marker in
  let installed =
    "x = 1\n"
    ^ block ~name:"fennec-fastlane-mark" ~hook_type:"before_tool" ~command:("m # " ^ marker)
    ^ "\n" ^ block ~name:"fennec-fastlane" ~hook_type:"after_tool" ~command:("h # " ^ marker)
  in
  let kept, had = strip_block ~marker installed in
  had && Harness.contains kept "x = 1" && not (Harness.contains kept marker)
  && not (Harness.contains kept "before_tool") && not (Harness.contains kept "after_tool")

let%test "enable_experimental flips an existing false and prepends when absent" =
  Harness.contains (enable_experimental "enable_experimental_hooks = false\n") "enable_experimental_hooks = true"
  && not (Harness.contains (enable_experimental "enable_experimental_hooks = false\n") "false")
  && String.length (enable_experimental "[model]\nx = 1\n") > 0
  && Harness.contains (enable_experimental "[model]\nx = 1\n") "enable_experimental_hooks = true"
