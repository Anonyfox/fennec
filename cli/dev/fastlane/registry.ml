(* The known coding-harness adapters. Adding a harness = adding its module to {!all} — nothing else in
   the core, the CLI, or the installer changes. *)

let all : Harness.t list =
  [ Claude.adapter; Codex.adapter; Vibe.adapter; Gemini.adapter; Cline.adapter; Cursor.adapter ]

let find id = List.find_opt (fun (h : Harness.t) -> h.id = id) all

(* the harnesses actually present on this machine (config dir exists). `fennec dev --agent` auto-installs
   into THESE only, so it never writes hook configs for harnesses you don't use. *)
let installed () = List.filter (fun (h : Harness.t) -> h.installed ()) all

(* Self-registration: the harnesses whose environment we can see. A coding session sets identifying
   env vars (CLAUDECODE, CODEX_THREAD_ID, …), so running `fennec agent attach` from inside one targets
   exactly that harness; from a plain shell none match and we fall back to the harnesses PRESENT on the
   machine ({!installed}) — never the absent ones, so we don't litter configs for tools you don't use. *)
let active () = match List.filter (fun (h : Harness.t) -> h.detect ()) all with [] -> installed () | some -> some

let ids () = List.map (fun (h : Harness.t) -> h.id) all

let%test "every adapter has a distinct, lowercase id" =
  let ids = ids () in
  List.length (List.sort_uniq compare ids) = List.length ids
  && List.for_all (fun id -> id = String.lowercase_ascii id && id <> "") ids

let%test "find resolves a known id and rejects an unknown one" =
  (match find "claude" with Some h -> h.id = "claude" | None -> false) && find "nope" = None

let%test "each adapter wires its documented injection field" =
  let shape id needle =
    match find id with Some h -> Harness.contains (h.render_feedback ~event:"AfterTool" ~text:"v") needle | None -> false
  in
  shape "claude" "additionalContext" && shape "codex" "additionalContext" && shape "gemini" "additionalContext"
  && shape "vibe" "additional_context" && shape "cursor" "additional_context" && shape "cline" "contextModification"

(* Offline end-to-end install check for the three newest adapters: run each install against a throwaway
   HOME and assert it wrote the exact documented config (no harness needed). Pins the per-adapter wiring
   — events, matcher, timeout unit, command, file layout — that the kit tests don't cover by themselves. *)
let%test "gemini/cline/cursor install their documented config shapes (temp HOME)" =
  let old_home = Sys.getenv_opt "HOME" in
  let th = Filename.concat (Filename.get_temp_dir_name ()) ("fennec-adapter-" ^ string_of_int (Unix.getpid ())) in
  Fun.protect
    ~finally:(fun () ->
      (match old_home with Some h -> Unix.putenv "HOME" h | None -> ());
      ignore (Sys.command (Printf.sprintf "rm -rf %s" (Filename.quote th))))
    (fun () ->
      Unix.putenv "HOME" th;
      List.iter (fun id -> match find id with Some h -> ignore (h.install ()) | None -> ()) [ "gemini"; "cline"; "cursor" ];
      let read p = Option.value (Harness.read_file (Filename.concat th p)) ~default:"" in
      let g = read ".gemini/settings.json" in
      let cpre = read "Documents/Cline/Hooks/PreToolUse" and cpost = read "Documents/Cline/Hooks/PostToolUse" in
      let cu = read ".cursor/hooks.json" in
      let c = Harness.contains in
      (* Gemini: BeforeTool mark + AfterTool hook, ms timeout, write_file|replace matcher *)
      c g "BeforeTool" && c g "AfterTool" && c g "15000" && c g "write_file|replace"
      && c g "agent mark --quiet" && c g "agent hook --harness gemini"
      (* Cline: two exec script files (shebang) with the two commands *)
      && c cpre "#!/bin/sh" && c cpre "agent mark --quiet" && c cpost "agent hook --harness cline"
      (* Cursor: version:1 + preToolUse/postToolUse, Write matcher *)
      && c cu "\"version\": 1" && c cu "preToolUse" && c cu "postToolUse" && c cu "Write" && c cu "agent hook --harness cursor")
