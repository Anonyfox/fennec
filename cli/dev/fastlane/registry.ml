(* The known coding-harness adapters. Adding a harness = adding its module to {!all} — nothing else in
   the core, the CLI, or the installer changes. *)

let all : Harness.t list =
  [ Claude.adapter; Codex.adapter; Vibe.adapter; Gemini.adapter; Cline.adapter; Cursor.adapter ]

let find id = List.find_opt (fun (h : Harness.t) -> h.id = id) all

(* the harnesses actually present on this machine (config dir exists). `fennec dev --agent` auto-installs
   into THESE only, so it never writes hook configs for harnesses you don't use. *)
let installed () = List.filter (fun (h : Harness.t) -> h.installed ()) all

(* Self-registration: the harnesses whose environment we can see. A coding session sets identifying
   env vars (CLAUDECODE, CODEX_THREAD_ID, …); from a plain shell none match, so we fall back to ALL
   known harnesses — `fennec dev --attach` then prepares every harness's hook, and whichever the user
   actually drives picks it up (each hook reads its OWN config file, so extra ones are inert). *)
let active () = match List.filter (fun (h : Harness.t) -> h.detect ()) all with [] -> all | some -> some

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
