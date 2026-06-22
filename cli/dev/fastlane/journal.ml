type t = { dir : string; events : string; errors : string; mutable next_id : int; mutable next_error_id : int }

let dir t = t.dir
let events_path ~dir = Filename.concat dir "events.jsonl"
let status_path ~dir = Filename.concat dir "status"
let marker_dir ~dir = Filename.concat dir "markers"
let cursor_path ~dir = Filename.concat dir "cursor"

(* The RUNTIME-ERROR stream lives in a SEPARATE file from [events.jsonl] on purpose: [events.jsonl] is
   the edit→verdict contract (mark/wait/hook read it), and a flood of async HTTP errors must never
   perturb that id sequence. Errors get their own id space + their own hook cursor. *)
let errors_path ~dir = Filename.concat dir "errors.jsonl"
let errors_cursor_path ~dir = Filename.concat dir "errors-cursor"

let mkdir_p dir =
  let rec go d =
    if d = "" || d = "." || d = "/" || Sys.file_exists d then ()
    else (go (Filename.dirname d); try Unix.mkdir d 0o755 with Unix.Unix_error (Unix.EEXIST, _, _) -> ())
  in
  go dir

let sanitize s =
  let b = Bytes.of_string s in
  for i = 0 to Bytes.length b - 1 do
    let c = Bytes.get b i in
    let ok =
      match c with
      | 'A' .. 'Z' | 'a' .. 'z' | '0' .. '9' | '-' | '_' | '.' -> true
      | _ -> false
    in
    if not ok then Bytes.set b i '-'
  done;
  Bytes.to_string b

let default_dir ~root =
  match Sys.getenv_opt "FENNEC_AGENT_DIR" with
  | Some d when d <> "" -> d
  | _ ->
    let state =
      match Sys.getenv_opt "XDG_STATE_HOME" with
      | Some d when d <> "" -> d
      | _ -> Filename.concat (Sys.getenv "HOME") ".local/state"
    in
    let key = Digest.to_hex (Digest.string root) in
    Filename.concat (Filename.concat (Filename.concat state "fennec") "agent") (sanitize (Filename.basename root) ^ "-" ^ key)

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

let write_file path s = Out_channel.with_open_text path (fun oc -> output_string oc s)

let append_line path s =
  let oc = open_out_gen [ Open_creat; Open_text; Open_append ] 0o644 path in
  Fun.protect ~finally:(fun () -> close_out_noerr oc) (fun () -> output_string oc s; output_char oc '\n'; flush oc)

let start ?dir ?port ~root () =
  let dir = match dir with Some d when d <> "" -> d | _ -> default_dir ~root in
  mkdir_p dir;
  mkdir_p (marker_dir ~dir);
  let events = events_path ~dir in
  let errors = errors_path ~dir in
  let status = status_path ~dir in
  write_file events "";
  write_file errors "";
  write_file status
    (Printf.sprintf "pid=%d\nroot=%s\nevents=%s\nerrors=%s\nstarted_at=%.0f\n%s" (Unix.getpid ()) root events errors
       (Unix.gettimeofday ()) (match port with None -> "" | Some p -> Printf.sprintf "port=%d\n" p));
  write_file (cursor_path ~dir) "0\n";
  write_file (errors_cursor_path ~dir) "0\n";
  { dir; events; errors; next_id = 1; next_error_id = 1 }

let opt_field b name = function
  | None -> ()
  | Some v -> Buffer.add_string b ("," ^ name ^ ":" ^ json_escape v)

let emit t ~kind ?summary ?trigger ?ms ?(fields = []) () =
  let id = t.next_id in
  t.next_id <- id + 1;
  let b = Buffer.create 256 in
  Buffer.add_string b (Printf.sprintf "{\"id\":%d,\"time\":%.3f,\"kind\":%s" id (Unix.gettimeofday ()) (json_escape kind));
  opt_field b "\"summary\"" summary;
  (match trigger with
  | None | Some [] -> ()
  | Some xs ->
    Buffer.add_string b ",\"trigger\":[";
    List.iteri (fun i x -> if i > 0 then Buffer.add_char b ','; Buffer.add_string b (json_escape x)) xs;
    Buffer.add_char b ']');
  (match ms with None -> () | Some None -> () | Some (Some m) -> Buffer.add_string b (Printf.sprintf ",\"ms\":%.0f" m));
  List.iter (fun (k, v) -> Buffer.add_char b ','; Buffer.add_string b (json_escape k); Buffer.add_char b ':'; Buffer.add_string b (json_escape v)) fields;
  Buffer.add_char b '}';
  append_line t.events (Buffer.contents b)

(* append one finished-but-FAILED request to the separate error stream (see {!errors_path}). Called by
   the dev supervisor for each request that {!Paw.Access.is_error}; its own id space keeps the
   edit→verdict journal untouched. Effect-isolated upstream (the caller swallows any IO failure). *)
let emit_error t (a : Paw.Access.t) =
  let id = t.next_error_id in
  t.next_error_id <- id + 1;
  let b = Buffer.create 200 in
  Buffer.add_string b
    (Printf.sprintf "{\"id\":%d,\"time\":%.3f,\"status\":%d,\"method\":%s,\"path\":%s,\"dur_us\":%d" id
       (Unix.gettimeofday ()) a.status (json_escape a.meth) (json_escape a.path) a.dur_us);
  (match a.error with Some e -> Buffer.add_string b (",\"error\":" ^ json_escape e) | None -> ());
  (match a.ip with Some ip -> Buffer.add_string b (",\"ip\":" ^ json_escape ip) | None -> ());
  (match a.req_id with Some r -> Buffer.add_string b (",\"req_id\":" ^ json_escape r) | None -> ());
  Buffer.add_char b '}';
  append_line t.errors (Buffer.contents b)

(* first index of [needle] in [hay], or None — a local copy so this lib never depends back on the
   fennec_dev supervisor it plugs into. *)
let find_sub hay needle =
  let lh = String.length hay and ln = String.length needle in
  let rec go i = if ln = 0 then Some 0 else if i + ln > lh then None else if String.sub hay i ln = needle then Some i else go (i + 1) in
  go 0

let find_string_field line name =
  let needle = "\"" ^ name ^ "\":\"" in
  match find_sub line needle with
  | None -> None
  | Some i ->
    let start = i + String.length needle in
    let rec scan j escaped =
      if j >= String.length line then None
      else
        match (line.[j], escaped) with
        | '"', false -> Some (String.sub line start (j - start))
        | '\\', false -> scan (j + 1) true
        | _ -> scan (j + 1) false
    in
    scan start false

let find_int_field line name =
  let needle = "\"" ^ name ^ "\":" in
  match find_sub line needle with
  | None -> None
  | Some i ->
    let start = i + String.length needle in
    let rec skip_ws j =
      if j < String.length line && (line.[j] = ' ' || line.[j] = '\t') then skip_ws (j + 1) else j
    in
    let start = skip_ws start in
    let rec scan j =
      if j < String.length line && line.[j] >= '0' && line.[j] <= '9' then scan (j + 1) else j
    in
    let stop = scan start in
    if stop = start then None else int_of_string_opt (String.sub line start (stop - start))

let unescape_json_string s =
  let b = Buffer.create (String.length s) in
  let rec go i =
    if i >= String.length s then ()
    else if s.[i] <> '\\' then (Buffer.add_char b s.[i]; go (i + 1))
    else if i + 1 >= String.length s then Buffer.add_char b '\\'
    else (
      (match s.[i + 1] with
      | 'n' -> Buffer.add_char b '\n'
      | 'r' -> Buffer.add_char b '\r'
      | 't' -> Buffer.add_char b '\t'
      | '"' -> Buffer.add_char b '"'
      | '\\' -> Buffer.add_char b '\\'
      | c -> Buffer.add_char b c);
      go (i + 2))
  in
  go 0;
  Buffer.contents b

(* ── project resolution: which journal does THIS edit's hook talk to? ─────────────────────────────
   The post-edit hook is ONE global, project-agnostic entry firing on every edit in every repo, so it
   must resolve the RIGHT project's journal — airtight across many projects, in parallel and over time.
   The journals are isolated by [default_dir]'s md5(root), so correct resolution IS isolation. *)

(* the fennec project root for [start]: the nearest ancestor (inclusive) holding a [dune-project], else
   [start] itself. Mirrors the dev server's own root notion, so a hook resolving from an edit's path
   lands on exactly the journal that server writes. *)
let project_root_of ~start =
  let rec go dir =
    if Sys.file_exists (Filename.concat dir "dune-project") then dir
    else
      let parent = Filename.dirname dir in
      if parent = dir then start else go parent
  in
  go start

(* first string element of a JSON array field, e.g. ["workspaceRoots":["/a","/b"]] -> "/a". Cline's
   hook payload carries no "cwd"; its project root is the first workspaceRoots entry. *)
let find_first_array_string text name =
  match find_sub text ("\"" ^ name ^ "\"") with
  | None -> None
  | Some i ->
    let n = String.length text in
    let rec open_bracket j =
      if j >= n then None
      else match text.[j] with '[' -> Some (j + 1) | ':' | ' ' | '\t' | '\n' | '\r' -> open_bracket (j + 1) | _ -> None
    in
    (match open_bracket (i + String.length name + 2) with
    | None -> None
    | Some k ->
      let rec to_quote j = if j >= n then None else match text.[j] with '"' -> Some (j + 1) | ']' -> None | _ -> to_quote (j + 1) in
      (match to_quote k with
      | None -> None
      | Some s ->
        let rec scan j esc =
          if j >= n then None
          else match (text.[j], esc) with '"', false -> Some (String.sub text s (j - s)) | '\\', false -> scan (j + 1) true | _ -> scan (j + 1) false
        in
        scan s false))

(* Resolve the journal dir for a hook/mark invocation. Priority: an explicit [override] (--dir); else
   the EDITED FILE's project (its absolute [file_path] / [filePath] from the payload — robust to wherever
   the harness spawned the hook, and correct even across projects); else the session root from the
   payload ([cwd], or Cline's [workspaceRoots][0]); else the process cwd. [mark] and [hook] both call
   this, so a pre/post pair always lands on the same journal. *)
let resolve_dir ?override ~input () =
  match override with
  | Some d when d <> "" -> d
  | _ ->
    let abs_dir_of name =
      match find_string_field input name with
      | Some f -> let f = unescape_json_string f in if Filename.is_relative f then None else Some (Filename.dirname f)
      | None -> None
    in
    let from_file = match abs_dir_of "file_path" with Some d -> Some d | None -> abs_dir_of "filePath" in
    let from_cwd =
      match find_string_field input "cwd" with
      | Some c -> Some (unescape_json_string c)
      | None -> ( match find_first_array_string input "workspaceRoots" with Some c -> Some (unescape_json_string c) | None -> None)
    in
    let start = match from_file with Some d -> d | None -> ( match from_cwd with Some c -> c | None -> Sys.getcwd ()) in
    (* canonicalise (resolve symlinks) so we agree with the dev server, whose root came from getcwd():
       e.g. an edit under /tmp/p (→ /private/tmp/p) must hash to the same journal key, not a sibling. *)
    let start = try Unix.realpath start with _ -> start in
    default_dir ~root:(project_root_of ~start)

let summarize_event line =
  match find_string_field line "summary" with
  | Some s -> unescape_json_string s
  | None -> (
    match find_string_field line "kind" with
    | Some k -> unescape_json_string k
    | None -> line)

let event_id line = find_int_field line "id"
let event_kind line = find_string_field line "kind" |> Option.map unescape_json_string

(* ── runtime error stream: read + format (the inverse of {!emit_error}) ──────────────────────────
   Read by SEPARATE short-lived CLI processes (`fennec agent errors`, `fennec agent hook`), so these
   take [~dir] rather than the writer's [t] handle. *)

(* the stored error lines with id > [after], oldest-first *)
let read_error_lines_after ~dir ~after =
  let path = errors_path ~dir in
  if not (Sys.file_exists path) then []
  else
    In_channel.with_open_text path (fun ic ->
      let rec loop acc =
        match input_line ic with
        | line -> ( match event_id line with Some id when id > after -> loop (line :: acc) | _ -> loop acc)
        | exception End_of_file -> List.rev acc
      in
      loop [])

(* one stored error line → a terse human row: "500 POST /api  1.2ms · <error>" *)
let format_error_line line =
  let status = Option.value (find_int_field line "status") ~default:0 in
  let meth = Option.value (find_string_field line "method" |> Option.map unescape_json_string) ~default:"?" in
  let path = Option.value (find_string_field line "path" |> Option.map unescape_json_string) ~default:"?" in
  let dur =
    match find_int_field line "dur_us" with
    | Some us when us < 1000 -> Printf.sprintf "%dµs" us
    | Some us -> Printf.sprintf "%.1fms" (float_of_int us /. 1000.)
    | None -> ""
  in
  let err = match find_string_field line "error" with Some e -> " · " ^ unescape_json_string e | None -> "" in
  Printf.sprintf "%d %s %s  %s%s" status meth path dur err

let errors_cursor_after ~dir =
  let path = errors_cursor_path ~dir in
  if Sys.file_exists path then
    In_channel.with_open_text path In_channel.input_all |> String.trim |> int_of_string_opt |> Option.value ~default:0
  else 0

let set_errors_cursor ~dir id = write_file (errors_cursor_path ~dir) (string_of_int id ^ "\n")

(* a human listing for `fennec agent errors` — stateless (no cursor advance); paginate with [~after] *)
let errors_report ~dir ?(after = 0) () =
  match read_error_lines_after ~dir ~after with
  | [] -> "no runtime HTTP errors recorded\n"
  | lines -> String.concat "\n" (List.map format_error_line lines) ^ "\n"

(* the note the post-tool hook appends so the agent passively sees async failures. Reads only what is
   NEW since the errors cursor, then ADVANCES the cursor (so each error is reported once), and caps the
   rows shown to keep the feedback terse. [""] when there is nothing new — existing hook output (and
   its tests) is then byte-identical. *)
let errors_note_for_hook ~dir =
  let after = errors_cursor_after ~dir in
  match read_error_lines_after ~dir ~after with
  | [] -> ""
  | lines ->
    let newest = List.fold_left (fun m l -> match event_id l with Some id -> max m id | None -> m) after lines in
    set_errors_cursor ~dir newest;
    let n = List.length lines in
    let cap = 5 in
    let shown = if n > cap then List.filteri (fun i _ -> i >= n - cap) lines else lines in
    let rows = String.concat "\n" (List.map (fun l -> "  " ^ format_error_line l) shown) in
    let more = if n > cap then Printf.sprintf "\n  (+%d more — `fennec agent errors`)" (n - cap) else "" in
    Printf.sprintf "\n%d runtime HTTP error(s) since your last check:\n%s%s\n" n rows more

let latest_event_line ~dir =
  let events = events_path ~dir in
  if not (Sys.file_exists events) then None
  else
    In_channel.with_open_text events (fun ic ->
      let rec loop last =
        match input_line ic with
        | line -> loop (Some line)
        | exception End_of_file -> last
      in
      loop None)

let latest_id ~dir = match latest_event_line ~dir with None -> None | Some line -> event_id line

let first_event_after ~dir ~after =
  let events = events_path ~dir in
  if not (Sys.file_exists events) then None
  else
    In_channel.with_open_text events (fun ic ->
      let rec loop () =
        match input_line ic with
        | line -> (
          match event_id line with
          | Some id when id > after -> Some (id, summarize_event line)
          | _ -> loop ())
        | exception End_of_file -> None
      in
      loop ())

let find_status_field text name =
  let prefix = name ^ "=" in
  String.split_on_char '\n' text
  |> List.find_map (fun line ->
         if String.length line >= String.length prefix && String.sub line 0 (String.length prefix) = prefix
         then Some (String.sub line (String.length prefix) (String.length line - String.length prefix))
         else None)

let pid_alive pid =
  try Unix.kill pid 0; true with Unix.Unix_error (Unix.ESRCH, _, _) -> false | _ -> true

let liveness ~dir =
  let status = status_path ~dir in
  if not (Sys.file_exists status) then `Unknown
  else
    let text = In_channel.with_open_text status In_channel.input_all in
    match (match find_status_field text "pid" with None -> None | Some s -> int_of_string_opt s) with
    | None -> `Unknown
    | Some pid -> if pid_alive pid then `Alive else `Dead pid

let dead_message pid =
  Printf.sprintf "fennec dev is not running (recorded pid %d is dead). Restart with `fennec dev --agent`." pid

(* SESSION-level key (not session+tool): a pre-tool mark and the post-tool hook for the SAME edit must
   land on the same key so the hook reads exactly this edit's pre-state. The two events carry the same
   session id but not necessarily the same tool-call id (and some harnesses omit it), so keying on the
   session is what correlates them. The pre-tool hook fires immediately before the edit, so the latest
   mark for a session is precisely that edit's lower bound. Falls back to a tool id, else nothing. *)
let marker_key input =
  let pick names = List.find_map (fun name -> find_string_field input name) names in
  match pick [ "session_id"; "sessionId"; "conversation_id"; "conversationId" ] with
  | Some s -> Some (sanitize s)
  | None -> ( match pick [ "tool_use_id"; "toolUseId"; "toolUseID"; "call_id"; "callId" ] with Some t -> Some (sanitize t) | None -> None)

let marker_path ~dir key = Filename.concat (marker_dir ~dir) (key ^ ".mark")

let mark ~dir ~input =
  (* The pre-tool mark fires on EVERY edit in EVERY project (it's the always-on pre-hook). Where there
     is no dev journal — a non-fennec edit — do nothing and litter nothing. *)
  if not (Sys.file_exists (events_path ~dir)) then 0
  else begin
    mkdir_p (marker_dir ~dir);
    let id = Option.value (latest_id ~dir) ~default:0 in
    (match marker_key input with None -> () | Some key -> write_file (marker_path ~dir key) (string_of_int id ^ "\n"));
    id
  end

let marked_after ~dir ~input =
  match marker_key input with
  | None -> None
  | Some key ->
    let path = marker_path ~dir key in
    if not (Sys.file_exists path) then None
    else In_channel.with_open_text path In_channel.input_all |> String.trim |> int_of_string_opt

let explicit_after input =
  List.find_map (fun name -> find_int_field input name)
    [ "fennec_agent_after_id"; "fennecAgentAfterId"; "after_id"; "afterId" ]

let cursor_after ~dir =
  let path = cursor_path ~dir in
  if Sys.file_exists path then
    In_channel.with_open_text path In_channel.input_all |> String.trim |> int_of_string_opt |> Option.value ~default:0
  else 0

let set_cursor ~dir id = write_file (cursor_path ~dir) (string_of_int id ^ "\n")

let wait_next ?after ~dir ~timeout () =
  let events = events_path ~dir in
  let deadline = Unix.gettimeofday () +. timeout in
  let rec wait_for_journal () =
    if Sys.file_exists events then Ok ()
    else if Unix.gettimeofday () >= deadline then
      Error (Printf.sprintf "fennec agent: no event journal at %s; start `fennec dev --agent` first" events)
    else (Unix.sleepf 0.05; wait_for_journal ())
  in
  match wait_for_journal () with
  | Error _ as e -> e
  | Ok () ->
    let after = match after with Some id -> id | None -> Option.value (latest_id ~dir) ~default:0 in
    match first_event_after ~dir ~after with
    | Some found -> Ok found
    | None -> (
    match liveness ~dir with
    | `Dead pid -> Error (dead_message pid)
    | `Alive | `Unknown ->
    let start = (Unix.stat events).Unix.st_size in
    let rd, wr = Unix.pipe () in
    let pid = Unix.create_process "tail" [| "tail"; "-c"; Printf.sprintf "+%d" (start + 1); "-f"; events |] Unix.stdin wr Unix.stderr in
    Unix.close wr;
    Fun.protect
      ~finally:(fun () ->
        (try Unix.kill pid Sys.sigterm with _ -> ());
        (try ignore (Unix.waitpid [] pid) with _ -> ());
        (try Unix.close rd with _ -> ()))
      (fun () ->
        let buf = Bytes.create 4096 in
        let pending = Buffer.create 256 in
        let rec loop () =
          let now = Unix.gettimeofday () in
          if now >= deadline then Error (Printf.sprintf "fennec agent: no dev event within %.0fs" timeout)
          else
            let wait = min 0.25 (deadline -. now) in
            let readable, _, _ = Unix.select [ rd ] [] [] wait in
            if readable = [] then
              match liveness ~dir with
              | `Dead pid -> Error (dead_message pid)
              | `Alive | `Unknown -> loop ()
            else
              let n = Unix.read rd buf 0 (Bytes.length buf) in
              if n = 0 then Error "fennec agent: event stream ended"
              else (
                Buffer.add_subbytes pending buf 0 n;
                let s = Buffer.contents pending in
                match find_sub s "\n" with
                | Some i ->
                  let line = String.sub s 0 i in
                  let id = Option.value (event_id line) ~default:(after + 1) in
                  if id > after then Ok (id, summarize_event line) else loop ()
                | None -> loop ())
        in
        loop ()))

(* MARKER kinds: progress signals, not settled results. "ready" is the startup line; "building" is the
   watcher having started a rebuild. Skipped when scanning for a verdict, but their presence means the
   watcher reacted — which gates how long the hook waits (see {!wait_settle}). *)
let is_marker_kind = function Some "ready" | Some "building" -> true | _ -> false

let inert_grace = 2.5

(* The post-edit hook's wait. Returns the next SETTLE verdict with id > [after], or None. None means
   one of two things, both of which the hook treats as "say nothing":
     - INERT edit: no marker and no settle within [grace] — the watcher rebuilds nothing for this
       change, so blocking the full [timeout] would be pure latency on every doc/test/foreign edit;
     - a settle that outran [timeout] (rare — a build slower than the whole budget).
   A "building" marker (the watcher started) flips the bound from [grace] to the full [timeout], so a
   genuinely slow build's verdict still lands on THIS edit. The cursor only advances on a real settle,
   so a missed slow build is re-offered to the next hook rather than lost. *)
let wait_settle ~after ~dir ~grace ~timeout () =
  let events = events_path ~dir in
  let t0 = Unix.gettimeofday () in
  let saw_marker = ref false in
  let verdict_of line =
    match event_id line with
    | Some id when id > after ->
      if is_marker_kind (event_kind line) then (saw_marker := true; None) else Some (id, summarize_event line)
    | _ -> None
  in
  (* 1) anything already on disk past [after]? (also primes [saw_marker] from an in-flight build) *)
  let scan_existing () =
    if not (Sys.file_exists events) then None
    else
      In_channel.with_open_text events (fun ic ->
        let rec loop () =
          match input_line ic with
          | line -> ( match verdict_of line with Some r -> Some r | None -> loop ())
          | exception End_of_file -> None
        in
        loop ())
  in
  match scan_existing () with
  | Some r -> Some r
  | None ->
    (* 2) tail for newly-appended lines until a settle, the bound, or the server dying *)
    let start = (try (Unix.stat events).Unix.st_size with _ -> 0) in
    let rd, wr = Unix.pipe () in
    let pid = Unix.create_process "tail" [| "tail"; "-c"; Printf.sprintf "+%d" (start + 1); "-f"; events |] Unix.stdin wr Unix.stderr in
    Unix.close wr;
    Fun.protect
      ~finally:(fun () ->
        (try Unix.kill pid Sys.sigterm with _ -> ());
        (try ignore (Unix.waitpid [] pid) with _ -> ());
        (try Unix.close rd with _ -> ()))
      (fun () ->
        let buf = Bytes.create 4096 and pending = Buffer.create 256 in
        let result = ref None and stop = ref false in
        (* pull every COMPLETE line out of [pending]; set result + stop on the first settle *)
        let drain () =
          let rec go () =
            let s = Buffer.contents pending in
            match find_sub s "\n" with
            | None -> ()
            | Some i ->
              let line = String.sub s 0 i in
              Buffer.clear pending;
              Buffer.add_string pending (String.sub s (i + 1) (String.length s - i - 1));
              (match verdict_of line with Some r -> result := Some r; stop := true | None -> go ())
          in
          go ()
        in
        while not !stop do
          let elapsed = Unix.gettimeofday () -. t0 in
          let bound = if !saw_marker then timeout else grace in
          if elapsed >= bound then stop := true
          else
            let readable, _, _ = (try Unix.select [ rd ] [] [] (min 0.2 (bound -. elapsed)) with _ -> ([], [], [])) in
            if readable = [] then (match liveness ~dir with `Dead _ -> stop := true | _ -> ())
            else
              let n = (try Unix.read rd buf 0 (Bytes.length buf) with _ -> 0) in
              if n = 0 then stop := true else (Buffer.add_subbytes pending buf 0 n; drain ())
        done;
        !result)

(* The harness-AGNOSTIC feedback an editing agent should see after one tool ran: the next settled
   verdict (or an advisory timeout / "dev not running" line), followed by any NEW runtime HTTP errors
   since the last hook (the error stream never enters the edit→verdict journal). The lower bound is an
   explicit override in the payload, else the id we marked before the tool, else our read cursor; a
   delivered verdict advances the cursor so each is reported once. A harness adapter wraps this text in
   its own injection JSON ({!Harness.render_feedback}) — the ONLY per-harness step. *)
(* Is this tool a file edit? Some harnesses (Cline) fire the post-tool hook on EVERY tool — reads,
   greps, shell — so without this we'd grace-wait on each. The JSON harnesses already scope us via a
   matcher, so their payload's tool name (if any) is an edit, and a payload with no tool name proceeds.
   Heuristic over the tool name: most edit tools contain edit/write/patch/replace/apply/create. *)
let is_edit_tool input =
  match (match find_string_field input "tool_name" with Some t -> Some t | None -> find_string_field input "toolName") with
  | None -> true
  | Some t ->
    let n = String.lowercase_ascii (unescape_json_string t) in
    let has s = find_sub n s <> None in
    has "edit" || has "write" || has "patch" || has "replac" || has "apply" || has "new_file" || has "create" || has "modify"

let feedback_text ~dir ~timeout ~input =
  (* The always-on hook fires on EVERY edit in EVERY project; it must be SILENT unless a dev server is
     actually live for this one. A non-file-edit tool (Cline fires us on reads/greps too) → "". Then
     [`Unknown] (no journal here — a non-fennec edit) and [`Dead] (the server was stopped/killed — the
     functional "detach") both return "" so the harness injects nothing. Only [`Alive] + a file edit
     waits for + reports a verdict. *)
  if not (is_edit_tool input) then ""
  else
  match liveness ~dir with
  | `Unknown | `Dead _ -> ""
  | `Alive ->
    let after =
      match explicit_after input with
      | Some id -> id
      | None -> ( match marked_after ~dir ~input with Some id -> id | None -> cursor_after ~dir)
    in
    let feedback =
      match wait_settle ~after ~dir ~grace:(Float.min inert_grace timeout) ~timeout () with
      | Some (id, s) -> set_cursor ~dir id; "Fennec dev feedback after this tool:\n" ^ s ^ "\n"
      (* None = an inert edit (nothing the watcher rebuilds), or a build slower than [timeout]; the
         cursor stays put, so the next hook picks the verdict up. Stay quiet rather than nag. *)
      | None -> ""
    in
    feedback ^ errors_note_for_hook ~dir

let status ~dir =
  let status = status_path ~dir in
  let events = events_path ~dir in
  let base =
    if Sys.file_exists status then In_channel.with_open_text status In_channel.input_all
    else "not attached\nevents=" ^ events ^ "\n"
  in
  let alive =
    match liveness ~dir with
    | `Alive -> "alive=true\n"
    | `Dead pid -> Printf.sprintf "alive=false\nrestart=fennec dev --agent\nstale_pid=%d\n" pid
    | `Unknown -> "alive=unknown\n"
  in
  let latest =
    latest_event_line ~dir
    |> Option.map (fun line ->
           let id = Option.value (event_id line) ~default:0 in
           Printf.sprintf "latest_id=%d\nlatest=%s\n" id (summarize_event line))
  in
  base ^ alive ^ Option.value latest ~default:""

(* inline tests *)
let%test "json_escape quotes control characters" =
  json_escape "a\"b\\c\n" = "\"a\\\"b\\\\c\\n\""

let%test "summary parser reads escaped summary" =
  summarize_event "{\"kind\":\"reload\",\"summary\":\"file \\\"x\\\" reloaded\"}" = "file \"x\" reloaded"

let%test "find_int_field reads numeric fields" =
  find_int_field "{\"id\":42,\"kind\":\"reload\"}" "id" = Some 42

let contains_ hay needle =
  let lh = String.length hay and ln = String.length needle in
  let rec go i = i + ln <= lh && (String.sub hay i ln = needle || go (i + 1)) in
  ln = 0 || go 0

let with_temp_agent f =
  let dir = Filename.concat (Filename.get_temp_dir_name ()) ("fennec-agent-test-" ^ string_of_int (Unix.getpid ()) ^ "-" ^ string_of_float (Unix.gettimeofday ())) in
  Fun.protect
    ~finally:(fun () -> ignore (Sys.command (Printf.sprintf "rm -rf %s" (Filename.quote dir))))
    (fun () -> f dir)

let%test_unit "wait_after ignores stale events already in the journal" =
  let chk = Fennec_hunt_unit.check in
  with_temp_agent (fun dir ->
      let t = start ~dir ~root:"/tmp/fennec-agent-test" () in
      emit t ~kind:"ready" ~summary:"ready" ();
      emit t ~kind:"reload" ~summary:"fresh reload" ();
      match wait_next ~after:1 ~dir ~timeout:0.1 () with
      | Ok (2, "fresh reload") -> ()
      | Ok (_id, s) -> chk ("unexpected event: " ^ s) false
      | Error msg -> chk ("unexpected error: " ^ msg) false)

let%test_unit "mark plus hook catches an event that settled before post hook starts" =
  let chk = Fennec_hunt_unit.check in
  with_temp_agent (fun dir ->
      let t = start ~dir ~root:"/tmp/fennec-agent-test" () in
      emit t ~kind:"ready" ~summary:"ready" ();
      let input = {|{"session_id":"s1","tool_use_id":"t1","hook_event_name":"PostToolUse"}|} in
      chk "mark snapshots id 1" (mark ~dir ~input = 1);
      emit t ~kind:"reload" ~summary:"post-edit reload" ();
      let text = feedback_text ~dir ~timeout:0.1 ~input in
      chk "feedback includes the post-edit verdict" (contains_ text "post-edit reload"))

let%test_unit "plain post-tool hook catches already-settled feedback without mark" =
  let chk = Fennec_hunt_unit.check in
  with_temp_agent (fun dir ->
      let t = start ~dir ~root:"/tmp/fennec-agent-test" () in
      emit t ~kind:"ready" ~summary:"ready" ();
      emit t ~kind:"reload" ~summary:"settled before hook ran" ();
      let text = feedback_text ~dir ~timeout:0.1 ~input:"{}" in
      chk "feedback skips ready and returns the settled verdict" (contains_ text "settled before hook ran");
      emit t ~kind:"idle" ~summary:"second feedback" ();
      let text = feedback_text ~dir ~timeout:0.1 ~input:"{}" in
      chk "the read cursor advances" (contains_ text "second feedback");
      chk "a delivered verdict is not replayed" (not (contains_ text "settled before hook ran")))

(* the always-on hook must be INVISIBLE unless there is a live server with a verdict to give. *)
let%test_unit "feedback_text is silent on an inert edit (alive, no verdict within grace)" =
  let chk = Fennec_hunt_unit.check in
  with_temp_agent (fun dir ->
      let _t = start ~dir ~root:"/tmp/fennec-agent-test" () in (* status pid = us ⇒ Alive *)
      let text = feedback_text ~dir ~timeout:0.05 ~input:"{}" in
      chk "no verdict ⇒ empty (no advisory, no crash)" (text = ""))

let%test_unit "feedback_text is silent when the dev server is dead (functional detach on kill)" =
  let chk = Fennec_hunt_unit.check in
  with_temp_agent (fun dir ->
      let t = start ~dir ~root:"/tmp/fennec-agent-test" () in
      emit t ~kind:"reload" ~summary:"a verdict from before the server died" ();
      write_file (status_path ~dir) "pid=999999\nroot=/tmp/x\n"; (* a pid that does not exist ⇒ Dead *)
      let text = feedback_text ~dir ~timeout:0.1 ~input:"{}" in
      chk "dead server ⇒ no injection" (text = ""))

let%test_unit "feedback_text is silent with no journal at all (a non-fennec edit elsewhere)" =
  let chk = Fennec_hunt_unit.check in
  with_temp_agent (fun dir ->
      (* no [start] ⇒ no status file ⇒ liveness Unknown *)
      let text = feedback_text ~dir ~timeout:0.1 ~input:"{}" in
      chk "no server here ⇒ no injection" (text = ""))

let%test_unit "feedback_text skips the building marker and returns the settle past it" =
  let chk = Fennec_hunt_unit.check in
  with_temp_agent (fun dir ->
      let t = start ~dir ~root:"/tmp/fennec-agent-test" () in
      emit t ~kind:"building" ~summary:"building…" ();
      emit t ~kind:"reload" ~summary:"index.mlx changed · backend restart" ();
      let text = feedback_text ~dir ~timeout:0.2 ~input:"{}" in
      chk "building marker not injected" (not (contains_ text "building"));
      chk "the settle past it is" (contains_ text "index.mlx changed"))

(* ── project resolution (the airtight-multi-project core) ── *)
let with_temp_project f =
  let proj = Filename.concat (Filename.get_temp_dir_name ()) (Printf.sprintf "fennec-proj-%d-%f" (Unix.getpid ()) (Unix.gettimeofday ())) in
  Fun.protect
    ~finally:(fun () -> ignore (Sys.command (Printf.sprintf "rm -rf %s" (Filename.quote proj))))
    (fun () ->
      mkdir_p (Filename.concat proj "sub");
      write_file (Filename.concat proj "dune-project") "(lang dune 3.0)\n";
      f proj)

let%test_unit "resolve_dir: explicit override wins" =
  Fennec_hunt_unit.check "override used verbatim"
    (resolve_dir ~override:"/x/agent" ~input:{|{"tool_input":{"file_path":"/a/b.ml"}}|} () = "/x/agent")

let%test_unit "resolve_dir: an absolute file_path lands on THAT file's project, not the cwd" =
  let chk = Fennec_hunt_unit.check in
  with_temp_project (fun proj ->
      (* cwd points at a DIFFERENT place — the file's project must win (cross-project correctness) *)
      let input = Printf.sprintf {|{"cwd":"/somewhere/else","tool_input":{"file_path":%S}}|} (Filename.concat proj "sub/f.ml") in
      chk "resolved to the edited file's project journal" (resolve_dir ~input () = default_dir ~root:(Unix.realpath proj)))

let%test_unit "resolve_dir: no file_path falls back to the payload cwd's project" =
  let chk = Fennec_hunt_unit.check in
  with_temp_project (fun proj ->
      let input = Printf.sprintf {|{"cwd":%S}|} (Filename.concat proj "sub") in
      chk "resolved via cwd to the project root" (resolve_dir ~input () = default_dir ~root:(Unix.realpath proj)))

let%test_unit "resolve_dir: a relative file_path is ignored in favour of the cwd" =
  let chk = Fennec_hunt_unit.check in
  with_temp_project (fun proj ->
      let input = Printf.sprintf {|{"cwd":%S,"tool_input":{"file_path":"sub/rel.ml"}}|} proj in
      chk "relative path skipped, cwd used" (resolve_dir ~input () = default_dir ~root:(Unix.realpath proj)))

let%test_unit "resolve_dir: two different projects resolve to two different journals (no cross-talk)" =
  let chk = Fennec_hunt_unit.check in
  with_temp_project (fun a ->
      with_temp_project (fun b ->
          let ia = Printf.sprintf {|{"tool_input":{"file_path":%S}}|} (Filename.concat a "sub/x.ml") in
          let ib = Printf.sprintf {|{"tool_input":{"file_path":%S}}|} (Filename.concat b "sub/y.ml") in
          chk "A resolves to A" (resolve_dir ~input:ia () = default_dir ~root:(Unix.realpath a));
          chk "B resolves to B" (resolve_dir ~input:ib () = default_dir ~root:(Unix.realpath b));
          chk "A and B are distinct journals" (resolve_dir ~input:ia () <> resolve_dir ~input:ib ())))

let%test_unit "resolve_dir: Cline-style filePath and workspaceRoots[0] resolve to the project (no cwd)" =
  let chk = Fennec_hunt_unit.check in
  with_temp_project (fun proj ->
      let want = default_dir ~root:(Unix.realpath proj) in
      let by_filepath = Printf.sprintf {|{"tool_input":{"filePath":%S}}|} (Filename.concat proj "sub/f.ml") in
      chk "camelCase filePath resolves" (resolve_dir ~input:by_filepath () = want);
      let by_ws = Printf.sprintf {|{"workspaceRoots":[%S,"/other/x"]}|} (Filename.concat proj "sub") in
      chk "workspaceRoots[0] resolves" (resolve_dir ~input:by_ws () = want))

let%test_unit "is_edit_tool: edits pass, reads/shell are filtered, no tool name proceeds" =
  let chk = Fennec_hunt_unit.check in
  let tool name = is_edit_tool (Printf.sprintf {|{"tool_name":%S}|} name) in
  let tool2 name = is_edit_tool (Printf.sprintf {|{"toolName":%S}|} name) in
  List.iter (fun n -> chk (n ^ " is an edit") (tool n)) [ "write_file"; "Write"; "apply_patch"; "Edit"; "MultiEdit"; "replace" ];
  chk "Cline write_to_file (toolName) is an edit" (tool2 "write_to_file");
  List.iter (fun n -> chk (n ^ " is NOT an edit") (not (tool n))) [ "read_files"; "run_commands"; "Grep"; "Shell" ];
  chk "no tool name in payload → proceed (matcher already scoped us)" (is_edit_tool "{}")

let%test_unit "status reports latest id and liveness" =
  let chk = Fennec_hunt_unit.check in
  with_temp_agent (fun dir ->
      let t = start ~dir ~port:9123 ~root:"/tmp/fennec-agent-test" () in
      emit t ~kind:"idle" ~summary:"build ok · no served change" ();
      let s = status ~dir in
      chk "status has latest id" (contains_ s "latest_id=1");
      chk "status has latest summary" (contains_ s "build ok");
      chk "status has alive" (contains_ s "alive=true");
      chk "status has port" (contains_ s "port=9123"))

(* ── runtime error stream ── *)

let err_access ?(status = 500) ?(error = Some "boom") ?(meth = "POST") path : Paw.Access.t =
  { ts = 0.; meth; path; status; dur_us = 1500; bytes = 0; ip = Some "127.0.0.1"; req_id = None; error;
    version = "HTTP/1.1"; host = "" }

let%test_unit "emit_error round-trips through errors_report" =
  let chk = Fennec_hunt_unit.check in
  with_temp_agent (fun dir ->
      let t = start ~dir ~root:"/tmp/fennec-agent-test" () in
      emit_error t (err_access ~status:500 ~error:(Some "kaboom") "/api/login");
      let r = errors_report ~dir () in
      chk "lists the status" (contains_ r "500");
      chk "lists the path" (contains_ r "/api/login");
      chk "lists the error message" (contains_ r "kaboom"))

let%test_unit "runtime errors do NOT enter the edit->verdict journal" =
  let chk = Fennec_hunt_unit.check in
  with_temp_agent (fun dir ->
      let t = start ~dir ~root:"/tmp/fennec-agent-test" () in
      emit t ~kind:"ready" ~summary:"ready" ();
      emit_error t (err_access "/boom");
      chk "events latest id is unchanged by an error" (latest_id ~dir = Some 1);
      match wait_next ~after:1 ~dir ~timeout:0.05 () with
      | Error _ -> () (* good: the error never materialised as an edit event *)
      | Ok (_, s) -> chk ("an error leaked into events.jsonl: " ^ s) false)

let%test_unit "errors_note_for_hook reports once, then advances its own cursor" =
  let chk = Fennec_hunt_unit.check in
  with_temp_agent (fun dir ->
      let t = start ~dir ~root:"/tmp/fennec-agent-test" () in
      emit_error t (err_access ~error:(Some "first") "/a");
      let note1 = errors_note_for_hook ~dir in
      chk "first note shows the error" (contains_ note1 "first");
      chk "first note counts it" (contains_ note1 "1 runtime");
      chk "second note is empty (cursor advanced)" (errors_note_for_hook ~dir = "");
      emit_error t (err_access ~error:(Some "second") "/b");
      let note3 = errors_note_for_hook ~dir in
      chk "a new error past the cursor is reported" (contains_ note3 "second");
      chk "the old error is not replayed" (not (contains_ note3 "first")))

let%test_unit "feedback_text carries the runtime-error note alongside the edit verdict" =
  let chk = Fennec_hunt_unit.check in
  with_temp_agent (fun dir ->
      let t = start ~dir ~root:"/tmp/fennec-agent-test" () in
      emit t ~kind:"ready" ~summary:"ready" ();
      emit t ~kind:"reload" ~summary:"rebuilt ok" ();
      emit_error t (err_access ~error:(Some "downstream 503") "/api");
      let text = feedback_text ~dir ~timeout:0.1 ~input:"{}" in
      chk "the edit verdict is present" (contains_ text "rebuilt ok");
      chk "the runtime error is present" (contains_ text "downstream 503"))

let%test_unit "errors_report pages past ids already seen with ~after" =
  let chk = Fennec_hunt_unit.check in
  with_temp_agent (fun dir ->
      let t = start ~dir ~root:"/tmp/fennec-agent-test" () in
      emit_error t (err_access ~error:(Some "old-one") "/a");
      emit_error t (err_access ~error:(Some "new-one") "/b");
      let r = errors_report ~dir ~after:1 () in
      chk "shows the error after id 1" (contains_ r "new-one");
      chk "hides the one at id 1" (not (contains_ r "old-one")))
