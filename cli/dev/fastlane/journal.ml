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

let first_hook_event_after ~dir ~after =
  let events = events_path ~dir in
  if not (Sys.file_exists events) then None
  else
    In_channel.with_open_text events (fun ic ->
      let rec loop () =
        match input_line ic with
        | line -> (
          match (event_id line, event_kind line) with
          | Some id, Some "ready" when id > after -> loop ()
          | Some id, _ when id > after -> Some (id, summarize_event line)
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

let marker_key input =
  let pick names = List.find_map (fun name -> find_string_field input name) names in
  let session = pick [ "session_id"; "sessionId"; "conversation_id"; "conversationId" ] in
  let tool = pick [ "tool_use_id"; "toolUseId"; "toolUseID"; "call_id"; "callId" ] in
  match (session, tool) with
  | Some s, Some t -> Some (sanitize (s ^ "-" ^ t))
  | _, Some t -> Some (sanitize t)
  | Some s, None -> Some (sanitize (s ^ "-last"))
  | None, None -> None

let marker_path ~dir key = Filename.concat (marker_dir ~dir) (key ^ ".mark")

let mark ~dir ~input =
  mkdir_p (marker_dir ~dir);
  let id = Option.value (latest_id ~dir) ~default:0 in
  (match marker_key input with
  | None -> ()
  | Some key -> write_file (marker_path ~dir key) (string_of_int id ^ "\n"));
  id

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

let wait_hook_next ~after ~dir ~timeout () =
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
  | Ok () -> (
    match first_hook_event_after ~dir ~after with
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
                      if id <= after || event_kind line = Some "ready" then loop ()
                      else Ok (id, summarize_event line)
                    | None -> loop ())
            in
            loop ())))

(* The harness-AGNOSTIC feedback an editing agent should see after one tool ran: the next settled
   verdict (or an advisory timeout / "dev not running" line), followed by any NEW runtime HTTP errors
   since the last hook (the error stream never enters the edit→verdict journal). The lower bound is an
   explicit override in the payload, else the id we marked before the tool, else our read cursor; a
   delivered verdict advances the cursor so each is reported once. A harness adapter wraps this text in
   its own injection JSON ({!Harness.render_feedback}) — the ONLY per-harness step. *)
let feedback_text ~dir ~timeout ~input =
  let after =
    match explicit_after input with
    | Some id -> id
    | None -> ( match marked_after ~dir ~input with Some id -> id | None -> cursor_after ~dir)
  in
  let feedback =
    match wait_hook_next ~after ~dir ~timeout () with
    | Ok (id, s) -> set_cursor ~dir id; "Fennec dev feedback after this tool:\n" ^ s ^ "\n"
    | Error msg -> msg ^ "\n"
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

let%test_unit "feedback_text on timeout is an advisory line, not a crash" =
  let chk = Fennec_hunt_unit.check in
  with_temp_agent (fun dir ->
      let _t = start ~dir ~root:"/tmp/fennec-agent-test" () in
      let text = feedback_text ~dir ~timeout:0.01 ~input:"{}" in
      chk "timeout message visible" (contains_ text "no dev event"))

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
