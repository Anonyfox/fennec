type t = { dir : string; events : string; mutable next_id : int }

let dir t = t.dir
let events_path ~dir = Filename.concat dir "events.jsonl"
let status_path ~dir = Filename.concat dir "status"
let marker_dir ~dir = Filename.concat dir "markers"

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
  let status = status_path ~dir in
  write_file events "";
  write_file status
    (Printf.sprintf "pid=%d\nroot=%s\nevents=%s\nstarted_at=%.0f\n%s" (Unix.getpid ()) root events (Unix.gettimeofday ())
       (match port with None -> "" | Some p -> Printf.sprintf "port=%d\n" p));
  { dir; events; next_id = 1 }

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

let emit_verdict t verdict =
  let kind, summary, trigger, ms, fields = Verdict.agent_event verdict in
  let trigger = match trigger with [] -> None | xs -> Some xs in
  match ms with
  | None -> emit t ~kind ~summary ?trigger ~fields ()
  | Some ms -> emit t ~kind ~summary ?trigger ~ms:(Some ms) ~fields ()

let find_string_field line name =
  let needle = "\"" ^ name ^ "\":\"" in
  match Dune_watch.find_sub line needle with
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
  match Dune_watch.find_sub line needle with
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
                match Dune_watch.find_sub s "\n" with
                | Some i ->
                  let line = String.sub s 0 i in
                  let id = Option.value (event_id line) ~default:(after + 1) in
                  if id > after then Ok (id, summarize_event line) else loop ()
                | None -> loop ())
        in
        loop ()))

let hook_json ~dir ~timeout ~event ~input =
  let after = match explicit_after input with Some id -> Some id | None -> marked_after ~dir ~input in
  let summary =
    match wait_next ?after ~dir ~timeout () with
    | Ok (_id, s) -> "Fennec dev feedback after this tool:\n" ^ s ^ "\n"
    | Error msg -> msg ^ "\n"
  in
  "{\"hookSpecificOutput\":{\"hookEventName\":" ^ json_escape event ^ ",\"additionalContext\":" ^ json_escape summary ^ "}}"

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
      let json = hook_json ~dir ~timeout:0.1 ~event:"PostToolUse" ~input in
      chk "hook includes post-edit event" (contains_ json "post-edit reload"))

let%test_unit "hook timeout is advisory JSON, not a crash" =
  let chk = Fennec_hunt_unit.check in
  with_temp_agent (fun dir ->
      let _t = start ~dir ~root:"/tmp/fennec-agent-test" () in
      let json = hook_json ~dir ~timeout:0.01 ~event:"PostToolUse" ~input:"{}" in
      chk "hook output shape" (contains_ json "hookSpecificOutput");
      chk "timeout message visible" (contains_ json "no dev event"))

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
