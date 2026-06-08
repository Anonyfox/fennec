type t = { dir : string; events : string; mutable next_id : int }

let dir t = t.dir
let events_path ~dir = Filename.concat dir "events.jsonl"
let status_path ~dir = Filename.concat dir "status"

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

let start ?dir ~root () =
  let dir = match dir with Some d when d <> "" -> d | _ -> default_dir ~root in
  mkdir_p dir;
  let events = events_path ~dir in
  let status = status_path ~dir in
  write_file events "";
  write_file status
    (Printf.sprintf "pid=%d\nroot=%s\nevents=%s\nstarted_at=%.0f\n" (Unix.getpid ()) root events (Unix.gettimeofday ()));
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

let wait_next ~dir ~timeout =
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
            let readable, _, _ = Unix.select [ rd ] [] [] (deadline -. now) in
            if readable = [] then Error (Printf.sprintf "fennec agent: no dev event within %.0fs" timeout)
            else
              let n = Unix.read rd buf 0 (Bytes.length buf) in
              if n = 0 then Error "fennec agent: event stream ended"
              else (
                Buffer.add_subbytes pending buf 0 n;
                let s = Buffer.contents pending in
                match Dune_watch.find_sub s "\n" with
                | Some i -> Ok (summarize_event (String.sub s 0 i))
                | None -> loop ())
        in
        loop ())

let hook_json ~dir ~timeout ~event =
  let summary =
    match wait_next ~dir ~timeout with
    | Ok s -> "Fennec dev feedback after this tool:\n" ^ s ^ "\n"
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
  let latest =
    if not (Sys.file_exists events) then None
    else
      let lines = In_channel.with_open_text events (fun ic ->
        let rec loop last =
          match input_line ic with
          | line -> loop (Some line)
          | exception End_of_file -> last
        in
        loop None)
      in
      Option.map summarize_event lines
  in
  match latest with None -> base | Some s -> base ^ "latest=" ^ s ^ "\n"

(* inline tests *)
let%test "json_escape quotes control characters" =
  json_escape "a\"b\\c\n" = "\"a\\\"b\\\\c\\n\""

let%test "summary parser reads escaped summary" =
  summarize_event "{\"kind\":\"reload\",\"summary\":\"file \\\"x\\\" reloaded\"}" = "file \"x\" reloaded"
