(* See repl.mli. The session glues Editor + Console_client + a sink; [run] is the standalone terminal
   loop. The print-above-prompt rendering lives in the [Direct] sink. *)

module P = Console_protocol

type sink = {
  banner : P.banner -> unit;
  output : id:int -> text:string -> is_error:bool -> unit;
  prompt : string -> cursor:int -> unit;
  commit : unit -> unit;
  notice : string -> unit;
}

type t = {
  client : Console_client.t;
  editor : Editor.t;
  sink : sink;
  prompt_str : string;
  mutable next_id : int;
  mutable current_eval : int option; (* the in-flight eval id, for Cancel *)
}

let create ~client ~sink ?(prompt = "fennec\xc2\xbb ") () =
  { client; editor = Editor.create (); sink; prompt_str = prompt; next_id = 0; current_eval = None }

let render_prompt s =
  s.sink.prompt (s.prompt_str ^ Editor.buffer s.editor) ~cursor:(String.length s.prompt_str + Editor.cursor s.editor)

let feed_key s (key : Key.t) : [ `Ok | `Quit ] =
  match Editor.feed s.editor key with
  | Editor.Edited -> render_prompt s; `Ok
  | Editor.Nothing -> `Ok
  | Editor.Clear -> s.sink.notice "\027[2J\027[H"; render_prompt s; `Ok
  | Editor.Submit phrase ->
    s.sink.commit (); (* freeze the on-screen "prompt ^ phrase" line *)
    s.next_id <- s.next_id + 1;
    s.current_eval <- Some s.next_id;
    Console_client.send s.client (P.Eval { id = s.next_id; phrase });
    render_prompt s;
    `Ok
  | Editor.Interrupt ->
    (match s.current_eval with
     | Some id -> Console_client.send s.client (P.Cancel { id })
     | None -> Editor.set s.editor ""; render_prompt s);
    `Ok
  | Editor.Eof -> `Quit
  | Editor.Complete prefix ->
    s.next_id <- s.next_id + 1;
    Console_client.send s.client (P.Complete { id = s.next_id; prefix });
    `Ok

let feed_replies s (replies : P.reply list) =
  List.iter
    (fun (r : P.reply) ->
      match r with
      | P.Banner b -> s.sink.banner b; render_prompt s
      | P.Output { id; text; is_error } -> s.sink.output ~id ~text ~is_error; render_prompt s
      | P.Done _ -> s.current_eval <- None; render_prompt s
      | P.Completions { items; _ } -> (
        match items with
        | [] -> ()
        | [ one ] -> Editor.replace_leaf s.editor one; render_prompt s
        | many -> s.sink.notice (String.concat "   " many); render_prompt s))
    replies

(* ── the standalone terminal renderer (print-above-prompt) ───────────────────────────────────────── *)

module Direct = struct
  type state = { mutable on_prompt : bool }

  let erase st = if st.on_prompt then (print_string "\r\027[K"; st.on_prompt <- false)
  let put st s = erase st; print_string s; print_string "\r\n"; flush stdout

  let banner_line (tty : Tty.t) (b : P.banner) =
    let c = Tty.sgr tty in
    Printf.sprintf "%s  %s   %s %s   %s %s"
      (c "1;38;5;173" ("🦊 " ^ b.app ^ " console"))
      (c "2" ("v" ^ b.version))
      (c "2" "data") (c "36" b.backend)
      (c "2" "open") (c "36" (String.concat " " b.opened))

  let make tty : sink =
    let st = { on_prompt = false } in
    let c = Tty.sgr tty in
    {
      banner = (fun b -> put st (banner_line tty b));
      output =
        (fun ~id:_ ~text ~is_error ->
          String.split_on_char '\n' text
          |> List.iter (fun l -> put st (if is_error then c "31" l else l)));
      notice = (fun msg -> put st (c "2" msg));
      commit = (fun () -> if st.on_prompt then (print_string "\r\n"; st.on_prompt <- false; flush stdout));
      prompt =
        (fun line ~cursor ->
          erase st;
          print_string (c "38;5;173" line);
          print_string "\r";
          if cursor > 0 then Printf.printf "\027[%dC" cursor;
          st.on_prompt <- true;
          flush stdout);
    }
end

(* ── raw mode ────────────────────────────────────────────────────────────────────────────────────── *)

let with_raw_mode f =
  match try Unix.isatty Unix.stdin with _ -> false with
  | false -> f () (* not a TTY (piped): no interactive editing, just run the loop *)
  | true ->
    let saved = Unix.tcgetattr Unix.stdin in
    let raw =
      { saved with
        c_icanon = false; (* char-by-char *)
        c_echo = false; (* we render the line ourselves *)
        c_isig = false; (* Ctrl-C/Z arrive as bytes — the editor owns them *)
        c_icrnl = false; (* CR stays CR (=13) *)
        c_ixon = false; (* no flow-control capture of Ctrl-S/Q *)
        c_opost = false; (* no output post-processing — our \r\n are literal *)
        c_vmin = 1;
        c_vtime = 0 }
    in
    Unix.tcsetattr Unix.stdin Unix.TCSANOW raw;
    Fun.protect ~finally:(fun () -> try Unix.tcsetattr Unix.stdin Unix.TCSANOW saved with _ -> ()) f

let run ~sock_path =
  let tty = Tty.detect () in
  let client = Console_client.connect sock_path in
  Console_client.send client (P.Hello { cols = tty.width });
  let session = create ~client ~sink:(Direct.make tty) () in
  render_prompt session;
  with_raw_mode @@ fun () ->
  let cfd = Console_client.fd client in
  let buf = Bytes.create 4096 in
  let quit = ref false in
  while not !quit do
    match Unix.select [ Unix.stdin; cfd ] [] [] (-1.0) with
    | exception Unix.Unix_error (Unix.EINTR, _, _) -> ()
    | exception _ -> quit := true
    | rs, _, _ ->
      if List.mem Unix.stdin rs then (
        match Unix.read Unix.stdin buf 0 (Bytes.length buf) with
        | 0 -> quit := true
        | n ->
          List.iter
            (fun key -> match feed_key session key with `Quit -> quit := true | `Ok -> ())
            (Key.parse (Bytes.sub_string buf 0 n))
        | exception _ -> quit := true);
      if (not !quit) && List.mem cfd rs then
        match Console_client.receive client with `Closed -> quit := true | `Replies rs -> feed_replies session rs
  done;
  print_string "\r\n";
  flush stdout;
  Console_client.close client

(* ── tests (the pure session logic, with a recording sink) ───────────────────────────────────────── *)

let recording_sink log =
  { banner = (fun b -> log := Printf.sprintf "banner:%s" b.app :: !log);
    output = (fun ~id ~text ~is_error -> log := Printf.sprintf "out[%d]%s:%s" id (if is_error then "E" else "") text :: !log);
    prompt = (fun line ~cursor:_ -> log := ("prompt:" ^ line) :: !log);
    commit = (fun () -> log := "commit" :: !log);
    notice = (fun m -> log := ("notice:" ^ m) :: !log) }

let%test "a completion of one candidate is applied to the buffer's leaf" =
  (* drive the session's reply handling without a real socket: only the editor + sink are exercised *)
  let log = ref [] in
  let dummy = { client = Obj.magic (); editor = Editor.create (); sink = recording_sink log; prompt_str = "» "; next_id = 0; current_eval = None } in
  Editor.set dummy.editor "Pulse.fi";
  feed_replies dummy [ P.Completions { id = 1; items = [ "find" ] } ];
  Editor.buffer dummy.editor = "Pulse.find"

let%test "engine output is rendered and the prompt redrawn" =
  let log = ref [] in
  let dummy = { client = Obj.magic (); editor = Editor.create (); sink = recording_sink log; prompt_str = "» "; next_id = 0; current_eval = None } in
  feed_replies dummy [ P.Output { id = 2; text = "- : int = 2"; is_error = false }; P.Done { id = 2 } ];
  List.exists (fun l -> l = "out[2]:- : int = 2") !log
