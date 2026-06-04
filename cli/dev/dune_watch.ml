(* See dune_watch.mli for the grammar and the design rationale. *)

type outcome = Ok | Errors of int
type line = Trigger of string | Settled of outcome | Other

(* ---- the pure, total line classifier (the only dune-format knowledge) ---- *)

let find_sub (hay : string) (needle : string) : int option =
  let lh = String.length hay and ln = String.length needle in
  if ln = 0 then Some 0
  else
    let rec go i = if i + ln > lh then None else if String.sub hay i ln = needle then Some i else go (i + 1) in
    go 0

let contains hay needle = find_sub hay needle <> None

(* the first run of digits at or after [from] *)
let int_from (s : string) (from : int) : int option =
  let n = String.length s in
  let i = ref from in
  while !i < n && not (s.[!i] >= '0' && s.[!i] <= '9') do incr i done;
  let j = ref !i in
  while !j < n && s.[!j] >= '0' && s.[!j] <= '9' do incr j done;
  if !j > !i then int_of_string_opt (String.sub s !i (!j - !i)) else None

(* the text between the first '(' and the matching last ')' *)
let parenthetical (s : string) : string =
  match (String.index_opt s '(', String.rindex_opt s ')') with
  | Some i, Some j when j > i + 1 -> String.sub s (i + 1) (j - i - 1)
  | _ -> ""

(* strip ANSI CSI escapes (e.g. colour). dune's piped stderr is normally plain, but
   CLICOLOR_FORCE / some CI wrappers force colour even on a pipe — and a leading "\027[31m"
   carries digits ("31") that would otherwise corrupt the error-count scan below. *)
let strip_ansi (s : string) : string =
  if not (String.contains s '\027') then s
  else begin
    let b = Buffer.create (String.length s) and n = String.length s and i = ref 0 in
    while !i < n do
      if s.[!i] = '\027' && !i + 1 < n && s.[!i + 1] = '[' then begin
        i := !i + 2;
        while !i < n && not (s.[!i] >= '@' && s.[!i] <= '~') do incr i done; (* CSI params … *)
        if !i < n then incr i (* … then the final byte *)
      end
      else (Buffer.add_char b s.[!i]; incr i)
    done;
    Buffer.contents b
  end

let classify_line (raw : string) : line =
  let line = strip_ansi raw in
  if contains line "waiting for filesystem changes" then
    (* "Success, waiting…" vs "Had <n> error(s), waiting…". Anchor the count AFTER the "Had "
       token so a stray digit elsewhere (a path, a timestamp) can't be read as the error count. *)
    if contains line "error" then
      let n = match find_sub line "Had " with Some i -> int_from line (i + 4) | None -> int_from line 0 in
      Settled (Errors (match n with Some n -> n | None -> 1))
    else Settled Ok
  else if contains line "NEW BUILD" && String.contains line '(' then Trigger (parenthetical line)
  else Other

(* ---- the IO layer: spawn dune, read its stderr, assemble events ---- *)

type event =
  | Settled_build of { outcome : outcome; triggers : string list; messages : string; duration_ms : float option }
  | Exited

type t = {
  pid : int;
  fd : Unix.file_descr; (* read end of dune's stderr *)
  carry : Buffer.t; (* partial trailing line between reads *)
  mutable triggers : string list; (* reversed, since the last settle *)
  msg : Buffer.t; (* accumulated diagnostics since the last settle *)
  mutable started : float option; (* wall-clock of the first trigger in this cycle *)
  mutable building : bool; (* a build has started ("NEW BUILD") and not yet settled *)
  queue : event Queue.t; (* parsed-but-not-yet-polled events *)
  mutable eof : bool;
}

let pid t = t.pid
let is_building t = t.building

let start (targets : string list) : t =
  let rd, wr = Unix.pipe ~cloexec:false () in
  let args = Array.of_list ("dune" :: "build" :: "--watch" :: targets) in
  (* dune's status goes to stderr; capture it, leave stdout inherited *)
  let pid = Unix.create_process "dune" args Unix.stdin Unix.stdout wr in
  Unix.close wr;
  { pid; fd = rd; carry = Buffer.create 1024; triggers = []; msg = Buffer.create 1024; started = None; building = false; queue = Queue.create (); eof = false }

let now () = Unix.gettimeofday ()

(* feed one complete (newline-stripped) line into the assembler *)
let feed_line t (raw : string) =
  let line = if String.length raw > 0 && raw.[String.length raw - 1] = '\r' then String.sub raw 0 (String.length raw - 1) else raw in
  match classify_line line with
  | Trigger desc ->
    if t.started = None then t.started <- Some (now ());
    t.building <- true;
    t.triggers <- desc :: t.triggers
  | Settled outcome ->
    t.building <- false;
    let duration_ms = match t.started with Some s -> Some ((now () -. s) *. 1000.) | None -> None in
    Queue.push
      (Settled_build { outcome; triggers = List.rev t.triggers; messages = Buffer.contents t.msg; duration_ms })
      t.queue;
    t.triggers <- [];
    Buffer.clear t.msg;
    t.started <- None
  | Other ->
    (* keep real diagnostics, drop blank lines and the asterisk banner remnants. Cap the buffer
       so a pathological non-settling error stream can't grow it without bound. *)
    let trimmed = String.trim line in
    if trimmed <> "" && (not (contains trimmed "**********")) && Buffer.length t.msg < 65536 then (
      Buffer.add_string t.msg line;
      Buffer.add_char t.msg '\n')

(* drain complete lines out of [carry] *)
let drain_lines t =
  let s = Buffer.contents t.carry in
  Buffer.clear t.carry;
  let n = String.length s in
  let start = ref 0 in
  for i = 0 to n - 1 do
    if s.[i] = '\n' then (
      feed_line t (String.sub s !start (i - !start));
      start := i + 1)
  done;
  if !start < n then Buffer.add_string t.carry (String.sub s !start (n - !start))

(* build a watcher over an arbitrary fd WITHOUT spawning dune — test seam only (see .mli) *)
let of_fd (fd : Unix.file_descr) : t =
  { pid = 0; fd; carry = Buffer.create 1024; triggers = []; msg = Buffer.create 1024; started = None; building = false; queue = Queue.create (); eof = false }

let read_once t =
  let buf = Bytes.create 8192 in
  match Unix.read t.fd buf 0 (Bytes.length buf) with
  | 0 ->
    (* dune closed the pipe. If its final write was a complete line WITHOUT a trailing newline
       (a process dying mid-flush), it's sitting in [carry] — flush it so that last settle isn't
       silently lost and reported as a bare [Exited]. *)
    if Buffer.length t.carry > 0 then (feed_line t (Buffer.contents t.carry); Buffer.clear t.carry);
    t.eof <- true
  | k ->
    Buffer.add_subbytes t.carry buf 0 k;
    drain_lines t
  | exception _ -> t.eof <- true

let poll t ~timeout : event option =
  if not (Queue.is_empty t.queue) then Some (Queue.pop t.queue)
  else if t.eof then Some Exited
  else
    match (try Unix.select [ t.fd ] [] [] timeout with _ -> ([], [], [])) with
    | [], _, _ -> None
    | _ ->
      read_once t;
      if not (Queue.is_empty t.queue) then Some (Queue.pop t.queue) else if t.eof then Some Exited else None
