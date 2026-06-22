(* See editor.mli. Single-line buffer + cursor + a most-recent-first history with the usual emacs-ish
   bindings. Pure state; the caller renders and does I/O. *)

type t = {
  mutable buf : string;
  mutable cursor : int; (* byte index, 0..len *)
  mutable history : string list; (* most recent first *)
  mutable hist_idx : int; (* -1 = editing the live buffer; >=0 = browsing history.(hist_idx) *)
  mutable saved : string; (* the live buffer, stashed while browsing history *)
}

type outcome =
  | Edited
  | Submit of string
  | Interrupt
  | Eof
  | Complete of string
  | Clear
  | Nothing

let create () = { buf = ""; cursor = 0; history = []; hist_idx = -1; saved = "" }
let buffer t = t.buf
let cursor t = t.cursor
let len t = String.length t.buf
let set t s = t.buf <- s; t.cursor <- String.length s

let insert t s =
  t.buf <- String.sub t.buf 0 t.cursor ^ s ^ String.sub t.buf t.cursor (len t - t.cursor);
  t.cursor <- t.cursor + String.length s

let delete_range t lo hi =
  (* drop [lo,hi) and clamp the cursor into the shortened buffer *)
  let lo = max 0 lo and hi = min (len t) hi in
  if lo < hi then begin
    t.buf <- String.sub t.buf 0 lo ^ String.sub t.buf hi (len t - hi);
    if t.cursor > lo then t.cursor <- max lo (t.cursor - (hi - lo))
  end

let is_word_char c = (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || (c >= '0' && c <= '9') || c = '_' || c = '\'' || c = '.'

(* the start index of the identifier ending at the cursor (for Ctrl-W and completion) *)
let word_start t =
  let i = ref t.cursor in
  while !i > 0 && is_word_char t.buf.[!i - 1] do decr i done;
  !i

let word_before_cursor t = String.sub t.buf (word_start t) (t.cursor - word_start t)

(* the start of the LAST dotted segment ending at the cursor (after the last '.' within the word) *)
let leaf_start t =
  let ws = word_start t in
  let i = ref t.cursor in
  while !i > ws && t.buf.[!i - 1] <> '.' do decr i done;
  !i

let replace_leaf t candidate =
  let ls = leaf_start t in
  t.buf <- String.sub t.buf 0 ls ^ candidate ^ String.sub t.buf t.cursor (len t - t.cursor);
  t.cursor <- ls + String.length candidate

let load_history t =
  let entry = if t.hist_idx < 0 then t.saved else List.nth t.history t.hist_idx in
  set t entry

let history_prev t =
  if t.history <> [] then begin
    if t.hist_idx < 0 then t.saved <- t.buf;
    if t.hist_idx < List.length t.history - 1 then (t.hist_idx <- t.hist_idx + 1; load_history t)
  end

let history_next t =
  if t.hist_idx >= 0 then (t.hist_idx <- t.hist_idx - 1; load_history t)

let feed t (k : Key.t) : outcome =
  match k with
  | Key.Char c -> insert t (String.make 1 c); Edited
  | Key.Enter ->
    let phrase = String.trim t.buf in
    if phrase = "" then Nothing
    else begin
      (* most-recent-first, no consecutive duplicate *)
      (match t.history with last :: _ when last = phrase -> () | _ -> t.history <- phrase :: t.history);
      t.buf <- ""; t.cursor <- 0; t.hist_idx <- -1; t.saved <- "";
      Submit phrase
    end
  | Key.Backspace -> if t.cursor > 0 then (delete_range t (t.cursor - 1) t.cursor; Edited) else Nothing
  | Key.Delete -> if t.cursor < len t then (delete_range t t.cursor (t.cursor + 1); Edited) else Nothing
  | Key.Left -> if t.cursor > 0 then (t.cursor <- t.cursor - 1; Edited) else Nothing
  | Key.Right -> if t.cursor < len t then (t.cursor <- t.cursor + 1; Edited) else Nothing
  | Key.Home | Key.Ctrl 'a' -> t.cursor <- 0; Edited
  | Key.End | Key.Ctrl 'e' -> t.cursor <- len t; Edited
  | Key.Up -> history_prev t; Edited
  | Key.Down -> history_next t; Edited
  | Key.Ctrl 'k' -> delete_range t t.cursor (len t); Edited
  | Key.Ctrl 'u' -> delete_range t 0 t.cursor; Edited
  | Key.Ctrl 'w' -> let s = word_start t in delete_range t s t.cursor; Edited
  | Key.Ctrl 'l' -> Clear
  | Key.Ctrl 'c' -> Interrupt
  | Key.Ctrl 'd' -> if len t = 0 then Eof else if t.cursor < len t then (delete_range t t.cursor (t.cursor + 1); Edited) else Nothing
  | Key.Tab -> Complete (word_before_cursor t)
  | Key.Ctrl _ | Key.Esc | Key.Unknown -> Nothing

(* ── tests ───────────────────────────────────────────────────────────────────────────────────────── *)

let feed_str t s = List.iter (fun k -> ignore (feed t k)) (Key.parse s)

let%test "typing then Enter submits the trimmed phrase and clears the buffer" =
  let t = create () in
  feed_str t "  1 + 1  ";
  match feed t Key.Enter with Submit "1 + 1" -> buffer t = "" | _ -> false

let%test "backspace deletes before the cursor; Left/Right move it" =
  let t = create () in
  feed_str t "abc";
  ignore (feed t Key.Left);
  ignore (feed t Key.Backspace); (* removes 'b' → "ac", cursor at 1 *)
  buffer t = "ac" && cursor t = 1

let%test "Up/Down browse history and restore the live buffer" =
  let t = create () in
  feed_str t "first"; ignore (feed t Key.Enter);
  feed_str t "second"; ignore (feed t Key.Enter);
  feed_str t "draft"; (* live, unsubmitted *)
  ignore (feed t Key.Up); let a = buffer t = "second" in
  ignore (feed t Key.Up); let b = buffer t = "first" in
  ignore (feed t Key.Down); ignore (feed t Key.Down); let c = buffer t = "draft" in
  a && b && c

let%test "Ctrl-W kills the identifier before the cursor" =
  let t = create () in
  feed_str t "Pulse.find";
  (match feed t (Key.Ctrl 'w') with Edited -> () | _ -> ());
  buffer t = ""

let%test "Tab reports the identifier prefix before the cursor" =
  let t = create () in
  feed_str t "let x = Pulse.fi";
  match feed t Key.Tab with Complete "Pulse.fi" -> true | _ -> false

let%test "Ctrl-D on an empty buffer is Eof; Ctrl-C is Interrupt" =
  let t = create () in
  (match feed t (Key.Ctrl 'd') with Eof -> true | _ -> false)
  && (match feed t (Key.Ctrl 'c') with Interrupt -> true | _ -> false)
