(* See diagnostics.mli. Turn dune's captured diagnostic TEXT into structured problems so the UI
   can render a clean, persistent code-frame instead of dumping raw output. The OCaml/dune format
   is a sequence of `File "…", line N, characters A-B:` blocks. A block that carries an
   Error:/Warning: line is a PROBLEM; a block WITHOUT one is a secondary location of the previous
   problem (e.g. a syntax error's "This '[' might be unmatched" hint, which points at a second
   spot but is the SAME one error) — so it's folded into that problem's [related], not counted as
   another error.

   A pragmatic scanner, not a grammar: anything it can't place is preserved verbatim by the caller
   (it shows [raw] when [parse] returns []), so an unrecognised format (scss, esbuild, a future
   dune) degrades to today's behaviour rather than losing information. *)

type severity = Error | Warning

type problem = {
  severity : severity;
  file : string;
  line : int;
  col : int; (* 1-based start column, 0 if unknown *)
  message : string; (* the Error:/Warning: text (may be multi-line) *)
  excerpt : string list; (* the source-frame lines dune printed for THIS problem (often empty) *)
  related : string list; (* secondary locations/hints belonging to this same problem *)
}

let strip_ansi = Dune_watch.strip_ansi (* reuse the one scanner *)
let find_sub = Dune_watch.find_sub

let starts_with s pfx =
  let lp = String.length pfx in
  String.length s >= lp && String.sub s 0 lp = pfx

(* first integer at or after [from] in [s] *)
let int_after s from =
  let n = String.length s and i = ref from in
  while !i < n && not (s.[!i] >= '0' && s.[!i] <= '9') do incr i done;
  let j = ref !i in
  while !j < n && s.[!j] >= '0' && s.[!j] <= '9' do incr j done;
  if !j > !i then int_of_string_opt (String.sub s !i (!j - !i)) else None

(* split a `File "<path>", line N, characters A-B:` line into the quoted PATH and the REST that
   follows the closing quote. The location fields are then read from REST, never the whole line —
   a path can itself contain the substring "line"/"characters" (e.g. timeline2/x.ml, char1/x.ml),
   and searching the whole line would lift the location out of the PATH and point the code frame
   at the wrong spot. *)
let split_at_path s =
  match String.index_opt s '"' with
  | None -> ("", s)
  | Some a -> (
    match String.index_from_opt s (a + 1) '"' with
    | Some b when b > a -> (String.sub s (a + 1) (b - a - 1), String.sub s (b + 1) (String.length s - b - 1))
    | _ -> ("", s))

let parse (raw : string) : problem list =
  let lines = String.split_on_char '\n' (strip_ansi raw) |> Array.of_list in
  let n = Array.length lines in
  let problems = ref [] (* reversed *) and i = ref 0 in
  while !i < n do
    let l = lines.(!i) in
    if starts_with (String.trim l) "File \"" then begin
      let file, rest = split_at_path l in
      let line = match find_sub rest "line" with Some k -> Option.value ~default:0 (int_after rest (k + 4)) | None -> 0 in
      let col = match find_sub rest "characters" with Some k -> Option.value ~default:0 (Option.map (( + ) 1) (int_after rest (k + 10))) | None -> 0 in
      incr i;
      (* lines up to the next Error:/Warning:/File: *)
      let body = ref [] in
      while !i < n && (let t = String.trim lines.(!i) in (not (starts_with t "Error")) && (not (starts_with t "Warning")) && not (starts_with t "File \"")) do
        if String.trim lines.(!i) <> "" then body := lines.(!i) :: !body;
        incr i
      done;
      let body = List.rev !body in
      if !i < n && (starts_with (String.trim lines.(!i)) "Error" || starts_with (String.trim lines.(!i)) "Warning") then begin
        (* a PRIMARY problem: this block carries the Error/Warning message *)
        let severity = if starts_with (String.trim lines.(!i)) "Warning" then Warning else Error in
        let msg = Buffer.create 64 in
        Buffer.add_string msg (String.trim lines.(!i));
        incr i;
        while !i < n && String.trim lines.(!i) <> "" && not (starts_with (String.trim lines.(!i)) "File \"") do
          Buffer.add_char msg ' ';
          Buffer.add_string msg (String.trim lines.(!i));
          incr i
        done;
        problems := { severity; file; line; col; message = Buffer.contents msg; excerpt = body; related = [] } :: !problems
      end
      else begin
        (* a SECONDARY location (no Error/Warning) — fold into the previous problem *)
        let note = Printf.sprintf "%s:%d" file line :: List.map String.trim body in
        match !problems with
        | prev :: rest -> problems := { prev with related = prev.related @ note } :: rest
        | [] -> problems := { severity = Error; file; line; col; message = ""; excerpt = body; related = [] } :: !problems
      end
    end
    else incr i
  done;
  List.rev !problems

let count problems =
  List.fold_left (fun (e, w) p -> match p.severity with Error -> (e + 1, w) | Warning -> (e, w + 1)) (0, 0) problems
