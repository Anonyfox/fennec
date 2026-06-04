(* See diagnostics.mli. Turn dune's captured diagnostic TEXT into structured problems so the UI
   can render a clean, persistent code-frame instead of dumping raw output. The OCaml/dune format
   is:

     File "frontend/apps/web/index.mlx", line 11, characters 8-16:
     11 |       <h1>{greeting}</h1>
              ^^^^^^^^
     Error: Unbound value greeting

   This is a pragmatic scanner, not a grammar: anything it can't place is preserved verbatim by
   the caller (it shows [raw] when [parse] returns []), so a format it doesn't recognise (scss,
   esbuild, a future dune) degrades to today's behaviour rather than losing information. *)

type severity = Error | Warning

type problem = {
  severity : severity;
  file : string;
  line : int;
  col : int; (* 1-based start column, 0 if unknown *)
  message : string; (* the Error:/Warning: text (may be multi-line) *)
  excerpt : string list; (* the source-frame lines dune printed (e.g. "11 | …", "   ^^^") *)
}

let strip_ansi = Dune_watch.strip_ansi (* reuse the one scanner *)

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

(* the path inside the first pair of double-quotes *)
let quoted s =
  match String.index_opt s '"' with
  | None -> None
  | Some a -> ( match String.index_from_opt s (a + 1) '"' with Some b when b > a -> Some (String.sub s (a + 1) (b - a - 1)) | _ -> None)

let find_sub = Dune_watch.find_sub

let parse (raw : string) : problem list =
  let lines = String.split_on_char '\n' (strip_ansi raw) |> Array.of_list in
  let n = Array.length lines in
  let problems = ref [] and i = ref 0 in
  while !i < n do
    let l = lines.(!i) in
    if starts_with (String.trim l) "File \"" then begin
      let file = match quoted l with Some f -> f | None -> "" in
      let line = match find_sub l "line" with Some k -> Option.value ~default:0 (int_after l (k + 4)) | None -> 0 in
      let col = match find_sub l "characters" with Some k -> Option.value ~default:0 (Option.map (( + ) 1) (int_after l (k + 10))) | None -> 0 in
      incr i;
      (* excerpt: lines up to the Error:/Warning: line *)
      let excerpt = ref [] in
      while !i < n && (let t = String.trim lines.(!i) in not (starts_with t "Error" || starts_with t "Warning") && not (starts_with t "File \"")) do
        if String.trim lines.(!i) <> "" then excerpt := lines.(!i) :: !excerpt;
        incr i
      done;
      (* message: the Error/Warning line + indented continuations, until the next File/blank-run *)
      let severity = if !i < n && starts_with (String.trim lines.(!i)) "Warning" then Warning else Error in
      let msg = Buffer.create 64 in
      if !i < n && (starts_with (String.trim lines.(!i)) "Error" || starts_with (String.trim lines.(!i)) "Warning") then begin
        Buffer.add_string msg (String.trim lines.(!i));
        incr i;
        while !i < n && String.trim lines.(!i) <> "" && not (starts_with (String.trim lines.(!i)) "File \"") do
          Buffer.add_char msg ' ';
          Buffer.add_string msg (String.trim lines.(!i));
          incr i
        done
      end;
      problems := { severity; file; line; col; message = Buffer.contents msg; excerpt = List.rev !excerpt } :: !problems
    end
    else incr i
  done;
  List.rev !problems

let count problems =
  List.fold_left (fun (e, w) p -> match p.severity with Error -> (e + 1, w) | Warning -> (e, w + 1)) (0, 0) problems
