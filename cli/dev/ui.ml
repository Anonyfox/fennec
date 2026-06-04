(* See ui.mli. The dev terminal: a quiet, append-only event log of what happened, plus ONE live
   region pinned at the bottom that always reflects the current outstanding problems. The problem
   region is repainted in place (cursor-up + erase) so it stays current and clears when the build
   is FIXED — not when the next event scrolls it away. On a non-interactive sink (pipe/CI) it
   degrades to plain append lines with the same information. The supervisor is the sole writer, so
   the region is never corrupted by interleaved server output. *)

module D = Diagnostics

type level = Info | Warn | Error

type t = {
  out : string -> unit;
  caps : Tty.t;
  mutable dir : string;
  mutable ready_shown : bool;
  mutable problems : D.problem list; (* current outstanding ([] = healthy) *)
  mutable raw : string; (* unparseable diagnostic text, shown verbatim if [problems] is empty *)
  mutable serving : bool; (* is a last-good server still up while broken? *)
  mutable region : int; (* lines the live region currently occupies (interactive only) *)
  mutable builds : int;
  mutable total_ms : float;
}

let stdout_writer s = print_string s; flush stdout

let create ?(out = stdout_writer) ?caps () =
  let caps = match caps with Some c -> c | None -> Tty.detect () in
  { out; caps; dir = "."; ready_shown = false; problems = []; raw = ""; serving = false; region = 0; builds = 0; total_ms = 0. }

(* ---- colour + glyph helpers ---- *)
let c t code s = Tty.sgr t.caps code s
let dim t s = c t "2" s
let green t s = c t "32" s
let cyan t s = c t "36" s
let yellow t s = c t "33" s
let red t s = c t "31" s

let count_lines s = String.fold_left (fun n ch -> if ch = '\n' then n + 1 else n) 0 s

(* trigger files → a short "first/path +N" label, with the long head trimmed from the left so the
   filename always survives *)
let fmt_trigger (caps : Tty.t) trig =
  let strip s = match Dune_watch.find_sub s " changed" with Some i -> String.sub s 0 i | None -> s in
  match trig with
  | [] -> "filesystem change"
  | first :: rest ->
    let head = strip first in
    let budget = max 16 (caps.width - 28) in
    let head = if String.length head > budget then "…" ^ String.sub head (String.length head - budget + 1) (budget - 1) else head in
    if rest = [] then head else Printf.sprintf "%s +%d" head (List.length rest)

let fmt_ms = function Some ms -> Printf.sprintf "%.0fms" ms | None -> "—"

(* ---- live-region plumbing (interactive only) ---- *)
let erase_region t =
  if t.caps.interactive && t.region > 0 then (t.out (Tty.cursor_up t.region); t.out Tty.erase_below);
  t.region <- 0

let render_region t : string =
  if t.problems = [] && t.raw = "" then ""
  else begin
    let b = Buffer.create 256 in
    let errors, warnings = D.count t.problems in
    let head =
      let parts = (if errors > 0 then [ Printf.sprintf "%d error%s" errors (if errors = 1 then "" else "s") ] else [])
        @ (if warnings > 0 then [ Printf.sprintf "%d warning%s" warnings (if warnings = 1 then "" else "s") ] else []) in
      let summary = if parts = [] then "build failed" else "build failed · " ^ String.concat " · " parts in
      summary ^ if t.serving then dim t " · last good build still serving" else ""
    in
    Buffer.add_string b (Printf.sprintf "  %s %s\n" (red t "✗") head);
    if t.problems = [] then (
      (* parser didn't recognise the format: show the raw text, dimmed, indented *)
      List.iter (fun l -> if String.trim l <> "" then Buffer.add_string b (Printf.sprintf "     %s\n" (dim t l))) (String.split_on_char '\n' t.raw))
    else
      List.iter
        (fun (p : D.problem) ->
          Buffer.add_char b '\n';
          let loc = if p.col > 0 then Printf.sprintf "%s:%d:%d" p.file p.line p.col else Printf.sprintf "%s:%d" p.file p.line in
          Buffer.add_string b (Printf.sprintf "     %s\n" (cyan t loc));
          List.iter (fun l -> Buffer.add_string b (Printf.sprintf "     %s\n" (dim t l))) p.excerpt;
          if p.message <> "" then Buffer.add_string b (Printf.sprintf "     %s\n" p.message))
        t.problems;
    Buffer.contents b
  end

let draw_region t =
  let s = render_region t in
  if s <> "" then (t.out s; if t.caps.interactive then t.region <- count_lines s)

(* print a permanent log line, keeping any live region pinned below it *)
let log t line =
  if t.caps.interactive then (erase_region t; t.out (line ^ "\n"); draw_region t) else t.out (line ^ "\n")

(* ---- event lines ---- *)
let event t glyph color trigger ms verb =
  t.builds <- t.builds + 1;
  (match ms with Some m -> t.total_ms <- t.total_ms +. m | None -> ());
  let g = color t glyph in
  let trg = fmt_trigger t.caps trigger in
  (* pad trigger so the timing column lines up *)
  let pad = max 0 (28 - Tty.visible_width trg) in
  let line = Printf.sprintf "  %s  %s%s  %s  %s" g trg (String.make pad ' ') (dim t (Printf.sprintf "%6s" (fmt_ms ms))) (dim t verb) in
  log t line

let healthy t = t.problems <- []; t.raw <- ""; erase_region t

let rebuilt t ~trigger ~ms = healthy t; event t "●" green trigger ms "reload"
let reloaded t ~trigger ~ms = healthy t; event t "↻" cyan trigger ms "reload"
let restyled t ~trigger ~ms = healthy t; event t "↻" cyan trigger ms "css"

let failed t ~raw ~trigger:_ ~serving =
  t.problems <- D.parse raw;
  t.raw <- (if t.problems = [] then String.trim raw else "");
  t.serving <- serving;
  if t.caps.interactive then (erase_region t; draw_region t) else t.out (render_region t)

(* ---- one-off notices ---- *)
let notice t level msg =
  let g = match level with Info -> dim t "·" | Warn -> yellow t "⚠" | Error -> red t "✗" in
  log t (Printf.sprintf "  %s %s" g msg)

let app t line = log t line (* relay a server line verbatim, above the region *)

(* ---- lifecycle bookends ---- *)
let start t ~dir =
  t.dir <- dir;
  t.out (Printf.sprintf "\n  %s %s\n\n" "🦊" "fennec dev")

let ready t ~urls ~ms =
  if not t.ready_shown then begin
    t.ready_shown <- true;
    (match ms with Some m -> t.builds <- t.builds + 1; t.total_ms <- t.total_ms +. m | None -> ());
    let block = Buffer.create 128 in
    List.iter (fun u -> Buffer.add_string block (Printf.sprintf "  %s  %s\n" (cyan t "➜") (Tty.hyperlink t.caps ~url:u ~text:u))) urls;
    let timing = match ms with Some m -> Printf.sprintf "ready in %.0fms" m | None -> "ready" in
    Buffer.add_string block (Printf.sprintf "     %s\n" (dim t (Printf.sprintf "%s · watching %s" timing t.dir)));
    if t.caps.interactive then (erase_region t; t.out (Buffer.contents block); draw_region t) else t.out (Buffer.contents block)
  end

let stopped t =
  erase_region t;
  let avg = if t.builds > 0 then t.total_ms /. float_of_int t.builds else 0. in
  let tail = if t.builds > 0 then Printf.sprintf " — %d build%s, %.0fms avg" t.builds (if t.builds = 1 then "" else "s") avg else "" in
  t.out (Printf.sprintf "\n  %s%s\n" (dim t "stopped") (dim t tail))
