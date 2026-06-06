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

(* the trigger-label column width: event lines pad the trigger to this so the timing column lines
   up, and [fmt_trigger] trims the label to fit the terminal minus this gutter *)
let trigger_col = 28

(* trigger files → a short "first/path +N" label, with the long head trimmed from the left so the
   filename always survives *)
let fmt_trigger (caps : Tty.t) trig =
  let strip s = match Dune_watch.find_sub s " changed" with Some i -> String.sub s 0 i | None -> s in
  match trig with
  | [] -> "filesystem change"
  | first :: rest ->
    let head = strip first in
    let budget = max 16 (caps.width - trigger_col) in
    let head = if String.length head > budget then "…" ^ String.sub head (String.length head - budget + 1) (budget - 1) else head in
    if rest = [] then head else Printf.sprintf "%s +%d" head (List.length rest)

let fmt_ms = function Some ms -> Printf.sprintf "%.0fms" ms | None -> "—"

(* ---- live-region plumbing (interactive only) ---- *)
let erase_region t =
  if t.caps.interactive && t.region > 0 then (t.out (Tty.cursor_up t.region); t.out Tty.erase_below);
  t.region <- 0

(* A code frame read from the source itself — for errors where dune gives no excerpt (mlx/ocaml
   SYNTAX errors). dune's line:col is a real byte offset, but it marks the parser's point of
   confusion (often past the actual typo), so we show a few lines of CONTEXT around it: the user
   sees the structure and can spot the real mistake even when the caret lands downstream. The
   error line is shown bright, its neighbours dim, with a red caret under dune's column. Returns
   pre-coloured lines; [] if the file/line can't be read. *)
let code_frame t ~file ~line ~col : string list =
  if line <= 0 then []
  else
    match (try Some (In_channel.with_open_text file In_channel.input_all) with _ -> None) with
    | None -> []
    | Some content ->
      let arr = Array.of_list (String.split_on_char '\n' content) in
      let n = Array.length arr in
      if line > n then []
      else begin
        (* favour lines BEFORE the marked line: a syntax error's real cause (an unterminated
           string, an unmatched open delimiter) is almost always above dune's confusion point, so
           more upward context makes it visible even when the caret lands downstream *)
        let lo = max 1 (line - 4) and hi = min n (line + 2) in
        let gutw = String.length (string_of_int hi) in
        let acc = ref [] in
        for ln = lo to hi do
          let src = arr.(ln - 1) in
          let g = Printf.sprintf "%*d | " gutw ln in
          if ln = line then begin
            acc := (g ^ src) :: !acc;
            if col > 0 then begin
              (* indent by the DISPLAY width of the prefix (so multibyte before the caret aligns) *)
              let prefix = String.sub src 0 (min (String.length src) (col - 1)) in
              acc := (String.make (String.length g + Tty.visible_width prefix) ' ' ^ red t "^") :: !acc
            end
          end
          else acc := dim t (g ^ src) :: !acc
        done;
        List.rev !acc
      end

let render_region t : string =
  if t.problems = [] && t.raw = "" then ""
  else begin
    let b = Buffer.create 256 in
    let errors, warnings = D.count t.problems in
    let head =
      let parts = (if errors > 0 then [ Printf.sprintf "%d error%s" errors (if errors = 1 then "" else "s") ] else [])
        @ (if warnings > 0 then [ Printf.sprintf "%d warning%s" warnings (if warnings = 1 then "" else "s") ] else []) in
      let summary = if parts = [] then "build failed" else "build failed · " ^ String.concat " · " parts in
      (* make the server state explicit: a last-good server still answers, OR (a failed first
         build) there is NO server yet — so the dev URL / livereload won't connect until it's fixed *)
      summary ^ dim t (if t.serving then " · last good build still serving" else " · server not running")
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
          (* use dune's own excerpt when it gave one (type errors — it underlines the exact span);
             otherwise read the source ourselves for a context frame (syntax errors give no
             excerpt). Both render as indented lines under the location. *)
          let frame = if p.excerpt <> [] then List.map (dim t) p.excerpt else code_frame t ~file:p.file ~line:p.line ~col:p.col in
          List.iter (fun l -> Buffer.add_string b (Printf.sprintf "     %s\n" l)) frame;
          if p.message <> "" then Buffer.add_string b (Printf.sprintf "     %s\n" p.message);
          List.iter (fun l -> Buffer.add_string b (Printf.sprintf "     %s\n" (dim t l))) p.related)
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
  let pad = max 0 (trigger_col - Tty.visible_width trg) in
  let line = Printf.sprintf "  %s  %s%s  %s  %s" g trg (String.make pad ' ') (dim t (Printf.sprintf "%6s" (fmt_ms ms))) (dim t verb) in
  log t line

let healthy t = t.problems <- []; t.raw <- ""; erase_region t

let rebuilt t ~trigger ~ms = healthy t; event t "●" green trigger ms "reload"
let reloaded t ~trigger ~ms = healthy t; event t "↻" cyan trigger ms "reload"
let restyled t ~trigger ~ms = healthy t; event t "↻" cyan trigger ms "css"

(* inline test results after a green settle — a quiet line, never gates the build *)
let tested t ~passed ~failed ~libs ~ms =
  let ms_opt = if ms > 0.0 then Some ms else None in
  if failed = 0 then
    log t (Printf.sprintf "  %s  tests %s  %s  %s"
      (green t "✓")
      (green t (string_of_int passed))
      (dim t (Printf.sprintf "%6s" (fmt_ms ms_opt)))
      (dim t (Printf.sprintf "%d lib%s" libs (if libs = 1 then "" else "s"))))
  else
    log t (Printf.sprintf "  %s  tests %s  %s  %s"
      (red t "✗")
      (red t (Printf.sprintf "%d passed, %d failed" passed failed))
      (dim t (Printf.sprintf "%6s" (fmt_ms ms_opt)))
      (dim t (Printf.sprintf "%d lib%s" libs (if libs = 1 then "" else "s"))))

let failed t ~raw ~trigger:_ ~serving =
  t.problems <- D.parse raw;
  t.raw <- (if t.problems = [] then String.trim raw else "");
  t.serving <- serving;
  if t.caps.interactive then (erase_region t; draw_region t) else t.out (render_region t)

(* a green build that changed nothing the server cares about (e.g. reverting a typo to a
   byte-identical artifact): if a problem panel was up, it's now FIXED — drop it and confirm, so
   the panel can never get stuck showing a stale error. Silent if there was nothing to clear. *)
let resolved t ~ms =
  if t.problems <> [] || t.raw <> "" then begin
    t.problems <- [];
    t.raw <- "";
    let pad = String.make (max 0 (trigger_col - 8)) ' ' (* "resolved" is 8 cols *) in
    log t (Printf.sprintf "  %s  resolved%s  %s" (green t "●") pad (dim t (Printf.sprintf "%6s" (fmt_ms ms))))
  end

(* ---- one-off notices ---- *)
let notice t level msg =
  let g = match level with Info -> dim t "·" | Warn -> yellow t "⚠" | Error -> red t "✗" in
  log t (Printf.sprintf "  %s %s" g msg)

let app t line = log t line (* relay a server line verbatim, above the region *)

(* ---- lifecycle bookends ---- *)
let start t ~dir =
  t.dir <- dir;
  t.out (Printf.sprintf "\n  %s %s\n\n" "🦊" "fennec dev")

let ready t ~urls ~gateway ~ms =
  if not t.ready_shown then begin
    t.ready_shown <- true;
    (match ms with Some m -> t.builds <- t.builds + 1; t.total_ms <- t.total_ms +. m | None -> ());
    let block = Buffer.create 128 in
    List.iter (fun (name, url) -> Buffer.add_string block (Printf.sprintf "  %s  %s %s %s\n" (cyan t "➜") name (dim t "→") (Tty.hyperlink t.caps ~url ~text:url))) urls;
    (* the host-routed gateway: prod-identical selection — reach any domain here via its Host header
       (or an /etc/hosts entry). Dim, because the per-endpoint URLs above are the everyday targets. *)
    Buffer.add_string block (Printf.sprintf "     %s %s\n" (dim t "host routing →") (Tty.hyperlink t.caps ~url:gateway ~text:(dim t gateway)));
    let timing = match ms with Some m -> Printf.sprintf "ready in %.0fms" m | None -> "ready" in
    Buffer.add_string block (Printf.sprintf "     %s\n" (dim t (Printf.sprintf "%s · watching %s" timing t.dir)));
    if t.caps.interactive then (erase_region t; t.out (Buffer.contents block); draw_region t) else t.out (Buffer.contents block)
  end

let stopped t =
  erase_region t;
  let avg = if t.builds > 0 then t.total_ms /. float_of_int t.builds else 0. in
  let tail = if t.builds > 0 then Printf.sprintf " — %d build%s, %.0fms avg" t.builds (if t.builds = 1 then "" else "s") avg else "" in
  t.out (Printf.sprintf "\n  %s%s\n" (dim t "stopped") (dim t tail))

(* ──── tests ──── *)

let contains_ hay needle =
  let lh = String.length hay and ln = String.length needle in
  let rec go i = i + ln <= lh && (String.sub hay i ln = needle || go (i + 1)) in
  ln = 0 || go 0

let sample_ =
  "File \"index.mlx\", line 11, characters 8-16:\n11 |     <h1>{greeting}</h1>\n         ^^^^^^^^\nError: Unbound value greeting\n"

let%test_unit "plain mode — content" =
  let chk = Fennec_hunt_unit.check in
  let buf = Buffer.create 512 in
  let out s = Buffer.add_string buf s in
  let take () = let s = Buffer.contents buf in Buffer.clear buf; s in
  let ui = create ~out ~caps:Tty.plain () in
  start ui ~dir:"examples/site";
  chk "banner shows the fox + name" (contains_ (take ()) "fennec dev");
  ready ui ~urls:[ ("web", "http://localhost:4001") ] ~gateway:"http://localhost:4000" ~ms:(Some 412.);
  let s = take () in
  chk "ready shows the endpoint URL" (contains_ s "http://localhost:4001");
  chk "ready shows the endpoint name" (contains_ s "web");
  chk "ready shows the gateway URL" (contains_ s "http://localhost:4000");
  chk "ready shows the host-routing label" (contains_ s "host routing");
  chk "ready shows the time" (contains_ s "ready in 412ms");
  chk "ready shows the watched dir" (contains_ s "watching examples/site");
  ready ui ~urls:[ ("web", "http://localhost:4001") ] ~gateway:"http://localhost:4000" ~ms:(Some 99.);
  chk "ready is idempotent (a second report prints nothing)" (take () = "");
  rebuilt ui ~trigger:[ "index.mlx changed" ] ~ms:(Some 38.);
  let s = take () in
  chk "rebuilt shows file, ms, effect" (contains_ s "index.mlx" && contains_ s "38ms" && contains_ s "reload");
  restyled ui ~trigger:[ "main.scss changed" ] ~ms:(Some 9.);
  chk "restyled is labelled css" (contains_ (take ()) "css");
  failed ui ~raw:sample_ ~trigger:[] ~serving:true;
  let s = take () in
  chk "failed shows the error count" (contains_ s "1 error");
  chk "failed notes the last good server" (contains_ s "last good build still serving");
  chk "failed shows the location" (contains_ s "index.mlx:11");
  chk "failed shows the message" (contains_ s "Unbound value greeting");
  resolved ui ~ms:(Some 12.);
  chk "resolved clears the panel with a confirmation line" (contains_ (take ()) "resolved");
  resolved ui ~ms:(Some 5.);
  chk "resolved is silent when nothing is outstanding" (take () = "")

let%test_unit "plain mode — code frame" =
  let chk = Fennec_hunt_unit.check in
  let buf = Buffer.create 512 in
  let out s = Buffer.add_string buf s in
  let take () = let s = Buffer.contents buf in Buffer.clear buf; s in
  let ui = create ~out ~caps:Tty.plain () in
  let tmp = Filename.temp_file "fennec_cf" ".ml" in
  (let oc = open_out tmp in output_string oc "let a = 1\nlet b = (\nlet c = 3\nlet d = 4\n"; close_out oc);
  failed ui ~raw:(Printf.sprintf "File %S, line 2, characters 8-9:\nError: Syntax error\n" tmp) ~trigger:[] ~serving:false;
  let s = take () in
  chk "code frame shows the error line read from source" (contains_ s "let b = (");
  chk "code frame includes a line of context" (contains_ s "let a = 1");
  chk "code frame draws a caret" (contains_ s "^");
  resolved ui ~ms:None;
  (try Sys.remove tmp with _ -> ());
  failed ui ~raw:sample_ ~trigger:[] ~serving:false;
  chk "with no server the panel says 'server not running'" (contains_ (take ()) "server not running");
  resolved ui ~ms:None

let%test_unit "interactive — live region" =
  let chk = Fennec_hunt_unit.check in
  let ibuf = Buffer.create 512 in
  let iout s = Buffer.add_string ibuf s in
  let itake () = let s = Buffer.contents ibuf in Buffer.clear ibuf; s in
  let icaps = { Tty.color = false; hyperlinks = false; interactive = true; width = 80 } in
  let iui = create ~out:iout ~caps:icaps () in
  failed iui ~raw:sample_ ~trigger:[] ~serving:false;
  ignore (itake ());
  rebuilt iui ~trigger:[ "index.mlx changed" ] ~ms:(Some 40.);
  let s = itake () in
  chk "fixing erases the region (cursor-up + erase-below)" (contains_ s "\027[" && contains_ s "\027[J");
  chk "fixing then shows the success line" (contains_ s "reload")
