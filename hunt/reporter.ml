(* The test REPORTER: one place that turns run events into terminal output, tuned to where
   it is running and airtight under concurrency.

   Two faces, chosen automatically:
   - a fancy interactive terminal (a TTY) gets colour, unicode glyphs, and a single live
     status line that updates in place as tests finish;
   - a "dumb" sink (a CI log, a pipe, a file, TERM=dumb) gets plain ASCII, no colour, and NO
     cursor control at all — every event on its own line, in finish order, so log scrapers
     and humans both read it cleanly.

   Capability detection honours the cross-ecosystem conventions: NO_COLOR, FORCE_COLOR /
   CLICOLOR_FORCE, TERM=dumb, isatty, LANG/LC_* (for unicode), and COLUMNS (for width).

   Concurrency is the tricky part: with jobs > 1, several fibers finish at unpredictable
   times. EVERY write goes through one mutex and is assembled as a single atomic chunk
   (erase the status line, print the permanent content, redraw the status line), so two
   tests can never interleave a half-line, and the in-place status never corrupts the
   scrollback. Cursor control is emitted ONLY to a real TTY — never into a pipe or CI log. *)

type outcome = Passed | Failed_assert | Errored | Timed_out
type result = { name : string; outcome : outcome; ms : float; failure : Failure.t option }
type summary = { results : result list; passed : int; failed : int }

let label = function Passed -> "ok" | Failed_assert -> "FAIL" | Errored -> "ERROR" | Timed_out -> "TIMEOUT"

(* -------------------------------------------------------------- capabilities ------- *)
type caps = { color : bool; unicode : bool; status : bool; width : int }

let getenv k = match Sys.getenv_opt k with Some "" | None -> None | s -> s
let truthy = function Some v -> v <> "0" && String.lowercase_ascii v <> "false" | None -> false
let lc_contains hay needle =
  let hay = String.lowercase_ascii hay and n = String.length needle in
  let hl = String.length hay in
  let rec go i = i + n <= hl && (String.sub hay i n = needle || go (i + 1)) in
  n = 0 || go 0

let detect_caps () : caps =
  let isatty = try Unix.isatty Unix.stdout with _ -> false in
  let term = Option.value ~default:"" (getenv "TERM") in
  let dumb = term = "dumb" in
  let color =
    if getenv "NO_COLOR" <> None then false
    else if truthy (getenv "FORCE_COLOR") || truthy (getenv "CLICOLOR_FORCE") then true
    else isatty && not dumb && term <> ""
  in
  let unicode =
    if truthy (getenv "FENNEC_HUNT_ASCII") then false
    else if dumb then false
    else
      let loc =
        Option.value ~default:"" (getenv "LC_ALL")
        ^ Option.value ~default:"" (getenv "LC_CTYPE")
        ^ Option.value ~default:"" (getenv "LANG")
      in
      lc_contains loc "utf"
  in
  let status = isatty && not dumb in
  let width =
    match Option.bind (getenv "COLUMNS") int_of_string_opt with
    | Some n when n >= 40 -> min n 200
    | _ -> 80
  in
  { color; unicode; status; width }

type style = Auto | Plain | Pretty

(* -------------------------------------------------------------- the reporter ------- *)
type t = {
  caps : caps;
  emit : string -> unit;      (* writes a chunk verbatim; MUST NOT yield (so output is atomic) *)
  mutex : Mutex.t;
  mutable total : int;
  mutable done_ : int;
  mutable passed : int;
  mutable failed : int;
  mutable in_flight : string list; (* names currently running, in start order *)
  mutable status_drawn : bool;     (* an un-terminated status line is on screen *)
  mutable running : bool;          (* between run_started and run_finished *)
  mutable spin : int;
  mutable t0 : float;
}

let create ?(style = Auto) ?caps ?emit () : t =
  let caps = match caps with Some c -> c | None -> detect_caps () in
  let caps =
    match style with
    | Plain -> { caps with color = false; unicode = false; status = false }
    | Pretty -> { caps with color = true; unicode = true }
    | Auto -> caps
  in
  let emit = match emit with Some e -> e | None -> fun s -> print_string s; flush stdout in
  { caps; emit; mutex = Mutex.create (); total = 0; done_ = 0; passed = 0; failed = 0;
    in_flight = []; status_drawn = false; running = false; spin = 0; t0 = 0.0 }

(* ---- small text helpers ---- *)
let sgr on code s = if on then "\027[" ^ code ^ "m" ^ s ^ "\027[0m" else s
let truncate_vis w s = if String.length s <= w then s else String.sub s 0 (max 0 (w - 1)) ^ "."

let spinner t =
  (* a unicode braille spinner where the terminal can show it, plain ASCII otherwise *)
  let frames =
    if t.caps.unicode then
      [| "\xe2\xa0\x8b"; "\xe2\xa0\x99"; "\xe2\xa0\xb9"; "\xe2\xa0\xb8"; "\xe2\xa0\xbc";
         "\xe2\xa0\xb4"; "\xe2\xa0\xa6"; "\xe2\xa0\xa7"; "\xe2\xa0\x87"; "\xe2\xa0\x8f" |]
    else [| "|"; "/"; "-"; "\\" |]
  in
  frames.(t.spin mod Array.length frames)

let status_text t =
  let counts = Printf.sprintf "%d/%d  %d ok  %d failed" t.done_ t.total t.passed t.failed in
  let running = match t.in_flight with [] -> "" | xs -> "  ::  " ^ String.concat ", " xs in
  let s = Printf.sprintf "%s  %s%s" (spinner t) counts running in
  sgr t.caps.color "2" (truncate_vis (t.caps.width - 1) s)

(* The single serialization point. Under the lock: erase the live status line (if any),
   write [perm] (the permanent content, already newline-terminated), then redraw the status
   line if we are mid-run on a TTY. Assembled into ONE string and emitted once, so nothing
   can interleave. *)
let commit t (perm : Buffer.t -> unit) =
  Mutex.lock t.mutex;
  Fun.protect ~finally:(fun () -> Mutex.unlock t.mutex) (fun () ->
      let b = Buffer.create 256 in
      if t.status_drawn then (Buffer.add_string b "\r\027[2K"; t.status_drawn <- false);
      perm b;
      if t.caps.status && t.running && t.total > 0 then begin
        t.spin <- t.spin + 1;
        Buffer.add_string b (status_text t);
        t.status_drawn <- true
      end;
      t.emit (Buffer.contents b))

(* ---- events ---- *)
let run_started t ~total ~jobs ~grep ?(note = "") () =
  t.total <- total;
  t.running <- true;
  t.t0 <- (try Unix.gettimeofday () with _ -> 0.0);
  commit t (fun b ->
      let conc = if jobs <= 1 then "1 at a time" else Printf.sprintf "up to %d at a time" jobs in
      let grep = match grep with Some g -> Printf.sprintf " [grep %S]" g | None -> "" in
      let note = if note = "" then "" else " " ^ note in
      Buffer.add_string b
        (Printf.sprintf "%s %d test(s)%s, %s%s\n" (sgr t.caps.color "1" "running") total note conc grep))

let test_started t name =
  commit t (fun _ -> t.in_flight <- t.in_flight @ [ name ])

let result_line t (r : result) =
  let ms = Printf.sprintf "(%.0f ms)" r.ms in
  if t.caps.color || t.caps.unicode then begin
    let g, code =
      match r.outcome with
      | Passed -> ((if t.caps.unicode then "\xe2\x9c\x93" else "ok"), "32")
      | Failed_assert -> ((if t.caps.unicode then "\xe2\x9c\x97" else "FAIL"), "1;31")
      | Errored -> ((if t.caps.unicode then "\xe2\x80\xbc" else "ERROR"), "1;31")
      | Timed_out -> ((if t.caps.unicode then "\xe2\x8f\xb1" else "TIME"), "1;33")
    in
    let name = truncate_vis (max 16 (t.caps.width - 18)) r.name in
    Printf.sprintf "  %s %s  %s\n" (sgr t.caps.color code g) name (sgr t.caps.color "2" ms)
  end
  else Printf.sprintf "  %-7s %s  %s\n" (label r.outcome) r.name ms

let test_finished t (r : result) =
  commit t (fun b ->
      t.done_ <- t.done_ + 1;
      (match r.outcome with Passed -> t.passed <- t.passed + 1 | _ -> t.failed <- t.failed + 1);
      t.in_flight <- List.filter (fun n -> n <> r.name) t.in_flight;
      Buffer.add_string b (result_line t r);
      match r.failure with
      | Some f when r.outcome <> Passed ->
        Buffer.add_string b (Failure.render ~color:t.caps.color f);
        Buffer.add_char b '\n' (* breathing room before the next result line *)
      | _ -> ())

let run_finished t (s : summary) =
  let elapsed = match t.t0 with 0.0 -> 0.0 | t0 -> (try Unix.gettimeofday () -. t0 with _ -> 0.0) in
  t.running <- false;
  commit t (fun b ->
      (* a compact index of failures at the very bottom — so even after a long stream you
         see what failed and how to re-run each, without scrolling back up *)
      let fails = List.filter (fun r -> r.outcome <> Passed) s.results in
      if fails <> [] then begin
        Buffer.add_char b '\n';
        Buffer.add_string b (sgr t.caps.color "1;31" (Printf.sprintf "failures (%d):" (List.length fails)));
        Buffer.add_char b '\n';
        List.iter
          (fun r ->
            let mark = if t.caps.unicode then "\xe2\x9c\x97" else "-" in
            Buffer.add_string b (Printf.sprintf "  %s %s\n" (sgr t.caps.color "31" mark) r.name);
            match r.failure with
            | Some f -> Buffer.add_string b (Printf.sprintf "      rerun: %s\n" (sgr t.caps.color "1" f.Failure.rerun))
            | None -> ())
          fails
      end;
      let ok = s.failed = 0 in
      let head = if ok then sgr t.caps.color "1;32" "PASS" else sgr t.caps.color "1;31" "FAIL" in
      let secs = if elapsed > 0.0 then Printf.sprintf " in %.1fs" elapsed else "" in
      Buffer.add_string b
        (Printf.sprintf "\n%s  %d passed, %d failed (of %d)%s\n" head s.passed s.failed (List.length s.results) secs))
