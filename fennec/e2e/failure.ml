(* A test failure as DATA, plus a pure renderer that turns it into a beautiful, actionable
   ASCII report. Kept entirely free of I/O and of the backend functor, so the rendering of
   every failure mode can be unit-tested by constructing values directly.

   The design goal: a human or LLM reading ONE failure report knows exactly which step in
   which named test failed, what the page actually looked like at that instant, why it
   matters, what to check, and how to re-run just that test — without opening the test file
   or doing another slow browser run. *)

module Cond = Backend.Cond
module Diag = Backend.Diag

(* a step in the executed pipeline; the failed (or in-flight) one is the last *)
type step_status =
  | Ok                                  (* completed *)
  | Failed of Cond.t option * Diag.t    (* an assertion/action timed out, with its diagnostic *)
  | Running                             (* in flight when the test errored / timed out *)

type step = { index : int; label : string; status : step_status; ms : float }

type kind =
  | Assertion          (* a step's condition failed (the last step is [Failed _]) *)
  | Errored of string  (* the body raised a non-assertion exception *)
  | Timed_out of float (* the test exceeded its wall-clock budget (seconds) *)

type t = {
  test : string;             (* the test name (also the --grep key) *)
  trace : step list;         (* executed steps, in order *)
  kind : kind;
  rerun : string;            (* a copy-pasteable command to re-run just this test *)
}

(* ---------------------------------------------------------------- small helpers ---- *)
let buf_add = Buffer.add_string
let line b s = Buffer.add_string b s; Buffer.add_char b '\n'
let truncate n s = if String.length s <= n then s else String.sub s 0 (max 0 (n - 1)) ^ "..."
let pad_right n s = if String.length s >= n then s else s ^ String.make (n - String.length s) ' '
let pad_left n s = if String.length s >= n then s else String.make (n - String.length s) ' ' ^ s
let oneline s = String.map (fun c -> if c = '\n' || c = '\t' || c = '\r' then ' ' else c) s
let q s = "\"" ^ s ^ "\""

(* longest common prefix length — for pointing a caret at the first divergence *)
let common_prefix a b =
  let n = min (String.length a) (String.length b) in
  let rec go i = if i < n && a.[i] = b.[i] then go (i + 1) else i in
  go 0

(* A palette of styling functions. [plain] is all-identity (so colourless output is
   byte-for-byte what it always was); [ansi] wraps tokens in SGR codes. The renderer only
   ever colours a handful of well-delimited tokens, never whole lines, so alignment and the
   divergence caret are computed on the raw (uncoloured) text and stay correct. *)
type palette = {
  bad : string -> string;     (* failures / actual values / errors — red *)
  warn : string -> string;    (* timeouts / in-flight — yellow *)
  good : string -> string;    (* expected values / matched parts — green *)
  ok : string -> string;      (* the 'ok' status of a passed step — dim green *)
  sect : string -> string;    (* section headers — bold cyan *)
  strong : string -> string;  (* test name, rerun command — bold *)
}

let plain = let id s = s in { bad = id; warn = id; good = id; ok = id; sect = id; strong = id }
let sgr code s = "\027[" ^ code ^ "m" ^ s ^ "\027[0m"
let ansi =
  { bad = sgr "1;31"; warn = sgr "1;33"; good = sgr "32"; ok = sgr "2;32";
    sect = sgr "1;36"; strong = sgr "1" }

(* ---------------------------------------------------------------- sections --------- *)

(* the executed pipeline, numbered, the failed/running step marked with ">" *)
let render_trace b ~c trace =
  if trace = [] then ()
  else begin
    let stopped = List.length trace in
    line b ("  " ^ c.sect (Printf.sprintf "pipeline  (stopped at step %d of %d)" stopped stopped));
    let labelw = List.fold_left (fun w s -> max w (String.length s.label)) 0 trace |> min 56 in
    let idxw = String.length (string_of_int stopped) in
    List.iter
      (fun s ->
        let marker = match s.status with Ok -> " " | _ -> c.bad ">" in
        let status =
          let st = pad_right 4 (match s.status with Ok -> "ok" | Failed _ -> "FAIL" | Running -> "..") in
          match s.status with Ok -> c.ok st | Failed _ -> c.bad st | Running -> c.warn st
        in
        let ms = match s.status with Running -> "" | _ -> Printf.sprintf "%.0fms" s.ms in
        line b
          (Printf.sprintf "    %s %s  %s  %s  %s" marker (pad_left idxw (string_of_int s.index))
             (pad_right labelw (truncate labelw s.label)) status ms))
      trace;
    Buffer.add_char b '\n'
  end

(* an "expected vs actual" comparison with an optional divergence caret under the actual *)
let render_compare b ~c ~l1 ~v1 ~l2 ~v2 =
  let lw = max (String.length l1) (String.length l2) in
  let value_col = 6 + lw + 5 in (* "      " indent + padded label + "  :  " separator *)
  let cp = common_prefix v1 v2 in (* computed on the RAW values, before any colouring *)
  line b (Printf.sprintf "      %s  :  %s" (pad_right lw l1) (c.good v1));
  line b (Printf.sprintf "      %s  :  %s" (pad_right lw l2) (c.bad v2));
  if v1 <> v2 && cp > 0 && cp < String.length v2 then
    line b (c.bad (String.make (value_col + cp) ' ' ^ "^ first difference here"))

(* the per-reason "what happened" + the captured surrounding content *)
let render_what b ~c (cond : Cond.t option) (d : Diag.t) =
  let sel = match d.Diag.selector with Some s -> q s | None -> "(selector)" in
  let outer () = match d.Diag.outer_html with Some h -> line b ("      element:  " ^ h) | None -> () in
  let probe () =
    match d.Diag.probe with
    | [] | [ _ ] -> ()
    | parts ->
      line b "      selector probe (which part stops matching):";
      List.iter
        (fun (pfx, ok) -> line b (Printf.sprintf "        %s  %s" (if ok then c.ok "match   " else c.bad "NO MATCH") pfx))
        parts
  in
  let render_compare = render_compare ~c in
  line b ("  " ^ c.sect "what happened");
  (match d.Diag.reason with
   | Diag.No_match ->
     line b (Printf.sprintf "      no element matches  %s" sel);
     probe ()
   | Diag.Hidden_display v ->
     line b (Printf.sprintf "      %s is present but not shown — display:%s" sel v); outer ()
   | Diag.Hidden_visibility v ->
     line b (Printf.sprintf "      %s is present but not shown — visibility:%s" sel v); outer ()
   | Diag.Hidden_opacity ->
     line b (Printf.sprintf "      %s is present but invisible — opacity:0 (often a fade still in progress)" sel); outer ()
   | Diag.Zero_size ->
     line b (Printf.sprintf "      %s is present but has a zero-size box (no width or no height)" sel); outer ()
   | Diag.Disabled ->
     line b (Printf.sprintf "      %s is visible but [disabled] — it cannot be interacted with" sel); outer ()
   | Diag.Covered c ->
     line b (Printf.sprintf "      %s is visible but covered at its centre by  %s" sel c);
     line b "      a click would land on that element instead";
     outer ()
   | Diag.Not_hit_testable ->
     line b (Printf.sprintf "      %s is visible but nothing is hit-testable at its centre (off-screen?)" sel); outer ()
   | Diag.Still_visible ->
     line b (Printf.sprintf "      expected %s to be hidden, but it is still visible" sel); outer ()
   | Diag.Still_present n ->
     line b (Printf.sprintf "      expected %s to be removed, but %d still match" sel n)
   | Diag.Wrong_count n ->
     let want = match cond with Some (Cond.Count (_, m)) -> string_of_int m | _ -> "?" in
     line b (Printf.sprintf "      wanted %s to match %s element(s); found %d" sel want n)
   | Diag.Text_mismatch actual ->
     let want = match cond with Some (Cond.Text (_, s)) -> s | _ -> "?" in
     line b (Printf.sprintf "      %s exists, but its text does not contain the expected string" sel);
     render_compare b ~l1:"expected to contain" ~v1:want ~l2:"actual text" ~v2:(truncate 120 (oneline actual));
     outer ()
   | Diag.Value_mismatch actual ->
     let want = match cond with Some (Cond.Value (_, s)) -> s | _ -> "?" in
     let av = match actual with Some s -> s | None -> "(no value property)" in
     line b (Printf.sprintf "      %s exists, but its value is not what was expected" sel);
     render_compare b ~l1:"expected value" ~v1:want ~l2:"actual value" ~v2:(truncate 120 av);
     outer ()
   | Diag.Attr_absent ->
     let name, want = match cond with Some (Cond.Attr (_, n, v)) -> (n, v) | _ -> ("?", "?") in
     line b (Printf.sprintf "      %s exists, but has no [%s] attribute (expected %s)" sel name (q want)); outer ()
   | Diag.Attr_mismatch actual ->
     let name, want = match cond with Some (Cond.Attr (_, n, v)) -> (n, v) | _ -> ("?", "?") in
     line b (Printf.sprintf "      %s exists, but its [%s] attribute is wrong" sel name);
     render_compare b ~l1:"expected" ~v1:want ~l2:"actual" ~v2:(truncate 120 actual);
     outer ()
   | Diag.Url_mismatch actual ->
     let want = match cond with Some (Cond.Url s) -> s | _ -> "?" in
     line b "      the URL does not contain the expected string";
     render_compare b ~l1:"expected to contain" ~v1:want ~l2:"actual url" ~v2:actual
   | Diag.Js_false ->
     let e = match cond with Some (Cond.Js e) -> e | _ -> "(predicate)" in
     line b (Printf.sprintf "      the JS predicate stayed false:  %s" (truncate 100 e))
   | Diag.Js_threw err ->
     let e = match cond with Some (Cond.Js e) -> e | _ -> "(predicate)" in
     line b (Printf.sprintf "      the JS predicate threw:  %s" (truncate 100 e));
     line b (Printf.sprintf "        error:  %s" (truncate 160 (oneline err)))
   | Diag.Nav_error e -> line b (Printf.sprintf "      navigation failed:  %s" e)
   | Diag.Nav_timeout -> line b "      navigation did not reach its 'load' event in time"
   | Diag.Backend_error m -> line b (Printf.sprintf "      a browser/protocol error occurred:  %s" (truncate 160 (oneline m)))
   | Diag.Unknown s -> line b (Printf.sprintf "      %s" (if s = "" then "the condition was not met" else s)));
  Buffer.add_char b '\n'

(* url, readyState, and the captured console — console errors are high-signal *)
let render_page b ~c (d : Diag.t) =
  if d.Diag.url = "" && d.Diag.logs = [] then ()
  else begin
    line b ("  " ^ c.sect "page");
    if d.Diag.url <> "" then line b (Printf.sprintf "      url        %s   (readyState: %s)" d.Diag.url d.Diag.ready);
    (match d.Diag.logs with
     | [] -> line b "      console    (no messages)"
     | logs ->
       line b "      console    the page logged the following (often the real cause):";
       List.iter (fun l -> line b ("                 - " ^ truncate 160 (oneline l))) logs);
    Buffer.add_char b '\n'
  end

(* the Rust-style "help" — what to check and why, tuned to THIS failure mode *)
let render_hint b ~c (d : Diag.t) =
  let say lines = line b ("  " ^ c.sect "hint"); List.iter (fun l -> line b ("      " ^ l)) lines; Buffer.add_char b '\n' in
  match d.Diag.reason with
  | Diag.No_match ->
    say [ "Nothing matched the selector. Check for a typo, an element that has not"; "rendered yet (did the previous navigation/hydration finish?), or content inside"; "an iframe or shadow DOM (querySelector cannot reach those)." ]
  | Diag.Hidden_display _ | Diag.Hidden_visibility _ | Diag.Hidden_opacity | Diag.Zero_size ->
    say [ "The element is in the DOM but not shown. A parent may be collapsed, a"; "show/expand interaction may not have happened, or it is mid-transition." ]
  | Diag.Disabled ->
    say [ "The element is disabled, so a precondition is unmet — e.g. an invalid form,"; "a still-loading state, or a feature flag. Satisfy that first." ]
  | Diag.Covered _ ->
    say [ "Another element sits on top at the click point — typically an overlay, modal,"; "cookie banner, or sticky header. Dismiss/scroll past it before interacting." ]
  | Diag.Not_hit_testable ->
    say [ "The element is technically visible but not at a hit-testable point (likely"; "scrolled off-screen or clipped by an ancestor with overflow:hidden)." ]
  | Diag.Still_visible ->
    say [ "It was expected to disappear. A close/dismiss/await did not take effect, or"; "the wait was too short for an exit animation." ]
  | Diag.Still_present _ ->
    say [ "Elements that should have been removed are still present — a delete/clear did"; "not complete, or a re-render kept them." ]
  | Diag.Wrong_count _ ->
    say [ "The list rendered a different number of items than expected — check the data"; "source, filtering, or pagination." ]
  | Diag.Text_mismatch _ ->
    say [ "The element is correct; only its text differs. Compare the two lines above —"; "likely wrong data, formatting, or an off-by-one. The match is substring-contains." ]
  | Diag.Value_mismatch _ ->
    say [ "The input's value differs. The fill may not have applied, or a mask/formatter"; "transformed it. The match is exact." ]
  | Diag.Attr_absent | Diag.Attr_mismatch _ ->
    say [ "The attribute is missing or has a different value than expected." ]
  | Diag.Url_mismatch _ ->
    say [ "Navigation went somewhere else — a redirect, the wrong link, or a full reload"; "that reset client state. Compare expected vs actual above." ]
  | Diag.Js_false ->
    say [ "The predicate never became true within the timeout. Check the expression and"; "the application state it depends on." ]
  | Diag.Js_threw _ ->
    say [ "The predicate threw — fix the expression or the page state it assumes (the"; "error message is above)." ]
  | Diag.Nav_error _ ->
    say [ "The browser could not load the URL — the server may be down or unreachable,"; "the URL may be wrong, or a network error occurred." ]
  | Diag.Nav_timeout ->
    say [ "The page started loading but never fired 'load' — the server may be slow, or a"; "resource (script/stylesheet/XHR) hung. Check the server and the network." ]
  | Diag.Backend_error _ | Diag.Unknown _ -> ()

let render_rerun b ~c rerun = line b ("  " ^ c.sect "rerun"); line b ("      " ^ c.strong rerun)

(* the full report for a failed test. [color] adds ANSI styling to a few key tokens; with
   it off (the default) the output is byte-for-byte the plain report. *)
let render ?(color = false) (f : t) : string =
  let c = if color then ansi else plain in
  let b = Buffer.create 1024 in
  Buffer.add_char b '\n';
  (match f.kind with
   | Assertion -> line b (c.bad "FAIL" ^ "  " ^ c.strong f.test)
   | Errored e -> line b (c.bad "ERROR" ^ "  " ^ c.strong f.test); line b (Printf.sprintf "       the test raised: %s" (truncate 200 (oneline e)))
   | Timed_out secs -> line b (c.warn "TIMEOUT" ^ "  " ^ c.strong f.test); line b (Printf.sprintf "         exceeded the per-test budget of %.1fs" secs));
  Buffer.add_char b '\n';
  render_trace b ~c f.trace;
  (* the failed step's condition + diagnostic (assertion failures only) *)
  (match List.rev f.trace with
   | { status = Failed (cond, d); _ } :: _ -> render_what b ~c cond d; render_page b ~c d; render_hint b ~c d
   | _ -> ());
  render_rerun b ~c f.rerun;
  Buffer.contents b

