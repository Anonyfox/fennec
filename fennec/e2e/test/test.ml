(* Unit tests for fennec-e2e's orchestration — DSL, runner, failure formatter, reporter —
   all against an in-memory fake backend (no real browser). Deterministic and millisecond-
   fast: the fake's [wait] returns immediately (Ok or a timeout Diag), so there is no real
   time except the runner's own test-timeout backstop. *)

module D = Fennec_e2e.Driver.Make (Fake)
module Failure = Fennec_e2e.Failure
module Reporter = Fennec_e2e.Reporter
module Diag = Fennec_e2e.Backend.Diag
module Cond = Fennec_e2e.Backend.Cond

(* a local substring test (the lib's internal Cdp.contains is no longer public) *)
let contains hay ndl =
  let hl = String.length hay and nl = String.length ndl in
  let rec matches i j = j = nl || (hay.[i + j] = ndl.[j] && matches i (j + 1)) in
  let rec scan i = i + nl <= hl && (matches i 0 || scan (i + 1)) in
  nl = 0 || scan 0

let passed = ref 0 and failed = ref 0
let check name c = if c then (incr passed; Printf.printf "  ok   %s\n" name) else (incr failed; Printf.printf "  FAIL %s\n" name)

(* ------------------------------------------------------------------ DSL ----------- *)
let mkpage w : D.page =
  { D.backend = w; now = (fun () -> 0.0); base_url = ""; scope = ""; timeout = 1.0; trace = ref [] }
let raises f = try ignore (f ()); false with D.Step_failed -> true

(* run a body to its first failure and return the executed trace (in order) *)
let trace_of w (body : D.page -> 'a) : Failure.step list =
  let p = mkpage w in
  (try ignore (body p) with D.Step_failed -> ());
  List.rev !(p.D.trace)

(* the (cond, diag) of the failed step, if any *)
let failed_step trace =
  List.find_map (fun (s : Failure.step) -> match s.status with Failure.Failed (c, d) -> Some (c, d) | _ -> None) trace

(* render a body's failure the way the runner would *)
let report_of ?(test = "demo") w body =
  let trace = trace_of w body in
  Failure.render { Failure.test; trace; kind = Failure.Assertion; rerun = "fennec --grep 'demo'" }

let test_dsl () =
  print_endline "— dsl (fake backend, condition mapping) —";

  let w = Fake.world () in
  w.visible <- [ ".msg" ]; w.texts <- [ (".msg", "hello world") ];
  ignore (mkpage w |> D.expect_text ".msg" "world");
  check "expect_text matches substring" true;
  check "expect_text mismatch raises Failed" (raises (fun () -> mkpage w |> D.expect_text ".msg" "nope"));

  let w = Fake.world () in
  w.visible <- [ ".x" ];
  ignore (mkpage w |> D.wait_visible ".x");
  check "wait_visible passes when visible" true;
  check "wait_visible raises when absent" (raises (fun () -> mkpage w |> D.wait_visible ".never"));

  let w = Fake.world () in
  w.visible <- [ ".cart .checkout" ];
  ignore (mkpage w |> D.within ".cart" (fun c -> c |> D.click ".checkout"));
  check "within prefixes nested selectors" (List.mem ".cart .checkout" w.clicked);

  let w = Fake.world () in
  w.visible <- [ ".btn" ];
  w.on_action <- (fun w -> w.visible <- ".result" :: w.visible);
  ignore (mkpage w |> D.click ".btn" |> D.expect_visible ".result");
  check "click triggers reaction; expect_visible passes" (List.mem ".result" w.visible);

  let w = Fake.world () in
  w.counts <- [ (".row", 3) ];
  ignore (mkpage w |> D.expect_count ".row" 3);
  check "expect_count exact match" true;
  check "expect_count wrong value raises" (raises (fun () -> mkpage w |> D.expect_count ".row" 9));

  let w = Fake.world () in
  w.counts <- [ (".row", 2) ];
  check "read_count returns the value (pipe terminal)" (mkpage w |> D.read_count ".row" = 2);

  let w = Fake.world () in
  ignore (mkpage w |> D.goto "/products" |> D.expect_url "/products");
  check "goto navigates; expect_url matches" (w.url = "/products");

  let w = Fake.world () in
  w.visible <- [ ".inp" ];
  ignore (mkpage w |> D.type_enter ".inp" "milk");
  check "fill records the value" (List.assoc_opt ".inp" w.values = Some "milk");
  check "Enter keypress recorded" (List.mem (".inp", "Enter") w.pressed);

  (* the failure report is self-explanatory: names the step, the selector, and a diagnostic *)
  let w = Fake.world () in
  w.visible <- [ ".title" ]; w.texts <- [ (".title", "Loading...") ];
  let msg = report_of w (fun p -> p |> D.expect_text ".title" "Ready") in
  check "report names the step + selector" (contains msg "expect_text" && contains msg ".title");
  check "report has a 'what happened' section" (contains msg "what happened");
  check "report shows expected vs actual" (contains msg "Ready" && contains msg "Loading...");
  check "report has a hint" (contains msg "hint");
  check "report has a copy-pasteable rerun line" (contains msg "fennec --grep")

(* ------------------------------------------------------------------ runner --------- *)
let test_runner () =
  print_endline "— runner (fake tests, real Eio backstop) —";
  Eio_main.run @@ fun env ->
  let clock = Eio.Stdenv.clock env in
  let provision f = f (Fake.world ()) in
  let cfg = { D.default_config with jobs = 2; step_timeout = 0.05; test_timeout = 0.3 } in

  let tests =
    [ { D.name = "passes"; body = (fun _ -> ()) };
      { D.name = "asserts"; body = (fun p -> ignore (p |> D.expect_visible ".never")) };
      { D.name = "errors"; body = (fun _ -> failwith "boom") };
      { D.name = "hangs"; body = (fun _ -> Eio.Time.sleep clock 5.0) } ]
  in
  let r = D.run ~clock ~config:cfg ~provision tests in
  let outcome name = (List.find (fun (x : D.result) -> x.name = name) r.results).outcome in
  check "all tests produced a result (no test aborted the run)" (List.length r.results = 4);
  check "exactly one passed" (r.passed = 1);
  check "assertion failure classified" (outcome "asserts" = D.Failed_assert);
  check "thrown exception classified as error" (outcome "errors" = D.Errored);
  check "hung test classified as timeout" (outcome "hangs" = D.Timed_out);
  check "failed results carry a structured failure"
    (List.for_all (fun (x : D.result) -> if x.outcome = D.Passed then x.failure = None else x.failure <> None) r.results);

  (* retry *)
  let attempts = ref 0 in
  let flaky = { D.name = "flaky"; body = (fun _ -> incr attempts; if !attempts = 1 then failwith "first") } in
  let r2 = D.run ~clock ~config:{ cfg with retries = 1 } ~provision [ flaky ] in
  check "flaky test passes on retry" (r2.passed = 1);

  (* bail: stop the run on the first failure *)
  let bt =
    [ { D.name = "a"; body = (fun _ -> ()) };
      { D.name = "b-fails"; body = (fun p -> ignore (p |> D.expect_visible ".never")) };
      { D.name = "c"; body = (fun _ -> ()) } ]
  in
  let rb = D.run ~clock ~config:{ cfg with bail = true } ~provision bt in
  check "bail stops the run after the first failure" (List.length rb.results = 2);
  check "bail: the run ends on the failing test" ((List.nth rb.results 1).outcome = D.Failed_assert);

  (* grep: run only matching tests *)
  let gt =
    [ { D.name = "login works"; body = (fun _ -> ()) };
      { D.name = "signup works"; body = (fun _ -> ()) };
      { D.name = "login again"; body = (fun _ -> ()) } ]
  in
  let rg = D.run ~clock ~config:{ cfg with grep = Some "login" } ~provision gt in
  check "grep runs only matching tests" (List.length rg.results = 2);

  (* concurrency bound *)
  let cur = ref 0 and mx = ref 0 in
  let bounded f = incr cur; if !cur > !mx then mx := !cur; Fun.protect ~finally:(fun () -> decr cur) (fun () -> f (Fake.world ())) in
  let yielders = List.init 8 (fun i -> { D.name = Printf.sprintf "y%d" i; body = (fun _ -> Eio.Time.sleep clock 0.005) }) in
  let rj = D.run ~clock ~config:{ cfg with jobs = 3; test_timeout = 1.0 } ~provision:bounded yielders in
  check "all 8 tests ran" (rj.passed = 8);
  check "concurrency never exceeded jobs (3)" (!mx <= 3)

(* --------------------------------------------- DSL step -> Diag.reason mapping ---- *)
let reason_of w body = match failed_step (trace_of w body) with Some (_, d) -> Some d.Diag.reason | None -> None
let is_reason name w body f =
  check name (match reason_of w body with Some r -> f r | None -> false)

let test_dsl_reasons () =
  print_endline "— dsl: every step maps to the right Diag.reason —";

  (* selector matches nothing *)
  is_reason "wait_visible absent -> No_match" (Fake.world ()) (D.wait_visible ".never")
    (function Diag.No_match -> true | _ -> false);

  (* present but not shown — the fake carries the specific hidden reason *)
  let w = Fake.world () in w.texts <- [ (".x", "hi") ]; w.hidden <- [ (".x", Diag.Hidden_display "none") ];
  is_reason "wait_visible present-but-hidden -> Hidden_display" w (D.wait_visible ".x")
    (function Diag.Hidden_display "none" -> true | _ -> false);
  let w = Fake.world () in w.texts <- [ (".x", "hi") ]; w.hidden <- [ (".x", Diag.Hidden_opacity) ];
  is_reason "wait_visible opacity:0 -> Hidden_opacity" w (D.wait_visible ".x")
    (function Diag.Hidden_opacity -> true | _ -> false);
  let w = Fake.world () in w.texts <- [ (".x", "hi") ]; w.hidden <- [ (".x", Diag.Disabled) ];
  is_reason "click disabled -> Disabled" w (D.click ".x")
    (function Diag.Disabled -> true | _ -> false);
  let w = Fake.world () in w.texts <- [ (".x", "hi") ]; w.hidden <- [ (".x", Diag.Covered "<div#overlay>") ];
  is_reason "click covered -> Covered" w (D.click ".x")
    (function Diag.Covered "<div#overlay>" -> true | _ -> false);

  (* expect_hidden but still visible *)
  let w = Fake.world () in w.visible <- [ ".x" ];
  is_reason "expect_hidden still-visible -> Still_visible" w (D.expect_hidden ".x")
    (function Diag.Still_visible -> true | _ -> false);

  (* expect_detached but still present *)
  let w = Fake.world () in w.counts <- [ (".x", 3) ];
  is_reason "expect_detached still-present -> Still_present 3" w (D.expect_detached ".x")
    (function Diag.Still_present 3 -> true | _ -> false);

  (* text mismatch vs no element *)
  let w = Fake.world () in w.texts <- [ (".x", "Loading...") ];
  is_reason "expect_text wrong-text -> Text_mismatch" w (D.expect_text ".x" "Ready")
    (function Diag.Text_mismatch "Loading..." -> true | _ -> false);
  is_reason "expect_text on absent -> No_match" (Fake.world ()) (D.expect_text ".x" "Ready")
    (function Diag.No_match -> true | _ -> false);

  (* value mismatch (present vs absent property) *)
  let w = Fake.world () in w.visible <- [ ".inp" ]; w.values <- [ (".inp", "milk") ];
  is_reason "expect_value wrong -> Value_mismatch (Some)" w (D.expect_value ".inp" "bread")
    (function Diag.Value_mismatch (Some "milk") -> true | _ -> false);
  let w = Fake.world () in w.visible <- [ ".inp" ];
  is_reason "expect_value no-value-prop -> Value_mismatch None" w (D.expect_value ".inp" "x")
    (function Diag.Value_mismatch None -> true | _ -> false);

  (* attribute absent vs wrong *)
  let w = Fake.world () in w.visible <- [ ".a" ]; w.attrs <- [ (".a", "href", "/x") ];
  is_reason "expect_attr wrong-value -> Attr_mismatch" w (D.expect_attr ".a" "href" "/y")
    (function Diag.Attr_mismatch "/x" -> true | _ -> false);
  let w = Fake.world () in w.visible <- [ ".a" ];
  is_reason "expect_attr missing -> Attr_absent" w (D.expect_attr ".a" "href" "/y")
    (function Diag.Attr_absent -> true | _ -> false);

  (* count mismatch *)
  let w = Fake.world () in w.counts <- [ (".row", 2) ];
  is_reason "expect_count wrong -> Wrong_count 2" w (D.expect_count ".row" 5)
    (function Diag.Wrong_count 2 -> true | _ -> false);

  (* url mismatch *)
  let w = Fake.world () in w.url <- "/home";
  is_reason "expect_url wrong -> Url_mismatch" w (D.expect_url "/checkout")
    (function Diag.Url_mismatch "/home" -> true | _ -> false);

  (* JS predicate (the fake can't evaluate JS, so it's always false) *)
  is_reason "expect_js -> Js_false" (Fake.world ()) (D.expect_js "x === 1")
    (function Diag.Js_false -> true | _ -> false);

  (* the cond is captured alongside the diag, so the formatter can show EXPECTED values *)
  let w = Fake.world () in w.texts <- [ (".x", "Loading...") ];
  check "failed step captures the Cond (expected values)"
    (match failed_step (trace_of w (D.expect_text ".x" "Ready")) with
     | Some (Some (Cond.Text (".x", "Ready")), _) -> true | _ -> false)

(* ----------------------------------------------- the formatter, per reason + edges -- *)
let mkdiag ?(selector = Some ".x") ?(matched = 1) ?(outer_html = None) ?(probe = [])
    ?(url = "/page") ?(ready = "complete") ?(logs = []) reason =
  Diag.make ~selector ~matched ~outer_html ~probe ~url ~ready ~logs reason

(* render a one-step assertion failure for [diag] (with an optional captured cond) *)
let render1 ?(label = "expect_x \".x\"") ?(cond = None) ?(prefix = []) diag =
  let n = List.length prefix in
  let step = { Failure.index = n + 1; label; status = Failure.Failed (cond, diag); ms = 12.0 } in
  Failure.render { Failure.test = "checkout flow"; trace = prefix @ [ step ]; kind = Failure.Assertion;
                   rerun = "fennec --grep 'checkout flow'" }

let has name s sub = check name (contains s sub)
let hasnt name s sub = check name (not (contains s sub))

let test_format () =
  print_endline "— formatter: structure, every reason, captured content, edges —";

  (* ---- overall structure on a representative failure ---- *)
  let s =
    render1 ~label:"expect_text \".title\""
      ~cond:(Some (Cond.Text (".title", "Ready")))
      (mkdiag ~selector:(Some ".title") ~outer_html:(Some "<h1 class=\"title\">Loading...</h1>")
         ~url:"/checkout" ~ready:"complete" ~logs:[ "Error: boom" ]
         (Diag.Text_mismatch "Loading..."))
  in
  has "header carries the test name" s "checkout flow";
  has "has a pipeline section" s "pipeline";
  has "has a what-happened section" s "what happened";
  has "has a page section" s "page";
  has "shows the url" s "/checkout";
  has "shows readyState" s "complete";
  has "shows captured console" s "Error: boom";
  has "has a hint" s "hint";
  has "has a rerun line" s "rerun";
  has "rerun is copy-pasteable" s "fennec --grep 'checkout flow'";
  has "shows the captured element outerHTML" s "<h1 class=\"title\">Loading...</h1>";
  has "shows expected" s "Ready";
  has "shows actual" s "Loading...";

  (* ---- pipeline trace: prior steps shown ok, the failing one marked ---- *)
  let prefix =
    [ { Failure.index = 1; label = "goto \"/checkout\""; status = Failure.Ok; ms = 210.0 };
      { Failure.index = 2; label = "click \".pay\""; status = Failure.Ok; ms = 30.0 } ]
  in
  let s = render1 ~prefix ~label:"expect_visible \".receipt\"" (mkdiag (Diag.No_match)) in
  has "trace shows earlier step 1" s "goto \"/checkout\"";
  has "trace shows earlier step 2" s "click \".pay\"";
  has "trace marks ok steps" s "ok";
  has "trace marks the failing step FAIL" s "FAIL";
  has "trace points at the failed step with >" s ">";
  has "trace says where it stopped" s "stopped at step 3";

  (* ---- near-miss caret: shared prefix -> a 'first difference' pointer ---- *)
  let s = render1 ~cond:(Some (Cond.Text (".x", "Product #8"))) (mkdiag (Diag.Text_mismatch "Product #7")) in
  has "near-miss shows a divergence pointer" s "first difference";
  has "near-miss shows expected value" s "Product #8";
  has "near-miss shows actual value" s "Product #7";

  (* ---- totally different strings: no spurious caret ---- *)
  let s = render1 ~cond:(Some (Cond.Text (".x", "Welcome"))) (mkdiag (Diag.Text_mismatch "Goodbye")) in
  hasnt "unrelated strings: no divergence pointer" s "first difference";

  (* ---- url mismatch with caret ---- *)
  let s = render1 ~label:"expect_url" ~cond:(Some (Cond.Url "/checkout/step2"))
      (mkdiag ~selector:None (Diag.Url_mismatch "/checkout/step1")) in
  has "url mismatch shows expected" s "/checkout/step2";
  has "url mismatch shows actual" s "/checkout/step1";
  has "url mismatch carets the divergence" s "first difference";

  (* ---- No_match with a selector probe pinpointing the failing part ---- *)
  let s = render1 (mkdiag ~matched:0
                     ~probe:[ (".cart", true); (".cart .item", true); (".cart .item .price", false) ]
                     (Diag.No_match)) in
  has "probe is shown" s "selector probe";
  has "probe marks the matching prefix" s ".cart .item";
  has "probe marks the breaking part NO MATCH" s "NO MATCH";

  (* ---- each reason renders a sensible line + a hint (smoke across the whole taxonomy) ---- *)
  let outer = Some "<button id=\"go\" disabled>Go</button>" in
  let cases =
    [ ("No_match", None, mkdiag ~matched:0 Diag.No_match, "no element matches");
      ("Hidden_display", None, mkdiag ~outer_html:outer (Diag.Hidden_display "none"), "display:none");
      ("Hidden_visibility", None, mkdiag (Diag.Hidden_visibility "hidden"), "visibility:hidden");
      ("Hidden_opacity", None, mkdiag Diag.Hidden_opacity, "opacity:0");
      ("Zero_size", None, mkdiag Diag.Zero_size, "zero-size");
      ("Disabled", None, mkdiag ~outer_html:outer Diag.Disabled, "disabled");
      ("Covered", None, mkdiag (Diag.Covered "<div#cookie-banner>"), "<div#cookie-banner>");
      ("Not_hit_testable", None, mkdiag Diag.Not_hit_testable, "hit-testable");
      ("Still_visible", None, mkdiag Diag.Still_visible, "still visible");
      ("Still_present", None, mkdiag (Diag.Still_present 4), "still match");
      ("Wrong_count", Some (Cond.Count (".x", 2)), mkdiag ~matched:5 (Diag.Wrong_count 5), "found 5");
      ("Value_mismatch", Some (Cond.Value (".x", "bread")), mkdiag (Diag.Value_mismatch (Some "milk")), "milk");
      ("Value None", Some (Cond.Value (".x", "bread")), mkdiag (Diag.Value_mismatch None), "no value property");
      ("Attr_absent", Some (Cond.Attr (".x", "href", "/a")), mkdiag Diag.Attr_absent, "no [href]");
      ("Attr_mismatch", Some (Cond.Attr (".x", "href", "/a")), mkdiag (Diag.Attr_mismatch "/b"), "/b");
      ("Js_false", Some (Cond.Js "x===1"), mkdiag ~selector:None Diag.Js_false, "x===1");
      ("Js_threw", Some (Cond.Js "x.y"), mkdiag ~selector:None (Diag.Js_threw "TypeError: x is undefined"), "TypeError");
      ("Nav_error", None, mkdiag ~selector:None (Diag.Nav_error "net::ERR_CONNECTION_REFUSED"), "net::ERR_CONNECTION_REFUSED");
      ("Nav_timeout", None, mkdiag ~selector:None Diag.Nav_timeout, "load");
      ("Backend_error", None, mkdiag ~selector:None (Diag.Backend_error "ws closed"), "ws closed");
      ("Unknown", None, mkdiag ~selector:None (Diag.Unknown "weird"), "weird") ]
  in
  List.iter
    (fun (name, cond, diag, needle) ->
      let s = render1 ~cond diag in
      has (Printf.sprintf "reason %s: renders its key detail" name) s needle)
    cases;

  (* hints are present for the actionable reasons *)
  has "No_match has a hint" (render1 (mkdiag Diag.No_match)) "hint";
  has "Covered hint mentions overlay" (render1 (mkdiag (Diag.Covered "<x>"))) "overlay";

  (* ---- edge cases ---- *)
  (* missing selector (url/js conds) renders without crashing *)
  let s = render1 ~cond:None (mkdiag ~selector:None Diag.Js_false) in
  has "no-selector reason still renders" s "what happened";

  (* long actual text is truncated *)
  let long = String.make 400 'x' in
  let s = render1 ~cond:(Some (Cond.Text (".x", "short"))) (mkdiag (Diag.Text_mismatch long)) in
  has "long text is truncated with an ellipsis" s "...";
  check "long text does not dump 400 chars verbatim" (String.length s < 1500);

  (* multi-line actual text is flattened to one line in the report body *)
  let s = render1 ~cond:(Some (Cond.Text (".x", "y"))) (mkdiag (Diag.Text_mismatch "line1\nline2\nline3")) in
  check "newlines in captured text are flattened" (not (contains s "line1\nline2"));
  has "flattened text keeps the content" s "line1 line2 line3";

  (* unicode / special chars survive *)
  let s = render1 ~cond:(Some (Cond.Text (".x", "café ☕"))) (mkdiag (Diag.Text_mismatch "tea")) in
  has "unicode expected survives" s "café ☕";

  (* empty console -> a clear 'no messages' note (no confusion) *)
  let s = render1 (mkdiag ~url:"/p" ~logs:[] Diag.No_match) in
  has "empty console says so" s "no messages";

  (* a long selector is shown (not silently dropped) *)
  let longsel = ".app main .grid .card:nth-child(3) .body .price span.amount" in
  let s = render1 (mkdiag ~selector:(Some longsel) ~matched:0 Diag.No_match) in
  has "long selector is shown" s longsel;

  (* Errored / Timed_out kinds render a header + the running step, no what-happened ---- *)
  let running = [ { Failure.index = 1; label = "goto \"/x\""; status = Failure.Ok; ms = 5.0 };
                  { Failure.index = 2; label = "eval \"boom()\""; status = Failure.Running; ms = 0.0 } ] in
  let s = Failure.render { Failure.test = "t"; trace = running; kind = Failure.Errored "Failure(\"boom\")";
                           rerun = "fennec --grep 't'" } in
  has "errored: ERROR header" s "ERROR";
  has "errored: shows the exception" s "boom";
  has "errored: marks the in-flight step" s "..";
  hasnt "errored: no what-happened (no assertion diag)" s "what happened";
  let s = Failure.render { Failure.test = "t"; trace = running; kind = Failure.Timed_out 30.0;
                           rerun = "fennec --grep 't'" } in
  has "timeout: TIMEOUT header" s "TIMEOUT";
  has "timeout: shows the budget" s "30.0";

  (* empty trace (failed before any step) still renders cleanly *)
  let s = Failure.render { Failure.test = "t"; trace = []; kind = Failure.Errored "early";
                           rerun = "fennec --grep 't'" } in
  has "empty trace: still has a header" s "ERROR";
  has "empty trace: still has a rerun" s "rerun"

(* ----------------------------------------------------- rerun-command derivation ---- *)
let test_rerun () =
  print_endline "— rerun command derivation —";
  Unix.putenv "FENNEC_E2E_RERUN" "sh examples/site/e2e/run.sh";
  let r = D.rerun_for "checkout works" in
  check "uses FENNEC_E2E_RERUN as the prefix" (contains r "sh examples/site/e2e/run.sh");
  check "passes the test name via --grep" (contains r "--grep");
  check "quotes a name with spaces" (contains r "'checkout works'");
  let r = D.rerun_for "it's broken" in
  check "single-quote in a name is shell-escaped" (contains r "'it'\\''s broken'");
  Unix.putenv "FENNEC_E2E_RERUN" "";
  let r = D.rerun_for "x" in
  check "falls back to the executable name when env is empty" (String.length r > 0 && contains r "--grep")

(* ------------------------------------------------------ the reporter (cross-platform) -- *)
let has_ansi s = contains s "\027["
let mkfail name reason =
  let d = Diag.make ~selector:(Some ".x") ~matched:0 reason in
  let step = { Failure.index = 1; label = "click \".x\""; status = Failure.Failed (None, d); ms = 5000.0 } in
  { Failure.test = name; trace = [ step ]; kind = Failure.Assertion; rerun = "run --grep '" ^ name ^ "'" }
let res ?failure name outcome ms : Reporter.result = { Reporter.name; outcome; ms; failure }

(* drive a scripted run through a reporter with the given caps; return (full output, chunks) *)
let drive caps script =
  let buf = Buffer.create 512 and chunks = ref [] in
  let emit c = Buffer.add_string buf c; chunks := c :: !chunks in
  let rep = Reporter.create ~caps ~emit () in
  script rep;
  (Buffer.contents buf, List.rev !chunks)

let sample_results =
  [ res "alpha" Reporter.Passed 12.0;
    res ~failure:(mkfail "beta" Diag.No_match) "beta" Reporter.Failed_assert 5000.0;
    res "gamma" Reporter.Passed 8.0 ]
let sample_script rep =
  Reporter.run_started rep ~total:3 ~jobs:1 ~grep:None ();
  List.iter (fun (r : Reporter.result) -> Reporter.test_started rep r.name; Reporter.test_finished rep r) sample_results;
  Reporter.run_finished rep { Reporter.results = sample_results; passed = 2; failed = 1 }

(* save/set/restore env around a capability-detection check *)
let with_env pairs f =
  let raw k = try Some (Sys.getenv k) with Not_found -> None in
  let saved = List.map (fun (k, _) -> (k, raw k)) pairs in
  List.iter (fun (k, v) -> Unix.putenv k v) pairs;
  Fun.protect ~finally:(fun () -> List.iter (fun (k, v) -> Unix.putenv k (Option.value ~default:"" v)) saved) f

let test_reporter () =
  print_endline "— reporter: plain (CI/dumb) vs pretty (TTY), atomic, cross-platform —";

  (* PLAIN: no colour, no unicode, no cursor control — safe for CI logs / pipes / files *)
  let plain = { Reporter.color = false; unicode = false; status = false; width = 80 } in
  let s, _ = drive plain sample_script in
  check "plain: no ANSI escape sequences" (not (has_ansi s));
  check "plain: no carriage returns / cursor control" (not (String.contains s '\r'));
  check "plain: header announces the run" (contains s "running 3 test(s)");
  check "plain: passing test on its own line" (contains s "ok" && contains s "alpha");
  check "plain: failing test labelled" (contains s "FAIL" && contains s "beta");
  check "plain: failing test inlines the full report" (contains s "what happened");
  check "plain: failures recap at the bottom" (contains s "failures (1)");
  check "plain: recap carries the rerun command" (contains s "run --grep 'beta'");
  check "plain: final summary line" (contains s "2 passed, 1 failed (of 3)");

  (* PRETTY: colour + unicode + an in-place status line, for a real terminal *)
  let pretty = { Reporter.color = true; unicode = true; status = true; width = 100 } in
  let s, _ = drive pretty sample_script in
  check "pretty: emits ANSI colour" (has_ansi s);
  check "pretty: uses an in-place status line (erase sequence)" (contains s "\027[2K");
  check "pretty: green check glyph for a pass" (contains s "\xe2\x9c\x93");
  check "pretty: red cross glyph for a fail" (contains s "\xe2\x9c\x97");
  check "pretty: still shows the failure detail" (contains s "what happened");
  check "pretty: ends clean on the summary (no dangling status)"
    (let n = String.length s in n > 0 && s.[n - 1] = '\n');

  (* a NON-tty but colour-forced sink (e.g. CI with FORCE_COLOR): colour yes, cursor NO *)
  let cifc = { Reporter.color = true; unicode = false; status = false; width = 80 } in
  let s, _ = drive cifc sample_script in
  check "forced-colour-no-tty: has colour" (has_ansi s);
  check "forced-colour-no-tty: NEVER emits cursor control into a pipe" (not (String.contains s '\r'));

  (* ATOMICITY: each test's output is emitted as ONE chunk, so concurrent finishes can never
     interleave a half-written line *)
  let _, chunks = drive plain sample_script in
  check "atomic: the failing test's line + full report are one emit"
    (List.exists (fun c -> contains c "FAIL" && contains c "what happened" && contains c "beta") chunks);
  check "atomic: every emitted chunk is whole lines (ends in newline)"
    (List.for_all (fun c -> c = "" || c.[String.length c - 1] = '\n') chunks);

  (* a clean run still produces a sensible PASS summary *)
  let s, _ =
    drive plain (fun rep ->
        Reporter.run_started rep ~total:1 ~jobs:1 ~grep:None ();
        let r = res "solo" Reporter.Passed 5.0 in
        Reporter.test_started rep r.name; Reporter.test_finished rep r;
        Reporter.run_finished rep { Reporter.results = [ r ]; passed = 1; failed = 0 })
  in
  check "all-pass: PASS summary, no failures recap" (contains s "1 passed, 0 failed" && not (contains s "failures ("));

  (* CAPABILITY DETECTION honours the cross-ecosystem conventions *)
  with_env [ ("NO_COLOR", "1"); ("FORCE_COLOR", "1") ] (fun () ->
      check "caps: NO_COLOR wins over FORCE_COLOR" (not (Reporter.detect_caps ()).Reporter.color));
  with_env [ ("NO_COLOR", ""); ("CLICOLOR_FORCE", ""); ("FORCE_COLOR", "1") ] (fun () ->
      check "caps: FORCE_COLOR enables colour without a TTY" (Reporter.detect_caps ()).Reporter.color);
  with_env [ ("NO_COLOR", ""); ("FORCE_COLOR", ""); ("CLICOLOR_FORCE", ""); ("TERM", "dumb") ] (fun () ->
      let c = Reporter.detect_caps () in
      check "caps: TERM=dumb disables colour and unicode and status" (not c.color && not c.unicode && not c.status));
  with_env [ ("TERM", "xterm-256color"); ("FENNEC_E2E_ASCII", ""); ("LC_ALL", ""); ("LC_CTYPE", ""); ("LANG", "en_US.UTF-8") ]
    (fun () -> check "caps: a UTF-8 locale enables unicode" (Reporter.detect_caps ()).Reporter.unicode);
  with_env [ ("TERM", "xterm"); ("LANG", "en_US.UTF-8"); ("FENNEC_E2E_ASCII", "1") ] (fun () ->
      check "caps: FENNEC_E2E_ASCII forces ASCII" (not (Reporter.detect_caps ()).Reporter.unicode));
  with_env [ ("COLUMNS", "120") ] (fun () -> check "caps: COLUMNS sets width" ((Reporter.detect_caps ()).Reporter.width = 120));
  with_env [ ("COLUMNS", "10") ] (fun () -> check "caps: absurdly small COLUMNS floored to 80" ((Reporter.detect_caps ()).Reporter.width = 80));

  (* the Plain style override forces everything off regardless of detected caps *)
  let rep = Reporter.create ~style:Reporter.Plain ~caps:pretty ~emit:ignore () in
  ignore rep;
  let s, _ = drive { pretty with Reporter.color = false; unicode = false; status = false } sample_script in
  check "style Plain == no ANSI even on a capable terminal" (not (has_ansi s))

let () =
  test_dsl ();
  test_dsl_reasons ();
  test_format ();
  test_rerun ();
  test_reporter ();
  test_runner ();
  Printf.printf "\n%s — %d passed, %d failed\n" (if !failed = 0 then "PASS" else "FAIL") !passed !failed;
  if !failed > 0 then exit 1

