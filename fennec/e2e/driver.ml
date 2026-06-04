(* The page DSL + test runner, over a page {!Backend.S}. Instantiated with the real CDP
   backend for live runs ([Live]) and with an in-memory fake for unit tests.

   Every step is [page -> page] for piping, and every step RECORDS itself into the page's
   trace (label, ok/fail, duration). When a step's condition can't be met, the step is
   marked failed with its {!Backend.Diag} and the pipe short-circuits; the runner turns the
   trace + diagnostic into a {!Failure} report (which test, which step, what the page looked
   like, why it matters, how to re-run). So a failure explains itself completely. *)

module Make (B : Backend.S) = struct
  module Cond = Backend.Cond
  module Diag = Backend.Diag

  type backend = B.t

  type page = {
    backend : backend;
    now : unit -> float;        (* for per-step timing in the trace *)
    base_url : string;          (* prepended to a leading-'/' path in [goto] *)
    scope : string;             (* selector prefix from enclosing [within] blocks *)
    timeout : float;            (* per-step wait budget, seconds *)
    trace : Failure.step list ref; (* executed steps, most-recent first (reversed) *)
  }

  exception Step_failed

  let scoped p sel = if p.scope = "" then sel else p.scope ^ " " ^ sel
  (* labels quote selectors and clip long values, so a trace line stays one readable row *)
  let q s = "\"" ^ s ^ "\""
  let trunc n s = if String.length s <= n then s else String.sub s 0 (max 0 (n - 1)) ^ "..."

  let set_head p f = match !(p.trace) with hd :: tl -> p.trace := f hd :: tl | [] -> ()

  (* run one step: append a Running entry, time it, mark Ok or Failed(+diag) — on failure,
     short-circuit the pipe. [run] returns the backend's (unit, Diag) result. *)
  let record p ?cond ~label (run : unit -> (unit, Diag.t) result) : page =
    let idx = List.length !(p.trace) + 1 in
    p.trace := { Failure.index = idx; label; status = Failure.Running; ms = 0.0 } :: !(p.trace);
    let t0 = p.now () in
    match run () with
    | Ok () -> set_head p (fun s -> { s with Failure.status = Failure.Ok; ms = (p.now () -. t0) *. 1000.0 }); p
    | Error d -> set_head p (fun s -> { s with Failure.status = Failure.Failed (cond, d); ms = (p.now () -. t0) *. 1000.0 }); raise Step_failed

  let wait_cond p ?cond ~label c = record p ?cond:(Some (match cond with Some x -> x | None -> c)) ~label (fun () -> B.wait p.backend c ~timeout:p.timeout)

  (* ---- navigation ---- *)
  let goto path p =
    let url = if String.length path > 0 && path.[0] = '/' then p.base_url ^ path else path in
    record p ~label:(Printf.sprintf "goto %s" (q path)) (fun () -> B.navigate p.backend ~url ~timeout:p.timeout)

  (* ---- actions: wait for the precondition, then act (both inside one trace step) ---- *)
  let click sel p =
    let s = scoped p sel in
    record p ~cond:(Cond.Actionable s) ~label:(Printf.sprintf "click %s" (q s)) (fun () ->
        match B.wait p.backend (Cond.Actionable s) ~timeout:p.timeout with
        | Ok () -> B.click p.backend ~selector:s; Ok ()
        | Error _ as e -> e)
  let fill sel v p =
    let s = scoped p sel in
    record p ~cond:(Cond.Actionable s) ~label:(Printf.sprintf "fill %s %s" (q s) (q (trunc 40 v))) (fun () ->
        match B.wait p.backend (Cond.Actionable s) ~timeout:p.timeout with
        | Ok () -> B.fill p.backend ~selector:s ~value:v; Ok ()
        | Error _ as e -> e)
  let press sel key p =
    let s = scoped p sel in
    record p ~cond:(Cond.Present s) ~label:(Printf.sprintf "press %s %s" (q s) (q key)) (fun () ->
        match B.wait p.backend (Cond.Present s) ~timeout:p.timeout with
        | Ok () -> B.press p.backend ~selector:s ~key; Ok ()
        | Error _ as e -> e)
  let type_enter sel v p = p |> fill sel v |> press sel "Enter"

  (* ---- scoping ---- *)
  let within sel f p = ignore (f { p with scope = scoped p sel }); p

  (* ---- waits / web-first assertions ---- *)
  let wait_visible sel p = let s = scoped p sel in wait_cond p ~label:(Printf.sprintf "wait_visible %s" (q s)) (Cond.Visible s)
  let wait_hidden sel p = let s = scoped p sel in wait_cond p ~label:(Printf.sprintf "wait_hidden %s" (q s)) (Cond.Hidden s)
  let expect_visible = wait_visible
  let expect_hidden = wait_hidden
  let expect_present sel p = let s = scoped p sel in wait_cond p ~label:(Printf.sprintf "expect_present %s" (q s)) (Cond.Present s)
  let expect_detached sel p = let s = scoped p sel in wait_cond p ~label:(Printf.sprintf "expect_detached %s" (q s)) (Cond.Detached s)
  let expect_text sel t p = let s = scoped p sel in wait_cond p ~label:(Printf.sprintf "expect_text %s %s" (q s) (q (trunc 40 t))) (Cond.Text (s, t))
  let expect_value sel v p = let s = scoped p sel in wait_cond p ~label:(Printf.sprintf "expect_value %s %s" (q s) (q (trunc 40 v))) (Cond.Value (s, v))
  let expect_attr sel n v p = let s = scoped p sel in wait_cond p ~label:(Printf.sprintf "expect_attr %s [%s]=%s" (q s) n (q v)) (Cond.Attr (s, n, v))
  let expect_count sel n p = let s = scoped p sel in wait_cond p ~label:(Printf.sprintf "expect_count %s %d" (q s) n) (Cond.Count (s, n))
  let expect_url sub p = wait_cond p ~label:(Printf.sprintf "expect_url %s" (q sub)) (Cond.Url sub)
  let expect_js ?(descr = "expect_js") expr p = wait_cond p ~label:(Printf.sprintf "%s %s" descr (q (trunc 50 expr))) (Cond.Js expr)
  let wait_for ?(descr = "wait_for") expr p = wait_cond p ~label:(Printf.sprintf "%s %s" descr (q (trunc 50 expr))) (Cond.Js expr)

  (* ---- reads (terminal in a pipe) ---- *)
  let read_text sel p =
    let s = scoped p sel in
    ignore (wait_cond p ~label:(Printf.sprintf "read_text %s" (q s)) (Cond.Present s));
    Option.value ~default:"" (B.read_text p.backend ~selector:s)
  let read_count sel p = B.read_count p.backend ~selector:(scoped p sel)
  let read_value sel p = B.read_value p.backend ~selector:(scoped p sel)
  let read_attr sel name p = B.read_attr p.backend ~selector:(scoped p sel) ~name
  let read_url p = B.current_url p.backend

  (* ---- escape hatches (best-effort; for assertions on JS use expect_js) ---- *)
  let eval js p = ignore (B.eval p.backend js); p
  let eval_get js p = B.eval p.backend js

  (* ===================================================================== runner ==== *)
  type test = { name : string; body : page -> unit }

  let registry : test list ref = ref []
  let test name body = registry := { name; body } :: !registry
  let registered () = List.rev !registry

  (* the runner's result/outcome/summary types live in {!Reporter} (they don't depend on the
     backend), re-exported here so callers keep using [D.result], [D.Failed_assert], etc. *)
  type outcome = Reporter.outcome = Passed | Failed_assert | Errored | Timed_out
  type result = Reporter.result = { name : string; outcome : outcome; ms : float; failure : Failure.t option }
  type report = Reporter.summary = { results : result list; passed : int; failed : int }

  type config = {
    jobs : int; retries : int; bail : bool; grep : string option;
    base_url : string; step_timeout : float; test_timeout : float;
  }
  let default_config =
    { jobs = 1; retries = 0; bail = false; grep = None; base_url = ""; step_timeout = 5.0; test_timeout = 30.0 }

  (* how to re-run just this test, copy-pasteable. Prefix from FENNEC_E2E_RERUN (wrappers set
     it, e.g. "sh examples/site/e2e/run.sh") else the executable name; the test name is
     single-quote-escaped so spaces/specials survive a shell. *)
  let shell_quote s = "'" ^ String.concat "'\\''" (String.split_on_char '\'' s) ^ "'"
  let rerun_for name =
    let prefix = match Sys.getenv_opt "FENNEC_E2E_RERUN" with Some p when p <> "" -> p | _ -> Filename.basename Sys.executable_name in
    Printf.sprintf "%s --grep %s" prefix (shell_quote name)

  (* one attempt: returns (failure kind option, trace). [provision] only has to run the body
     with a fresh backend and clean up — the outcome is captured in a ref, so [provision]
     stays the natural [(backend -> unit) -> unit] rather than threading our result type. *)
  let run_once ~clock ~config ~provision t : Failure.kind option * Failure.step list =
    let trace = ref [] and kind = ref None in
    (try
       provision (fun backend ->
           let page =
             { backend; now = (fun () -> Eio.Time.now clock); base_url = config.base_url;
               scope = ""; timeout = config.step_timeout; trace }
           in
           match Eio.Time.with_timeout clock config.test_timeout (fun () -> t.body page; Ok ()) with
           | Ok () -> ()
           | Error `Timeout -> kind := Some (Failure.Timed_out config.test_timeout))
     with
     | Step_failed -> kind := Some Failure.Assertion
     | e -> kind := Some (Failure.Errored (Printexc.to_string e)));
    (!kind, List.rev !trace)

  let outcome_of_kind = function
    | Failure.Assertion -> Failed_assert
    | Failure.Errored _ -> Errored
    | Failure.Timed_out _ -> Timed_out

  let run_test ?reporter ~clock ~config ~provision (t : test) : result =
    (match reporter with Some rep -> Reporter.test_started rep t.name | None -> ());
    let t0 = Eio.Time.now clock in
    let rec attempt n =
      match run_once ~clock ~config ~provision t with
      | None, _ -> (None, [])
      | (Some _ as k), trace -> if n < config.retries then attempt (n + 1) else (k, trace)
    in
    let kind, trace = attempt 0 in
    let ms = (Eio.Time.now clock -. t0) *. 1000.0 in
    let outcome, failure =
      match kind with
      | None -> (Passed, None)
      | Some k -> (outcome_of_kind k, Some { Failure.test = t.name; trace; kind = k; rerun = rerun_for t.name })
    in
    let r = { name = t.name; outcome; ms; failure } in
    (match reporter with Some rep -> Reporter.test_finished rep r | None -> ());
    r

  let select config tests =
    match config.grep with None -> tests | Some g -> List.filter (fun (t : test) -> Cdp.contains t.name g) tests

  let run ?reporter ~clock ~config ~provision tests : report =
    let tests = select config tests in
    let results =
      if config.bail then begin
        let rec go = function
          | [] -> []
          | t :: rest ->
            let r = run_test ?reporter ~clock ~config ~provision t in
            if r.outcome = Passed then r :: go rest else [ r ]
        in
        go tests
      end
      else begin
        let arr = Array.of_list tests in
        let out = Array.make (Array.length arr) None in
        let sem = Eio.Semaphore.make (max 1 config.jobs) in
        Eio.Fiber.all
          (Array.to_list
             (Array.mapi
                (fun i t () ->
                  Eio.Semaphore.acquire sem;
                  Fun.protect ~finally:(fun () -> Eio.Semaphore.release sem)
                    (fun () -> out.(i) <- Some (run_test ?reporter ~clock ~config ~provision t)))
                arr));
        Array.to_list out |> List.filter_map Fun.id
      end
    in
    let passed = List.length (List.filter (fun r -> r.outcome = Passed) results) in
    { results; passed; failed = List.length results - passed }
end
