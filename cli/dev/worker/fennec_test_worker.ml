(* The resident WARM TEST WORKER — the dev loop's per-edit test runner.

   It pays the macOS "first-exec of a new Mach-O" tax (dyld + code-sign validation, ~320-450ms) ONCE,
   at dev boot, by linking the stable web-test framework into THIS bytecode binary with -linkall.
   Thereafter each edit costs a fork + a handful of Dynlink.loadfile calls + the test run — measured at
   ~8-9ms, ~40-50x faster than relinking and cold-launching a native test exe every save.

   Why fork per run: OCaml refuses to load a module name twice in one process, so a changed module
   could never be re-Dynlinked into a long-lived worker. Forking gives each run a fresh address space,
   so v1 then v2 of the same module load with no conflict. The framework is already in the (copied,
   COW) image, so the child resolves the app's .cma/.cmo against it for free.

   Protocol (newline-framed, Stdlib-only — mirrored by the supervisor-side Test_worker client):
     PING\n                        -> PONG\n
     SHUTDOWN\n                    -> (worker exits 0)
     RUN\n <path>\n … \n (blank)   -> RESULT <child_exit> <total_us> <dynlink_us> <run_us> <out_len>\n
                                      then <out_len> bytes of the child's combined stdout+stderr.
   The chain is the object list from Test_chain.objects: the app .cma's (dependency order) then the
   test .cmo. The LAST object is the test module — loading it runs the inline-test runner — so the
   child times "load all but the last" (pure framework-cma Dynlink) vs "load the last" (the test run)
   to give an honest dynlink-vs-run split; the parent times the whole fork→reap for [total_us]. *)

let () = Dynlink.allow_unsafe_modules true (* the framework .cma's were built without -unsafe guards *)

(* Sentinel child-exit for "the chain could not be LOADED" (a Dynlink CRC/interface mismatch — e.g.
   the worker's preloaded framework was built in a different profile than the app .cma's, or a new
   opam dep the worker lacks). The supervisor maps this exact code to a COLD fallback rather than a
   test failure, so a load problem never shows up as a false red test. A real test that happens to
   exit 75 simply also falls back to cold (which re-runs it correctly) — safe either way. *)
let load_failed_exit = 75

(* Never outlive the dev supervisor (our direct parent): if `fennec dev` dies — even by SIGKILL, which
   it can't clean up after, and with no next run to reap us — we'd otherwise linger forever holding the
   socket. We capture the parent pid at startup and, between requests, exit the moment getppid() stops
   matching it (a reparent to init/launchd/a subreaper). Mirrors the server's parent-watch
   (fennec/app/fennec.ml); immune to pid recycling (getppid is our real current parent). Polled while
   IDLE in BOTH the accept loop AND the per-connection request loop — the persistent client connection
   may not EOF on the supervisor's death (a sibling can inherit the fd), so a select-timeout poll, not
   the EOF, is what guarantees we go. *)
let parent = Unix.getppid ()
let parent_gone () = Unix.getppid () <> parent

(* ── tiny IO helpers (blocking, Stdlib + Unix only) ──────────────────────────────────────────── *)

let write_all fd s =
  let n = String.length s in
  let rec go off = if off < n then go (off + Unix.write_substring fd s off (n - off)) in
  go 0

(* read one '\n'-terminated line from [fd] (returns without the newline); None at EOF *)
let read_line_fd fd =
  let b = Buffer.create 128 in
  let one = Bytes.create 1 in
  let rec go () =
    match Unix.read fd one 0 1 with
    | 0 -> if Buffer.length b = 0 then None else Some (Buffer.contents b)
    | _ -> if Bytes.get one 0 = '\n' then Some (Buffer.contents b) else (Buffer.add_char b (Bytes.get one 0); go ())
    | exception Unix.Unix_error (Unix.EINTR, _, _) -> go ()
  in
  go ()

(* drain a pipe read-end fully into a string *)
let read_to_eof fd =
  let b = Buffer.create 4096 in
  let chunk = Bytes.create 65536 in
  let rec go () =
    match Unix.read fd chunk 0 (Bytes.length chunk) with
    | 0 -> Buffer.contents b
    | n -> Buffer.add_subbytes b chunk 0 n; go ()
    | exception Unix.Unix_error (Unix.EINTR, _, _) -> go ()
  in
  go ()

let now_us () = Unix.gettimeofday () *. 1_000_000.0

(* the child writes its dynlink/run split on this sentinel line so the parent can strip it back out of
   the captured output. \x00 can't appear in test text, so the split is unambiguous. *)
let timing_prefix = "\x00FENNEC_TW_TIMING "

(* ── timing sentinel: split the child's [dynlink_us]/[run_us] back out of its captured output ───── *)

let starts_with ~prefix s =
  String.length s >= String.length prefix && String.sub s 0 (String.length prefix) = prefix

(* parse "key=NNN" out of a space-separated sentinel line *)
let parse_kv line key =
  String.split_on_char ' ' line
  |> List.find_map (fun t ->
         match String.index_opt t '=' with
         | Some i when String.sub t 0 i = key -> float_of_string_opt (String.sub t (i + 1) (String.length t - i - 1))
         | _ -> None)
  |> Option.value ~default:0.0

(* find the sentinel (the unique line starting with [timing_prefix]) anywhere in [raw], parse its two
   values, and return [raw] with exactly that line removed. Robust to trailing newlines / empty lines
   that [String.split_on_char] leaves behind. *)
let strip_timing raw =
  let lines = String.split_on_char '\n' raw in
  let sentinel = List.find_opt (starts_with ~prefix:timing_prefix) lines in
  match sentinel with
  | None -> (0.0, 0.0, raw)
  | Some s ->
    let kept = List.filter (fun l -> not (starts_with ~prefix:timing_prefix l)) lines in
    (* drop a single trailing empty element (the newline after the last real line) so we don't append
       a spurious blank line to the test output *)
    let kept = match List.rev kept with "" :: rest -> List.rev rest | _ -> kept in
    (parse_kv s "dynlink_us", parse_kv s "run_us", String.concat "\n" kept)

(* ── run one chain in a fresh fork, capturing output + timings ────────────────────────────────── *)

type run = { child_exit : int; total_us : float; dynlink_us : float; run_us : float; output : string }

let run_chain (chain : string list) : run =
  let r_out, w_out = Unix.pipe ~cloexec:false () in
  let t0 = now_us () in
  match Unix.fork () with
  | 0 ->
    (* CHILD: redirect stdout+stderr into the pipe, Dynlink the chain, run, report timing, exit. *)
    Unix.close r_out;
    Unix.dup2 w_out Unix.stdout;
    Unix.dup2 w_out Unix.stderr;
    Unix.close w_out;
    let load_one p = Dynlink.loadfile p in
    (* split: all-but-last = framework-cma dynlink; last = the test module load (runs the tests) *)
    let rec split acc = function [ x ] -> (List.rev acc, Some x) | x :: xs -> split (x :: acc) xs | [] -> (List.rev acc, None) in
    let cmas, test = split [] chain in
    let code = ref 0 in
    let td0 = now_us () in
    (try List.iter load_one cmas
     with
     (* a chain that won't LOAD ⇒ load-failed sentinel ⇒ the supervisor retries cold (not a red test) *)
     | Dynlink.Error e -> Printf.eprintf "dynlink error (framework): %s\n%!" (Dynlink.error_message e); code := load_failed_exit
     | e -> Printf.eprintf "exception loading framework chain: %s\n%!" (Printexc.to_string e); code := load_failed_exit);
    let td1 = now_us () in
    let tr1 =
      if !code = 0 then (
        (match test with
         | Some p -> (
           try load_one p
           with
           (* the test .cmo not loading is also a load problem ⇒ fall back cold, not a red test *)
           | Dynlink.Error e -> Printf.eprintf "dynlink error (test): %s\n%!" (Dynlink.error_message e); code := load_failed_exit
           (* the test LOADED but raised while running ⇒ a genuine test error (4), reported as failed *)
           | e -> Printf.eprintf "exception running test: %s\n%!" (Printexc.to_string e); code := 4)
         | None -> ());
        now_us ())
      else td1
    in
    (* emit the timing sentinel last, so the parent strips exactly this trailing line *)
    Printf.printf "%sdynlink_us=%.0f run_us=%.0f\n%!" timing_prefix (td1 -. td0) (tr1 -. td1);
    (try Unix.close Unix.stdout with _ -> ());
    exit !code
  | pid ->
    Unix.close w_out;
    let raw = read_to_eof r_out in
    (try Unix.close r_out with _ -> ());
    let _, status = Unix.waitpid [] pid in
    let total_us = now_us () -. t0 in
    let child_exit = match status with Unix.WEXITED n -> n | Unix.WSIGNALED n -> 128 + n | Unix.WSTOPPED n -> 128 + n in
    let dynlink_us, run_us, output = strip_timing raw in
    { child_exit; total_us; dynlink_us; run_us; output }

(* ── request handling on one accepted connection ─────────────────────────────────────────────── *)

(* read a RUN chain: lines until a blank line (or EOF) *)
let read_chain fd =
  let rec go acc =
    match read_line_fd fd with
    | None | Some "" -> List.rev acc
    | Some p -> go (p :: acc)
  in
  go []

let handle_conn fd =
  let respond_result (r : run) =
    let header =
      Printf.sprintf "RESULT %d %.0f %.0f %.0f %d\n" r.child_exit r.total_us r.dynlink_us r.run_us
        (String.length r.output)
    in
    write_all fd header;
    write_all fd r.output
  in
  let rec loop () =
    (* poll for the supervisor's death between requests (the connection may never EOF), so we never
       outlive `fennec dev` even while parked on a persistent connection *)
    match Unix.select [ fd ] [] [] 0.25 with
    | exception Unix.Unix_error (Unix.EINTR, _, _) -> loop ()
    | [], _, _ -> if parent_gone () then exit 0 else loop ()
    | _ -> (
      match read_line_fd fd with
      | None -> () (* client hung up *)
      | Some "PING" -> write_all fd "PONG\n"; loop ()
      | Some "SHUTDOWN" -> write_all fd "BYE\n"; exit 0
      | Some "RUN" ->
        let chain = read_chain fd in
        let r = run_chain chain in
        (* the worker's own structured log line (stderr → the dev log) *)
        Printf.eprintf "[fennec-test-worker] run: exit=%d total=%.1fms dynlink=%.1fms run=%.1fms objs=%d\n%!"
          r.child_exit (r.total_us /. 1000.) (r.dynlink_us /. 1000.) (r.run_us /. 1000.) (List.length chain);
        respond_result r;
        loop ()
      | Some other -> write_all fd (Printf.sprintf "ERR unknown request %S\n" other); loop ())
  in
  loop ()

(* ── server loop ─────────────────────────────────────────────────────────────────────────────── *)

let () =
  let socket_path =
    match Sys.argv with
    | [| _; p |] -> p
    | _ -> prerr_endline "usage: fennec_test_worker <unix-socket-path>"; exit 64
  in
  (* don't die when a client disconnects mid-write *)
  (try Sys.set_signal Sys.sigpipe Sys.Signal_ignore with _ -> ());
  (try Unix.unlink socket_path with _ -> ());
  let srv = Unix.socket Unix.PF_UNIX Unix.SOCK_STREAM 0 in
  Unix.bind srv (Unix.ADDR_UNIX socket_path);
  Unix.listen srv 8;
  (* announce readiness so the supervisor's handshake can proceed deterministically *)
  Printf.eprintf "[fennec-test-worker] listening on %s\n%!" socket_path;
  (* the accept is bounded by a short select so the parent-death check (see [parent_gone]) runs even
     while no client is connected — we never outlive the supervisor that spawned us *)
  let rec accept_loop () =
    match Unix.select [ srv ] [] [] 0.25 with
    | exception Unix.Unix_error (Unix.EINTR, _, _) -> accept_loop ()
    | [], _, _ -> if parent_gone () then exit 0 else accept_loop () (* idle tick: parent alive? *)
    | _ -> (
      match Unix.accept srv with
      | fd, _ ->
        (try handle_conn fd with _ -> ());
        (try Unix.close fd with _ -> ());
        accept_loop ()
      | exception Unix.Unix_error (Unix.EINTR, _, _) -> accept_loop ())
  in
  accept_loop ()
