(* Supervisor-side client for the resident warm test worker (cli/dev/worker/fennec_test_worker.ml).

   It owns the worker's lifecycle and the wire: locate + spawn the bytecode binary, handshake, send a
   Dynlink chain and read the captured result back, health-check, and shut it down. It also owns the
   FALLBACK DECISION — every public entry point returns an option/None on any failure (worker missing,
   spawn failed, socket error, malformed reply) so the caller can drop to the cold [Dev_tests.run_one]
   path. A worker absence or crash must be invisible except for speed.

   Pure protocol (encode/decode of the RUN request + RESULT reply) is split out and unit-tested; the
   rest is IO. The wire format mirrors the worker binary's exactly (see its header). *)

let starts_with ~prefix s =
  String.length s >= String.length prefix && String.sub s 0 (String.length prefix) = prefix

(* ── the framework the worker preloads (mirrors cli/dev/worker/dune) ──────────────────────────────
   These are the PROJECT-LOCAL framework lib names linked into the worker with -linkall. Test_chain
   uses this set to decide warm-vs-cold: a test whose closure needs a project framework lib NOT here
   can't be warm-pathed. Kept in lockstep with cli/dev/worker/dune by hand (the two lists are short and
   change together); the inline tests below assert the core fur SSR floor is present. (opam deps are
   not listed — they ride in transitively; see Test_chain.) *)
let preloaded =
  [ "fennec";
    "fennec-paw";
    "fennec-mongo.bson";
    "fennec-mongo.bson_json";
    "fennec-mongo.driver";
    "fennec-mongo.query";
    "fennec-mongo.minimongo";
    "fennec-mongo.burrow";
    "fennec-mongo.backend";
    "fennec-mongo.dynamic";
    "fennec-mongo.json";
    "fennec-mongo.wire";
    "fennec-mongo.burrow.binary";
    "fennec-mongo.burrow.codec";
    "fennec-mongo.burrow.lmdb";
    "fennec-mongo.burrow.store";
    "fennec-mongo.ffi";
    "fennec.accounts";
    "fennec.accounts.client";
    "fennec.accounts.features";
    "fennec.accounts.primitives";
    "fennec.ddp";
    "fennec.mail";
    "fennec.web";
    "fennec.fur";
    "fennec.fur.form";
    "fennec.fur.handler";
    "fennec.fur.html";
    "fennec.fur.platform";
    "fennec.fur.server";
    "fennec.pulse.live";
    "fennec.pulse.live.client";
    "fennec.pulse.method";
    "fennec.pulse.sift";
    "fennec.pulse.sift.mongo";
    "fennec-hunt.unit";
    "fennec-hunt.prop" ]

(* ── result of one warm run ──────────────────────────────────────────────────────────────────────
   No pass/fail tally here — that parsing belongs to the caller ({!Dev_tests}), which already does it
   for the cold path; this module is pure transport. *)
type result = {
  exit_code : int;   (* the forked child's exit code: 0 = the test runner succeeded *)
  output : string;   (* the child's combined stdout+stderr (timing sentinel already stripped) *)
  total_ms : float;  (* fork → reap, measured by the worker's parent *)
  dynlink_ms : float;(* time to Dynlink the framework .cma's in the child *)
  run_ms : float;    (* time to load the test .cmo (the test run) in the child *)
}

(* ── wire encode/decode (pure — unit-tested) ─────────────────────────────────────────────────────*)

(* a RUN request: "RUN\n" then one object path per line, then a blank line *)
let encode_run (chain : string list) = "RUN\n" ^ String.concat "\n" chain ^ "\n\n"

(* parse a "RESULT <child_exit> <total_us> <dynlink_us> <run_us> <out_len>" header line *)
let decode_result_header line =
  match String.split_on_char ' ' (String.trim line) with
  | [ "RESULT"; ex; tot; dyn; run; len ] -> (
    match (int_of_string_opt ex, float_of_string_opt tot, float_of_string_opt dyn, float_of_string_opt run, int_of_string_opt len) with
    | Some ex, Some tot, Some dyn, Some run, Some len -> Some (ex, tot, dyn, run, len)
    | _ -> None)
  | _ -> None

(* ── the live worker handle ──────────────────────────────────────────────────────────────────────*)

type t = {
  pid : int;
  socket_path : string;
  mutable conn : Unix.file_descr option;  (* one persistent connection; reopened on demand *)
  mutable dead : bool;
}

let pid t = t.pid

(* ── locating the worker binary ──────────────────────────────────────────────────────────────────
   dev-only + NOT installed (it is a 26 MB bytecode image), so resolve it without assuming an install:
     1. $FENNEC_TEST_WORKER (explicit override / tests),
     2. <build-context>/cli/dev/worker/fennec_test_worker.bc — the fennec repo dogfood, in the active
        per-profile build dir (so it matches, CRC-wise, the app .cma's the watch builds there),
     3. next to the running fennec exe (if a build ever colocates it).
   None of these existing ⇒ no warm path; the caller stays on the cold path. *)
let candidate_paths ~root =
  let by_env = match Sys.getenv_opt "FENNEC_TEST_WORKER" with Some p when p <> "" -> [ p ] | _ -> [] in
  let in_build = Filename.concat (Build_dir.context_dir ~root) "cli/dev/worker/fennec_test_worker.bc" in
  let near_exe = Filename.concat (Filename.dirname Sys.executable_name) "fennec_test_worker.bc" in
  by_env @ [ in_build; near_exe ]

let find_worker_exe ~root = List.find_opt Sys.file_exists (candidate_paths ~root)

(* a short, unique unix-socket path (socket paths cap ~100 bytes) *)
let tmp_socket () =
  let name = Printf.sprintf "fennec-tw-%d.sock" (Unix.getpid ()) in
  let p = Filename.concat (Filename.get_temp_dir_name ()) name in
  if String.length p <= 100 then p else Filename.concat "/tmp" name

(* ── blocking line / body IO over the connection ─────────────────────────────────────────────────*)

let write_all fd s =
  let n = String.length s in
  let rec go off = if off < n then go (off + Unix.write_substring fd s off (n - off)) in
  go 0

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

let read_exactly fd len =
  let b = Bytes.create len in
  let rec go off =
    if off >= len then Some (Bytes.unsafe_to_string b)
    else
      match Unix.read fd b off (len - off) with
      | 0 -> None (* short read: connection closed early *)
      | n -> go (off + n)
      | exception Unix.Unix_error (Unix.EINTR, _, _) -> go off
  in
  go 0

(* ── connect (with a short retry while the worker binds its socket) ───────────────────────────────*)

let try_connect socket_path ~timeout =
  let deadline = Unix.gettimeofday () +. timeout in
  let rec go () =
    if not (Sys.file_exists socket_path) then
      if Unix.gettimeofday () < deadline then (Unix.sleepf 0.02; go ()) else None
    else
      match
        let fd = Unix.socket Unix.PF_UNIX Unix.SOCK_STREAM 0 in
        (try Unix.connect fd (Unix.ADDR_UNIX socket_path); Some fd
         with e -> (try Unix.close fd with _ -> ()); raise e)
      with
      | Some fd -> Some fd
      | None -> None
      | exception _ -> if Unix.gettimeofday () < deadline then (Unix.sleepf 0.02; go ()) else None
  in
  go ()

let conn_of t =
  match t.conn with
  | Some fd -> Some fd
  | None ->
    (match try_connect t.socket_path ~timeout:1.0 with
     | Some fd -> t.conn <- Some fd; Some fd
     | None -> None)

let drop_conn t = match t.conn with Some fd -> (try Unix.close fd with _ -> ()); t.conn <- None | None -> ()

(* ── spawn + handshake ───────────────────────────────────────────────────────────────────────────
   [Stublibs.ensure ()] must have run already (the worker dlopens the project's C stubs via
   CAML_LD_LIBRARY_PATH, which spawned children inherit). The worker is bytecode, so it is run through
   ocamlrun implicitly by its shebang/header — Unix.create_process on the .bc works directly. *)
let spawn ?worker_exe ~root () =
  (* writing to the worker's socket after it dies (e.g. a SHUTDOWN/PING racing its exit) would deliver
     SIGPIPE and kill US; ignore it so those writes fail as catchable EPIPE → fallback instead. *)
  (try Sys.set_signal Sys.sigpipe Sys.Signal_ignore with _ -> ());
  let exe = match worker_exe with Some e -> Some e | None -> find_worker_exe ~root in
  match exe with
  | None -> None
  | Some exe ->
    let socket_path = tmp_socket () in
    (try Unix.unlink socket_path with _ -> ());
    (match
       (* The worker's own stdout/stderr is internal diagnostics ("listening on …", per-run timing) that
          duplicates the [Ui.tested] line — pure noise on the dev terminal, where it leaked raw before.
          Send it to /dev/null (readiness is detected by the socket + PING/PONG below, NOT these lines);
          set FENNEC_DEV_DEBUG=1 to see it when debugging the worker. stdin is always /dev/null. *)
       let devnull_in = Unix.openfile "/dev/null" [ Unix.O_RDONLY ] 0 in
       let debug = match Sys.getenv_opt "FENNEC_DEV_DEBUG" with Some ("1" | "on" | "true" | "yes") -> true | _ -> false in
       let log = if debug then Unix.stderr else Unix.openfile "/dev/null" [ Unix.O_WRONLY ] 0 in
       let pid = Unix.create_process exe [| exe; socket_path |] devnull_in log log in
       (try Unix.close devnull_in with _ -> ());
       (if not debug then try Unix.close log with _ -> ());
       pid
     with
     | exception _ -> None
     | pid ->
       let t = { pid; socket_path; conn = None; dead = false } in
       (* handshake: wait for the socket + a PONG. give the heavy -linkall image time to boot. *)
       (match try_connect socket_path ~timeout:8.0 with
        | None -> (try Unix.kill pid Sys.sigterm with _ -> ()); None
        | Some fd ->
          t.conn <- Some fd;
          (try
             write_all fd "PING\n";
             match read_line_fd fd with
             | Some "PONG" -> Some t
             | _ -> drop_conn t; (try Unix.kill pid Sys.sigterm with _ -> ()); None
           with _ -> drop_conn t; (try Unix.kill pid Sys.sigterm with _ -> ()); None)))

(* ── health ──────────────────────────────────────────────────────────────────────────────────────*)

(* alive AND answering PING (a wedged worker counts as dead → caller falls back) *)
let healthy t =
  if t.dead then false
  else
    (* reap check: did the worker process exit? *)
    match (try Unix.waitpid [ Unix.WNOHANG ] t.pid with _ -> (0, Unix.WEXITED 0)) with
    | 0, _ -> (
      match conn_of t with
      | None -> t.dead <- true; false
      | Some fd -> (
        try
          write_all fd "PING\n";
          match read_line_fd fd with Some "PONG" -> true | _ -> drop_conn t; false
        with _ -> drop_conn t; false))
    | _ -> t.dead <- true; false

(* ── run a chain ─────────────────────────────────────────────────────────────────────────────────
   Returns None on ANY transport failure (so the caller falls back); the worker child's own non-zero
   exit is a SUCCESSFUL transport returning a result with [exit_code <> 0] (a real test failure, not a
   fallback case). *)
let run t ~chain =
  if t.dead then None
  else
    match conn_of t with
    | None -> None
    | Some fd -> (
      try
        write_all fd (encode_run chain);
        match read_line_fd fd with
        | None -> drop_conn t; None
        | Some header -> (
          match decode_result_header header with
          | None -> drop_conn t; None
          | Some (exit_code, total_us, dynlink_us, run_us, out_len) -> (
            match read_exactly fd out_len with
            | None -> drop_conn t; None
            | Some output ->
              Some
                { exit_code;
                  output;
                  total_ms = total_us /. 1000.;
                  dynlink_ms = dynlink_us /. 1000.;
                  run_ms = run_us /. 1000. }))
      with _ -> drop_conn t; None)

(* ── shutdown ────────────────────────────────────────────────────────────────────────────────────*)

let shutdown t =
  t.dead <- true;
  (match conn_of t with
   | Some fd -> ( try write_all fd "SHUTDOWN\n"; ignore (read_line_fd fd) with _ -> ())
   | None -> ());
  drop_conn t;
  (try Unix.kill t.pid Sys.sigterm with _ -> ());
  (try ignore (Unix.waitpid [] t.pid) with _ -> ());
  (try Sys.remove t.socket_path with _ -> ())

(* ═══════════════════════════════════════════════════════════════════════════ *)
(*  Tests — pure wire encode/decode                                          *)
(* ═══════════════════════════════════════════════════════════════════════════ *)

let%test "encode_run frames the chain with a trailing blank line" =
  encode_run [ "a.cma"; "b.cmo" ] = "RUN\na.cma\nb.cmo\n\n"

let%test "encode_run of an empty chain still terminates on a blank line" =
  (* RUN, an empty body line, then the blank terminator — the worker's read_chain stops at the first
     blank line ⇒ empty chain. (Not a real case: a chain always has at least the test .cmo.) *)
  encode_run [] = "RUN\n\n\n"

let%test "decode_result_header parses a well-formed header" =
  match decode_result_header "RESULT 0 23000 17000 4000 1899" with
  | Some (0, t, d, r, l) -> t = 23000. && d = 17000. && r = 4000. && l = 1899
  | _ -> false

let%test "decode_result_header reads a non-zero child exit (a real test failure, not a transport miss)" =
  match decode_result_header "RESULT 4 9000 5000 3000 42" with Some (4, _, _, _, 42) -> true | _ -> false

let%test "decode_result_header rejects a malformed header" =
  decode_result_header "RESULT garbage" = None && decode_result_header "PONG" = None

let%test "decode_result_header tolerates a trailing newline" =
  decode_result_header "RESULT 0 1 2 3 4\n" <> None

let%test "candidate_paths includes the in-build worker under the active build context dir" =
  let ps = candidate_paths ~root:"/proj" in
  let expect = Filename.concat (Build_dir.context_dir ~root:"/proj") "cli/dev/worker/" in
  List.exists (starts_with ~prefix:expect) ps

let%test "candidate_paths honours $FENNEC_TEST_WORKER first" =
  let key = "FENNEC_TEST_WORKER" in
  let saved = Sys.getenv_opt key in
  Unix.putenv key "/custom/worker.bc";
  let ps = candidate_paths ~root:"/proj" in
  (match saved with Some v -> Unix.putenv key v | None -> Unix.putenv key "");
  (match ps with first :: _ -> first = "/custom/worker.bc" | [] -> false)

(* the preload set must be a SUPERSET of the example's project-framework closure; the fennec.fur stack
   is the floor any fur SSR test needs *)
let%test "preloaded carries the core fur SSR framework libs" =
  List.for_all (fun l -> List.mem l preloaded)
    [ "fennec.fur"; "fennec.fur.server"; "fennec.pulse.sift"; "fennec.accounts.primitives"; "fennec-hunt.unit" ]
