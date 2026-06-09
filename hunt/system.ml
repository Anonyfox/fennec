(* System tests — process + filesystem + port operations, contained and deterministic.

   Each scenario runs in a sandbox: a temp workdir under an Eio.Switch. Every spawned process
   is put in its own session (a tiny re-exec through THIS binary that calls setsid then execvp,
   the same argv-dispatch trick fennec uses for its esbuild worker), so on teardown a single
   kill(-pgid) reaps the whole tree — orphans are impossible even if the tool under test leaks
   one. Output is captured to a file (no pipe deadlock). Every wait is deadline-bounded. *)

exception Timeout of string

(* the internal re-exec sentinel: [main] intercepts argv [_; sentinel; prog; args…] *)
let sentinel = "__fennec_hunt_exec__"

(* ════════════════════════════════════════════════════════════════════════════ *)
(*  Output styling (shared caps detection)                                       *)
(* ════════════════════════════════════════════════════════════════════════════ *)

let caps = lazy (Reporter.detect_caps ())
let color code s = if (Lazy.force caps).Reporter.color then "\027[" ^ code ^ "m" ^ s ^ "\027[0m" else s
let glyph uni ascii = if (Lazy.force caps).Reporter.unicode then uni else ascii

(* ════════════════════════════════════════════════════════════════════════════ *)
(*  Types                                                                         *)
(* ════════════════════════════════════════════════════════════════════════════ *)

type sandbox = {
  dir : string;                    (* absolute temp workdir *)
  env : Eio_unix.Stdenv.base;      (* the Eio environment *)
  sw : Eio.Switch.t;               (* scenario switch — all procs die on release *)
  mutable ports : int list;        (* ports handed out by [free_port] *)
}

(* the Eio process handle is captured only inside the status daemon, so the record stays
   simple: a pid (group leader, since we setsid), its log file, and its exit status *)
type proc = {
  pid : int;
  logpath : string;
  sb : sandbox;
  st : Unix.process_status option ref;
}

type result = { status : Unix.process_status; output : string; ms : float }
type response = { status : int; headers : (string * string) list; body : string }

(* ════════════════════════════════════════════════════════════════════════════ *)
(*  Ambient env + tally (set by [main])                                          *)
(* ════════════════════════════════════════════════════════════════════════════ *)

let ambient : Eio_unix.Stdenv.base option ref = ref None
let env_exn () = match !ambient with Some e -> e | None -> failwith "Fennec_hunt.System: call System.main first"
let passed = ref 0 and failed = ref 0
let ids = ref 0
let next_id () = incr ids; !ids

let now sb = Eio.Time.now (Eio.Stdenv.clock sb.env)
let fs sb = Eio.Stdenv.fs sb.env
(* sandbox-relative by default; an absolute path is used as-is (to read/edit real project files) *)
let p_of sb rel = Eio.Path.(fs sb / (if Filename.is_relative rel then Filename.concat sb.dir rel else rel))

(* ════════════════════════════════════════════════════════════════════════════ *)
(*  Ports                                                                         *)
(* ════════════════════════════════════════════════════════════════════════════ *)

let port_open port =
  match Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 with
  | fd ->
    Fun.protect
      ~finally:(fun () -> try Unix.close fd with _ -> ())
      (fun () -> try Unix.connect fd (Unix.ADDR_INET (Unix.inet_addr_loopback, port)); true with _ -> false)
  | exception _ -> false

let free_port sb =
  let fd = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
  Fun.protect
    ~finally:(fun () -> try Unix.close fd with _ -> ())
    (fun () ->
      Unix.setsockopt fd Unix.SO_REUSEADDR true;
      Unix.bind fd (Unix.ADDR_INET (Unix.inet_addr_loopback, 0));
      match Unix.getsockname fd with
      | Unix.ADDR_INET (_, p) -> sb.ports <- p :: sb.ports; p
      | _ -> failwith "free_port")

let wait_until ?(timeout = 10.0) cond =
  let clock = Eio.Stdenv.clock (env_exn ()) in
  let deadline = Eio.Time.now clock +. timeout in
  let rec loop () =
    if cond () then ()
    else if Eio.Time.now clock > deadline then raise (Timeout (Printf.sprintf "condition not met within %.0fs" timeout))
    else (Eio.Time.sleep clock 0.05; loop ())
  in
  loop ()

let wait_port ?(timeout = 10.0) port =
  try wait_until ~timeout (fun () -> port_open port)
  with Timeout _ -> raise (Timeout (Printf.sprintf "nothing listening on :%d after %.0fs" port timeout))

(* ════════════════════════════════════════════════════════════════════════════ *)
(*  Filesystem (sandbox-relative, real)                                          *)
(* ════════════════════════════════════════════════════════════════════════════ *)

let write sb rel content =
  (match Filename.dirname rel with
   | "." | "" -> ()
   | d -> (try Eio.Path.mkdirs ~exists_ok:true ~perm:0o755 Eio.Path.(fs sb / Filename.concat sb.dir d) with _ -> ()));
  Eio.Path.save ~create:(`Or_truncate 0o644) (p_of sb rel) content

let read sb rel = Eio.Path.load (p_of sb rel)
let exists sb rel = match Eio.Path.kind ~follow:true (p_of sb rel) with `Not_found -> false | _ -> true
let rm sb rel = (try Eio.Path.rmtree ~missing_ok:true (p_of sb rel) with _ -> (try Eio.Path.unlink (p_of sb rel) with _ -> ()))

(* edit a real file under test, restoring its original content on ANY exit (Fun.protect) *)
let with_edit sb path transform f =
  let original = read sb path in
  Fun.protect
    ~finally:(fun () -> try write sb path original with _ -> ())
    (fun () -> write sb path (transform original); f ())

(* ════════════════════════════════════════════════════════════════════════════ *)
(*  HTTP (reuses hunt's raw client)                                               *)
(* ════════════════════════════════════════════════════════════════════════════ *)

let request ?host ?(headers = []) ?(meth = "GET") ?body port path =
  let net = Eio.Stdenv.net (env_exn ()) in
  let headers = match host with Some h -> ("Host", h) :: headers | None -> headers in
  let r = Http_client.request ~net ~host:"localhost" ~port ~meth ~path ~headers ?body () in
  { status = r.Http_client.status; headers = r.Http_client.headers; body = r.Http_client.body }

let header resp name =
  List.find_map (fun (k, v) -> if String.lowercase_ascii k = String.lowercase_ascii name then Some v else None) resp.headers

(* ════════════════════════════════════════════════════════════════════════════ *)
(*  Processes                                                                     *)
(* ════════════════════════════════════════════════════════════════════════════ *)

let eio_status = function `Exited n -> Unix.WEXITED n | `Signaled n -> Unix.WSIGNALED n

let output p = try Eio.Path.load Eio.Path.(fs p.sb / p.logpath) with _ -> ""
let pid p = p.pid
let alive p = !(p.st) = None

let spawn sb ?(env = []) ?cwd argv =
  let logpath = Filename.concat sb.dir (Printf.sprintf ".proc-%d.log" (next_id ())) in
  let mgr = Eio.Stdenv.process_mgr sb.env in
  let sink = Eio.Path.open_out ~sw:sb.sw ~create:(`If_missing 0o644) Eio.Path.(fs sb / logpath) in
  let nullsrc = Eio.Path.open_in ~sw:sb.sw Eio.Path.(fs sb / "/dev/null") in
  let full_env =
    Array.append (Unix.environment ()) (Array.of_list (List.map (fun (k, v) -> k ^ "=" ^ v) env))
  in
  let cwd = Eio.Path.(fs sb / Option.value cwd ~default:sb.dir) in
  (* re-exec through ourselves so the child setsid's into its own session before exec *)
  let wrapped = Sys.executable_name :: sentinel :: argv in
  let handle =
    Eio.Process.spawn ~sw:sb.sw mgr ~cwd
      ~stdin:(nullsrc :> _ Eio.Flow.source)
      ~stdout:(sink :> _ Eio.Flow.sink)
      ~stderr:(sink :> _ Eio.Flow.sink)
      ~env:full_env wrapped
  in
  let pid = Eio.Process.pid handle in
  let st = ref None in
  (* a DAEMON fiber records the exit status (drives [alive] / [wait_exit]). It must be a daemon,
     not a plain fork: a plain fork would make Switch.run block teardown until the process exited
     naturally (it awaits the process), defeating the fast group-kill. A daemon is cancelled when
     the switch finishes, so teardown stays instant. *)
  Eio.Fiber.fork_daemon ~sw:sb.sw (fun () ->
      let s = try eio_status (Eio.Process.await handle) with _ -> Unix.WEXITED (-1) in
      st := Some s;
      `Stop_daemon);
  (* kill the whole process group on teardown — catches descendants Eio's leader-kill misses *)
  Eio.Switch.on_release sb.sw (fun () -> try Unix.kill (-pid) Sys.sigkill with _ -> ());
  { pid; logpath; sb; st }

let signal p s = (try Unix.kill p.pid s with _ -> ())

let wait_exit p ?(timeout = 30.0) () =
  let clock = Eio.Stdenv.clock p.sb.env in
  let deadline = Eio.Time.now clock +. timeout in
  let rec loop () =
    match !(p.st) with
    | Some s -> s
    | None ->
      if Eio.Time.now clock > deadline then raise (Timeout (Printf.sprintf "process %d did not exit within %.0fs" p.pid timeout))
      else (Eio.Time.sleep clock 0.02; loop ())
  in
  loop ()

let stop p =
  let clock = Eio.Stdenv.clock p.sb.env in
  (try Unix.kill (-p.pid) Sys.sigterm with _ -> ());
  let deadline = Eio.Time.now clock +. 2.0 in
  while alive p && Eio.Time.now clock < deadline do Eio.Time.sleep clock 0.02 done;
  if alive p then ((try Unix.kill (-p.pid) Sys.sigkill with _ -> ()); ignore (wait_exit p ~timeout:2.0 () : Unix.process_status))

let run_cmd sb ?env ?cwd argv =
  let p = spawn sb ?env ?cwd argv in
  let t0 = now sb in
  let status = wait_exit p ~timeout:120.0 () in
  { status; output = output p; ms = (now sb -. t0) *. 1000.0 }

(* ════════════════════════════════════════════════════════════════════════════ *)
(*  Harness context (typed — set by `fennec test system`, sane defaults by hand)  *)
(* ════════════════════════════════════════════════════════════════════════════ *)

(* The contract the harness fills in (Test_proto), exposed so a suite never hand-rolls getenv. *)
let app_dir () = Test_proto.app_dir ()    (* the project to run `fennec dev` in *)
let fennec () = Test_proto.bin ()         (* the fennec binary under test *)
let root () = Test_proto.root ()          (* the workspace root *)
let server_bc () = Test_proto.server_bc () (* the built server bytecode, if provided *)

(* Spawn THIS app's `fennec dev` (extra [args] appended), in the app dir, captured + reaped on
   teardown like any spawn. The standing-server primitive for System suites — no FENNEC_* in
   userland. Pair with {!wait_ready}. *)
let dev ?(args = []) sb = spawn sb ~cwd:(app_dir ()) (fennec () :: "dev" :: args)

let wait_output p ?(timeout = 10.0) sub =
  let clock = Eio.Stdenv.clock p.sb.env in
  let deadline = Eio.Time.now clock +. timeout in
  let rec loop () =
    if Fennec_hunt_util.contains (output p) sub then ()
    else if not (alive p) then failwith (Printf.sprintf "process exited before its output contained %S" sub)
    else if Eio.Time.now clock > deadline then raise (Timeout (Printf.sprintf "waited %.0fs for output %S" timeout sub))
    else (Eio.Time.sleep clock 0.05; loop ())
  in
  loop ()

let wait_ready p ~port ?(timeout = 30.0) () =
  let clock = Eio.Stdenv.clock p.sb.env in
  let deadline = Eio.Time.now clock +. timeout in
  let rec loop () =
    if alive p && port_open port then ()
    else if not (alive p) then failwith (Printf.sprintf "process exited before binding :%d" port)
    else if Eio.Time.now clock > deadline then raise (Timeout (Printf.sprintf "waited %.0fs for :%d" timeout port))
    else (Eio.Time.sleep clock 0.05; loop ())
  in
  loop ()

(* ════════════════════════════════════════════════════════════════════════════ *)
(*  Assertions + scenario runner                                                  *)
(* ════════════════════════════════════════════════════════════════════════════ *)

let check name cond = if not cond then failwith ("check failed: " ^ name)

let tmp_base () =
  let shm = "/dev/shm" in
  if (try Sys.is_directory shm && (Unix.access shm [ Unix.W_OK ]; true) with _ -> false) then shm
  else Filename.get_temp_dir_name ()

(* ── registry: scenarios REGISTER as a module-init side effect (the [let%system] ppx, or a hand
   [test] call), and [run] executes them — the same shape as the Unit / Http / Live registries.
   Holding the closure (not running on call) is what lets the runner filter (--grep) and skip
   @manual scenarios, with no hand-written entry point in userland. ── *)
type scenario = { name : string; manual : bool; file : string; line : int; body : sandbox -> unit }

let registry : scenario list ref = ref []
let register ~manual ~name ~file ~line body = registry := { name; manual; file; line; body } :: !registry

let test_loc ~name ~file ~line body = register ~manual:false ~name ~file ~line body
let test_manual_loc ~name ~file ~line body = register ~manual:true ~name ~file ~line body
let test name body = register ~manual:false ~name ~file:"" ~line:0 body

(* the captured output of every process a scenario spawned (the [.proc-N.log] files) — the
   evidence to print when it fails *)
let captured_logs dir =
  match Sys.readdir dir with
  | exception _ -> []
  | entries ->
    Array.to_list entries
    |> List.filter (fun f -> String.length f > 0 && f.[0] = '.' && Filename.check_suffix f ".log")
    |> List.sort compare
    |> List.filter_map (fun f ->
           match In_channel.with_open_bin (Filename.concat dir f) In_channel.input_all with
           | "" | (exception _) -> None
           | s -> Some s)

let tail ?(max = 2000) s =
  let n = String.length s in
  if n <= max then s else "…" ^ String.sub s (n - max) max

(* run ONE scenario in a fresh sandbox and print its outcome. On failure, dump the captured
   process output (the evidence) and KEEP the sandbox for inspection; on success, remove it. *)
let run_one (sc : scenario) =
  let env = env_exn () in
  let dir = Filename.concat (tmp_base ()) (Printf.sprintf "fennec-sys-%d-%d" (Unix.getpid ()) (next_id ())) in
  (try Unix.mkdir dir 0o755 with _ -> ());
  let clock = Eio.Stdenv.clock env in
  let t0 = Eio.Time.now clock in
  let outcome = try Eio.Switch.run (fun sw -> sc.body { dir; env; sw; ports = [] }); Ok () with e -> Error e in
  let ms = (Eio.Time.now clock -. t0) *. 1000.0 in
  match outcome with
  | Ok () ->
    incr passed;
    (try Eio.Path.rmtree ~missing_ok:true Eio.Path.(Eio.Stdenv.fs env / dir) with _ -> ());
    Printf.printf "  %s  %s %s\n%!" (color "32" (glyph "✓" "ok")) sc.name (color "2" (Printf.sprintf "(%.0fms)" ms))
  | Error e ->
    incr failed;
    let msg = match e with Failure m -> m | Timeout m -> "timeout: " ^ m | e -> Printexc.to_string e in
    let loc = if sc.file = "" then "" else color "2" (Printf.sprintf " (%s:%d)" (Filename.basename sc.file) sc.line) in
    Printf.printf "  %s  %s %s%s\n%!" (color "31" (glyph "✗" "FAIL")) sc.name (color "2" (Printf.sprintf "(%.0fms)" ms)) loc;
    String.split_on_char '\n' msg |> List.iter (fun l -> Printf.printf "     %s\n%!" l);
    (match captured_logs dir with
     | [] -> ()
     | logs ->
       Printf.printf "     %s\n%!" (color "2" "── captured process output ──");
       List.iter (fun s -> String.split_on_char '\n' (tail s)
                           |> List.iter (fun l -> Printf.printf "     %s %s\n%!" (color "2" "│") l)) logs);
    Printf.printf "     %s\n%!" (color "2" (Printf.sprintf "(sandbox kept for inspection: %s)" dir))

(* ════════════════════════════════════════════════════════════════════════════ *)
(*  Entry point                                                                   *)
(* ════════════════════════════════════════════════════════════════════════════ *)

(* the re-exec hop: when spawned as our own exec helper (argv = [_; sentinel; prog; args…]),
   become a session leader then exec the real program. Must run before anything else. *)
let handle_sentinel () =
  match Array.to_list Sys.argv with
  | _ :: s :: prog :: rest when s = sentinel ->
    (try ignore (Unix.setsid ()) with _ -> ());
    (try Unix.execvp prog (Array.of_list (prog :: rest))
     with _ -> Printf.eprintf "fennec system: exec failed: %s\n%!" prog; exit 127)
  | _ -> ()

(* tiny argv reader (the runner has no cmdliner): --grep SUB (substring on the scenario name),
   --manual (also run @manual scenarios, which are otherwise skipped). *)
let arg_value flag =
  let a = Sys.argv and r = ref None in
  Array.iteri (fun i s -> if s = flag && i + 1 < Array.length a then r := Some a.(i + 1)) a; !r
let arg_flag flag = Array.exists (fun s -> s = flag) Sys.argv

(* Run the registered scenarios (filtered by --grep on the scenario name; --manual includes opt-in
   ones). Exit codes: [0] all passed, [1] a scenario failed, [3] a --grep was given but matched
   nothing (so a filter that matches nothing is never a silent green). THIS is the executable's
   whole entry point — [run.ml] is just [let () = exit (Fennec_hunt.System.run ())]. *)
let run () =
  handle_sentinel ();
  let grep = arg_value "--grep" and include_manual = arg_flag "--manual" in
  let all = List.rev !registry in
  let chosen = List.filter (fun s ->
      (include_manual || not s.manual)
      && (match grep with Some g -> Fennec_hunt_util.contains s.name g | None -> true)) all in
  let skipped_manual = List.length (List.filter (fun s -> s.manual && not include_manual) all) in
  let manual_note () =
    if skipped_manual > 0 then
      Printf.printf "  %s\n%!" (color "2" (Printf.sprintf "%d @manual scenario%s skipped (pass --manual to include)"
                                             skipped_manual (if skipped_manual = 1 then "" else "s"))) in
  passed := 0; failed := 0;
  match grep, chosen with
  | Some g, [] ->
    Printf.printf "\n  %s\n%!" (color "33" (Printf.sprintf "no scenarios matched --grep %s" g));
    manual_note (); 3
  | _ ->
    Eio_main.run (fun env -> ambient := Some env; List.iter run_one chosen);
    Printf.printf "\n";
    manual_note ();
    if !failed > 0 then (
      Printf.printf "  %s\n%!" (color "31" (Printf.sprintf "%d of %d scenarios failed" !failed (!passed + !failed)));
      1)
    else (Printf.printf "  %s\n%!" (color "32" (Printf.sprintf "%d scenarios passed" !passed)); 0)

(* Back-compat / self-test entry: register via [f] (which calls [test]) then run. Userland uses
   the generated [run.ml] + [let%system] instead, with no [main] at all. *)
let main f = handle_sentinel (); f (); exit (run ())
