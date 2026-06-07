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
let p_of sb rel = Eio.Path.(fs sb / Filename.concat sb.dir rel)

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

let wait_port ?(timeout = 10.0) port =
  let clock = Eio.Stdenv.clock (env_exn ()) in
  let deadline = Eio.Time.now clock +. timeout in
  let rec loop () =
    if port_open port then ()
    else if Eio.Time.now clock > deadline then raise (Timeout (Printf.sprintf "nothing listening on :%d after %.0fs" port timeout))
    else (Eio.Time.sleep clock 0.05; loop ())
  in
  loop ()

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

(* ════════════════════════════════════════════════════════════════════════════ *)
(*  Processes                                                                     *)
(* ════════════════════════════════════════════════════════════════════════════ *)

let eio_status = function `Exited n -> Unix.WEXITED n | `Signaled n -> Unix.WSIGNALED n

let output p = try Eio.Path.load Eio.Path.(fs p.sb / p.logpath) with _ -> ""
let pid p = p.pid
let alive p = !(p.st) = None

let spawn sb ?(env = []) argv =
  let logpath = Filename.concat sb.dir (Printf.sprintf ".proc-%d.log" (next_id ())) in
  let mgr = Eio.Stdenv.process_mgr sb.env in
  let sink = Eio.Path.open_out ~sw:sb.sw ~create:(`If_missing 0o644) Eio.Path.(fs sb / logpath) in
  let nullsrc = Eio.Path.open_in ~sw:sb.sw Eio.Path.(fs sb / "/dev/null") in
  let full_env =
    Array.append (Unix.environment ()) (Array.of_list (List.map (fun (k, v) -> k ^ "=" ^ v) env))
  in
  let cwd = Eio.Path.(fs sb / sb.dir) in
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

let run sb ?env argv =
  let p = spawn sb ?env argv in
  let t0 = now sb in
  let status = wait_exit p ~timeout:120.0 () in
  { status; output = output p; ms = (now sb -. t0) *. 1000.0 }

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

let test name f =
  let env = env_exn () in
  let dir = Filename.concat (tmp_base ()) (Printf.sprintf "fennec-sys-%d-%d" (Unix.getpid ()) (next_id ())) in
  (try Unix.mkdir dir 0o755 with _ -> ());
  let clock = Eio.Stdenv.clock env in
  let t0 = Eio.Time.now clock in
  let outcome =
    try Eio.Switch.run (fun sw -> f { dir; env; sw; ports = [] }); Ok ()
    with e -> Error e
  in
  (try Eio.Path.rmtree ~missing_ok:true Eio.Path.(Eio.Stdenv.fs env / dir) with _ -> ());
  let ms = (Eio.Time.now clock -. t0) *. 1000.0 in
  match outcome with
  | Ok () ->
    incr passed;
    Printf.printf "  %s  %s %s\n%!" (color "32" (glyph "✓" "ok")) name (color "2" (Printf.sprintf "(%.0fms)" ms))
  | Error e ->
    incr failed;
    let msg = match e with Failure m -> m | Timeout m -> "timeout: " ^ m | e -> Printexc.to_string e in
    Printf.printf "  %s  %s %s\n%!" (color "31" (glyph "✗" "FAIL")) name (color "2" (Printf.sprintf "(%.0fms)" ms));
    String.split_on_char '\n' msg |> List.iter (fun l -> Printf.printf "     %s\n%!" l)

(* ════════════════════════════════════════════════════════════════════════════ *)
(*  Entry point                                                                   *)
(* ════════════════════════════════════════════════════════════════════════════ *)

let main f =
  (match Array.to_list Sys.argv with
   | _ :: s :: prog :: rest when s = sentinel ->
     (try ignore (Unix.setsid ()) with _ -> ());
     (try Unix.execvp prog (Array.of_list (prog :: rest))
      with _ -> Printf.eprintf "fennec system: exec failed: %s\n%!" prog; exit 127)
   | _ -> ());
  passed := 0;
  failed := 0;
  Eio_main.run (fun env -> ambient := Some env; f ());
  Printf.printf "\n";
  if !failed > 0 then (
    Printf.printf "  %s\n%!" (color "31" (Printf.sprintf "%d of %d scenarios failed" !failed (!passed + !failed)));
    exit 1)
  else Printf.printf "  %s\n%!" (color "32" (Printf.sprintf "%d scenarios passed" !passed))
