(* Deterministic single-node replica-set lifecycle.

   Change streams (and transactions) require a replica set, even for one node, so "just works"
   means: launch mongod with --replSet, wait until it answers, initiate the set if it has never
   been initiated, then wait until this node is PRIMARY. No fixed sleeps — every step polls an
   explicit condition with a bounded timeout. The replica config uses 127.0.0.1:<port>, never a
   hostname, which is the usual source of nondeterministic "member unreachable" failures.

   Three everyday modes sit on top of this:
     - reuse:     if a mongod is already answering on the port, adopt it instead of spawning a
                  second one (fast inner-loop dev). [proc = None] means we don't own the process and
                  [stop] leaves it running.
     - ephemeral: a throwaway instance on a free port with its data dir under tmpfs (/dev/shm) when
                  available, wiped on [stop]. The basis of [with_ephemeral] for hermetic,
                  parallel-safe tests.
     - owned:     we spawned it and we tear it down, but keep the data dir. *)

type t = {
  proc : [ `Generic ] Eio.Process.ty Eio.Resource.t option;
  client : Client.t;
  port : int;
  dbpath : string;
  ephemeral : bool;
}

let pidfile dbpath = Filename.concat dbpath "mongod.pid"

let mongod_args ~port ~dbpath ~replset ~logpath =
  [
    "mongod";
    "--replSet"; replset;
    "--port"; string_of_int port;
    "--dbpath"; dbpath;
    "--bind_ip"; "127.0.0.1";
    "--logpath"; logpath;
    (* a pidfile lets the at_exit backstop in [with_ephemeral] reap the process even if the program
       exits without unwinding (e.g. a stray Stdlib.exit) *)
    "--pidfilepath"; pidfile dbpath;
  ]

(* maxPoolSize raised above libmongoc's default of 100: every open change stream holds a pooled
   connection for its whole lifetime, so the concurrent-stream ceiling is the pool size. See
   Client.default_uri for the full rationale. *)
let uri ~port = Printf.sprintf "mongodb://127.0.0.1:%d/?directConnection=true&maxPoolSize=256" port

(* A short server-selection timeout so probing an absent server fails fast rather than blocking the
   default 30s. *)
let probe_uri ~port =
  Printf.sprintf "mongodb://127.0.0.1:%d/?directConnection=true&serverSelectionTimeoutMS=800" port

let server_up ~port =
  match
    let c = Client.connect ~uri:(probe_uri ~port) () in
    Client.ping c ~db:"admin"
  with
  | up -> up
  | exception _ -> false

(* poll [f] until true, or fail after [timeout] seconds *)
let wait_until ~clock ~timeout ~what f =
  let deadline = Eio.Time.now clock +. timeout in
  let rec loop () =
    if f () then ()
    else if Eio.Time.now clock > deadline then failwith (Printf.sprintf "timed out waiting for %s" what)
    else (Eio.Time.sleep clock 0.1; loop ())
  in
  loop ()

let is_primary client =
  match Client.command client ~db:"admin" (Bson.Document [ ("hello", Bson.Int 1) ]) with
  | reply -> ( match Bson.get reply "isWritablePrimary" with Some (Bson.Bool true) -> true | _ -> false)
  | exception _ -> false

let already_initiated client =
  match Client.command client ~db:"admin" (Bson.Document [ ("replSetGetStatus", Bson.Int 1) ]) with
  | _ -> true
  | exception _ -> false (* NotYetInitialized raises *)

let ensure_replica_set ~port ~replset client =
  if not (already_initiated client) then begin
    let config =
      Bson.Document
        [
          ("_id", Bson.String replset);
          ("members", Bson.Array [ Bson.Document [ ("_id", Bson.Int 0); ("host", Bson.String (Printf.sprintf "127.0.0.1:%d" port)) ] ]);
        ]
    in
    ignore (Client.command client ~db:"admin" (Bson.Document [ ("replSetInitiate", config) ]))
  end

(* Ask the OS for an unused TCP port by binding to port 0 and reading back the assignment.
   Inherently racy (the port could be taken before mongod binds), but good enough for the
   ephemeral/test path and far simpler than a registry. *)
let free_port () =
  let s = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
  Fun.protect
    ~finally:(fun () -> Unix.close s)
    (fun () ->
      Unix.bind s (Unix.ADDR_INET (Unix.inet_addr_loopback, 0));
      match Unix.getsockname s with Unix.ADDR_INET (_, p) -> p | _ -> failwith "free_port")

(* Prefer tmpfs (/dev/shm on Linux) so an ephemeral instance never touches disk; fall back to the OS
   temp dir (macOS has no /dev/shm). *)
let temp_dbpath () =
  let base = if (try Sys.is_directory "/dev/shm" with _ -> false) then "/dev/shm" else Filename.get_temp_dir_name () in
  Filename.temp_dir ~temp_dir:base "mongo-eph-" ""

let rec rm_rf path =
  match Sys.is_directory path with
  | true -> Sys.readdir path |> Array.iter (fun e -> rm_rf (Filename.concat path e)); Unix.rmdir path
  | false -> Sys.remove path
  | exception _ -> ()

let start ~env ~sw ?(port = 27017) ?(dbpath = "./.mongo-data") ?(replset = "rs0") ?(reuse = true) ?(ephemeral = false) () =
  let clock = Eio.Stdenv.clock env in
  if reuse && server_up ~port then begin
    (* Adopt the running instance: don't spawn, don't own. It may not be a replica set yet (e.g.
       someone started a bare mongod), so still ensure it. *)
    let client = Client.connect ~uri:(uri ~port) () in
    ensure_replica_set ~port ~replset client;
    wait_until ~clock ~timeout:30. ~what:"node to become PRIMARY" (fun () -> is_primary client);
    { proc = None; client; port; dbpath; ephemeral = false }
  end
  else begin
    if not (Sys.file_exists dbpath) then Unix.mkdir dbpath 0o755;
    let logpath = Filename.concat dbpath "mongod.log" in
    let proc_mgr = Eio.Stdenv.process_mgr env in
    let proc =
      (* drop the platform-specific (`Unix) tag so the handle is portable *)
      (Eio.Process.spawn ~sw proc_mgr (mongod_args ~port ~dbpath ~replset ~logpath) :> [ `Generic ] Eio.Process.ty Eio.Resource.t)
    in
    let client = Client.connect ~uri:(uri ~port) () in
    wait_until ~clock ~timeout:30. ~what:"mongod to accept connections" (fun () -> Client.ping client ~db:"admin");
    ensure_replica_set ~port ~replset client;
    wait_until ~clock ~timeout:30. ~what:"node to become PRIMARY" (fun () -> is_primary client);
    { proc = Some proc; client; port; dbpath; ephemeral }
  end

let stop t =
  match t.proc with
  | None -> () (* reused instance — leave it running for the next caller *)
  | Some proc ->
      (* graceful first (portable, unlike SIGTERM on Windows); the connection drops mid-command, so
         ignore the resulting error *)
      (try ignore (Client.command t.client ~db:"admin" (Bson.Document [ ("shutdown", Bson.Int 1) ])) with _ -> ());
      (try Eio.Process.signal proc Sys.sigterm with _ -> ());
      ignore (Eio.Process.await proc);
      if t.ephemeral then rm_rf t.dbpath

let client t = t.client
let uri_of t = uri ~port:t.port
let port t = t.port

(* The one-liner: start (or reuse) a local single-node replica set, hand the connected client to
   [f], and tear everything down afterwards. *)
let with_replica_set ~env ?port ?dbpath ?replset ?reuse f =
  Eio.Switch.run @@ fun sw ->
  let t = start ~env ~sw ?port ?dbpath ?replset ?reuse () in
  Fun.protect ~finally:(fun () -> stop t) (fun () -> f t)

(* Hermetic throwaway: a private mongod on a free port with a tmpfs data dir, wiped on exit. Never
   reuses or collides with a developer's local instance, so it is safe to run many in parallel.
   Ideal for tests. *)
let with_ephemeral ~env ?port ?replset f =
  Eio.Switch.run @@ fun sw ->
  let port = match port with Some p -> p | None -> free_port () in
  let dbpath = temp_dbpath () in
  let t = start ~env ~sw ?replset ~port ~dbpath ~reuse:false ~ephemeral:true () in
  (* Backstop against process death that bypasses [Fun.protect] — most notably a [Stdlib.exit] from
     inside the event loop, which does NOT unwind the stack. [stop] still does the graceful teardown
     on the normal/exception paths; this only fires if we never got there, and is a harmless no-op
     once [stop] has run (the pid is gone and the dir removed). This makes an orphaned mongod
     impossible regardless of how the process terminates. *)
  let backstop () =
    (try
       let ic = open_in (pidfile dbpath) in
       let pid = int_of_string (String.trim (input_line ic)) in
       close_in ic;
       (try Unix.kill pid Sys.sigkill with _ -> ())
     with _ -> ());
    rm_rf dbpath
  in
  at_exit backstop;
  Fun.protect ~finally:(fun () -> stop t) (fun () -> f t)
