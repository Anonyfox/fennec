(* Managed single-node MongoDB replica sets for dev/test sessions. A replica set (not a standalone
   mongod) is required because Pulse's reactive observe path uses MongoDB change streams.

   The lifecycle is deliberately one point of truth for the CLI:
   - explicit MONGO_URL wins;
   - fennec dev defaults to the in-process embedded engine (Burrow) — no mongod needed (see ensure_dev);
   - fennec test defaults to explicit :memory: and starts per-suite mongods only for --mongo.

   This module still owns real-mongod lifecycles (start/launch) for --mongo and for callers that set an
   explicit MONGO_URL; dev no longer auto-starts one.

   We own spawned mongods with the pure-Unix Mongod lifecycle. When a stable dev port is already
   answering (for example after a SIGKILL leak), we can adopt it for the session by initiating the
   replica set through the driver without claiming process ownership. *)

module Mongod = Fennec_mongo_mongod.Mongod
module Server = Fennec_mongo_driver.Server
module Runtime = Fennec_mongo_driver.Runtime

let replset = "rs0"

type t = Owned of Mongod.t | Adopted of { port : int; dbpath : string; uri : string }

let direct_uri port = Printf.sprintf "mongodb://127.0.0.1:%d/?directConnection=true" port
let port = function Owned t -> Mongod.port t | Adopted t -> t.port
let dbpath = function Owned t -> Mongod.dbpath t | Adopted t -> t.dbpath
let uri = function Owned t -> direct_uri (Mongod.port t) | Adopted t -> t.uri
let pid = function Owned t -> Some (Mongod.pid t) | Adopted _ -> None
let stop = function Owned t -> Mongod.stop t | Adopted _ -> ()
let export t = Unix.putenv Runtime.mongo_url_env (uri t)

let rec mkdir_p dir =
  if dir = "" || dir = "." || dir = "/" || Sys.file_exists dir then ()
  else begin
    mkdir_p (Filename.dirname dir);
    try Unix.mkdir dir 0o755 with Unix.Unix_error (Unix.EEXIST, _, _) -> ()
  end

let initiate port =
  Eio_main.run (fun env ->
      Eio.Switch.run (fun sw ->
          ignore (Server.start ~env ~sw ~reuse:true ~replset ~port ())))

let adopt ~port ~dbpath =
  try
    initiate port;
    Ok (Adopted { port; dbpath; uri = direct_uri port })
  with e -> Error (Printexc.to_string e)

let start ?port ?dbpath () =
  match Mongod.find () with
  | None -> Error (Mongod.install_hint ())
  | Some _ -> (
    try
      Option.iter (fun path -> mkdir_p path) dbpath;
      let t = Mongod.start ?port ?dbpath ~replset () in
      (try initiate (Mongod.port t) with e -> (try Mongod.stop t with _ -> ()); raise e);
      Ok (Owned t)
    with
    | Mongod.Launch_failed _ when Option.is_some port && Option.is_some dbpath ->
      adopt ~port:(Option.get port) ~dbpath:(Option.get dbpath)
    | e -> Error (Printexc.to_string e))

let launch () =
  match start () with
  | Ok t ->
    export t;
    Printf.eprintf "--mongo: managed replica-set mongod at 127.0.0.1:%d\n%!" (port t);
    Some t
  | Error msg ->
    Printf.eprintf "--mongo: could not launch mongod — database-backed features remain unavailable.\n%s\n%!" msg;
    None

(* the embedded engine's data directory for a dev session — gitignored and keyed by base port, so two
   `fennec dev` servers (or parallel e2e instances) on different ports never share an LMDB map *)
let dev_embedded_dir ~root ~base_port =
  Filename.concat root (Printf.sprintf "_build/.fennec/burrow/dev-%d" base_port)

let ensure_dev ~root ~base_port () =
  match Runtime.url () with
  | Some _ -> None (* an explicit MONGO_URL (real mongo / :memory: / :embedded:) always wins *)
  | None ->
    (* Default to the in-process embedded engine (Burrow): always available, nothing to install or
       carry around, and reactive observe works via the engine's in-process change path. mongosh-style
       access is the app's wire endpoint (auto-exposed in dev). A real mongod is still opt-in by setting
       MONGO_URL=mongodb://… , and `fennec test --mongo` still launches one per suite. *)
    let dir = dev_embedded_dir ~root ~base_port in
    Unix.putenv Runtime.mongo_url_env (Printf.sprintf ":embedded:%s" dir);
    Printf.eprintf "fennec dev: embedded MongoDB (Burrow) at %s\n%!" dir;
    None
