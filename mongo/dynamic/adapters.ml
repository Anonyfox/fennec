(* Native MongoDB backend — a {!Fennec_pulse.Backend.S} over the ported driver layer
   ({!Fennec_mongo_driver}: Client/Collection/Live), so the reactive/DDP/realtime stack runs over a
   real mongod with no other change (it is the same seam Minimongo implements in memory).

   Every blocking driver call runs in an Eio systhread, so a mongo round-trip suspends only the
   calling fiber, not the scheduler. [observe_changes] is REAL CHANGE STREAMS via {!Live} (one
   stream per collection, fanned out to per-query views) — not polling — which is why it needs a
   replica set (the managed mongod is launched as one by {!Fennec_mongo_driver.Server}). *)

module Driver = Fennec_mongo_driver
module Client = Driver.Client
module Coll = Driver.Collection
module Live = Driver.Live
module Runtime = Driver.Runtime
module Ffi = Fennec_mongo_ffi.Mongo_ffi
module Diff = Query.Diff
module Id = Query.Id
module Backend = Fennec_mongo_backend (* the PURE storage seam lib (Backend.S + query + Mini) *)
module B = Bson

let available = Ffi.available

type connection = Client.t

let connect uri = Client.connect ~uri ()

(* A native collection IS the driver collection. The ambient Eio switch that {!Live}'s change-stream
   daemons fork into is set when the collection is created. *)
type collection = Coll.t

let collection ?poll ~sw conn ~db ~name : collection =
  Live.set_switch sw;
  (match poll with Some p -> Live.set_poll_interval p | None -> ());
  Coll.create conn ~db ~name

let int_field reply k =
  match B.get reply k with Some v -> ( match B.as_float v with Some f -> int_of_float f | None -> 0) | None -> 0

let native_opts (q : Backend.query) =
  B.Document
    (List.filter_map Fun.id
       [ (match q.Backend.sort with B.Document [] -> None | s -> Some ("sort", s));
         (if q.Backend.skip > 0 then Some ("skip", B.int q.Backend.skip) else None);
         (if q.Backend.limit > 0 then Some ("limit", B.int q.Backend.limit) else None);
         (match q.Backend.fields with B.Document [] -> None | f -> Some ("projection", f)) ])

(* ---- Backend.S ---------------------------------------------------------- *)

let insert c (d : B.t) =
  (* the reactive layer mints an [_id] before calling here; mint one if a direct caller didn't, so
     the returned id always matches the stored document *)
  let kvs = Diff.kvs_of d in
  let id, doc =
    match List.assoc_opt "_id" kvs with
    | Some v -> (Diff.id_to_string v, d)
    | None -> let id = Id.random_id () in (id, B.Document (("_id", B.String id) :: kvs))
  in
  ignore (Coll.insert_one c doc);
  id

let update c ~multi ~upsert sel m =
  let reply =
    Client.command c.Coll.client ~db:c.Coll.db
      (B.doc
         [ ("update", B.str c.Coll.name);
           ("updates", B.array [ B.doc [ ("q", sel); ("u", m); ("multi", B.bool multi); ("upsert", B.bool upsert) ] ]) ])
  in
  int_field reply "n"

let remove c sel = int_field (Coll.delete_many c ~filter:sel) "n"
let find c (q : Backend.query) = Coll.find c ~filter:q.Backend.selector ~opts:(native_opts q) ()
let find_one c (q : Backend.query) = match find c { q with Backend.limit = 1 } with x :: _ -> Some x | [] -> None
let count c sel = Coll.count c ~filter:sel ()
let aggregate c ?lookup (pipeline : B.t list) =
  ignore lookup (* a real mongod resolves $lookup itself; the in-memory resolver is not needed here *);
  Coll.aggregate c ~pipeline:(B.Array pipeline) ()
let distinct c key sel = Coll.distinct c ~key ~filter:sel ()

(* BEST-EFFORT fence on the native driver: mongod's change-stream delivery is asynchronous (network)
   and v1 carries no resume-token plumbing, so the fence runs immediately — a method's [updated] may
   precede its stream deltas under lag (an optimistic client then briefly shows the pre-method state;
   Meteor fences via oplog positions — the resume-token equivalent is the marked seam here). *)
let fence _c k = k ()

(* index ops on the native driver — name-explicit so reconcile matches; list returns names *)
let ensure_index c ~name ~keys ~unique ~sparse =
  let opts =
    (if unique then [ ("unique", B.Bool true) ] else []) @ (if sparse then [ ("sparse", B.Bool true) ] else [])
  in
  ignore (Coll.create_index c ~keys ~opts ~name ())
let drop_index c ~name = Coll.drop_index c ~name
let index_names c =
  List.filter_map (fun d -> match B.get d "name" with Some (B.String s) -> Some s | _ -> None) (Coll.list_indexes c)

(* install the model's $jsonSchema so mongod itself rejects foreign writes that violate the shape:
   create-with-validator on a fresh collection, else collMod when it already exists. validationLevel
   "moderate" keeps legacy/invalid docs updatable (the evolution-tolerance stance in Schema). *)
let ensure_validator c = function
  | None -> ()
  | Some v ->
      let opts = [ ("validator", v); ("validationLevel", B.str "moderate") ] in
      ( try ignore (Client.command c.Coll.client ~db:c.Coll.db (B.doc (("create", B.str c.Coll.name) :: opts)))
        with _ -> ignore (Client.command c.Coll.client ~db:c.Coll.db (B.doc (("collMod", B.str c.Coll.name) :: opts))) )

(* real change streams: Live keeps ONE stream per collection, replays the initial set synchronously
   (ready-after-data), then routes per-query field-level deltas *)
let observe_changes c (q : Backend.query) ~added ~changed ~removed : Backend.handle =
  let h =
    Live.observe_changes
      (Live.query c ~selector:q.Backend.selector ~sort:q.Backend.sort ~skip:q.Backend.skip ~limit:q.Backend.limit
         ~fields:q.Backend.fields)
      ~added ~changed ~removed ()
  in
  { Backend.stop = h.Live.stop }

(* ---- Embedded (Burrow) backend ------------------------------------------ *)

(* Burrow — the native, in-process, on-disk MongoDB-compatible engine (fennec-mongo.burrow). One
   engine per on-disk database directory, shared across that database's collections; selected by a
   burrow:// MONGO_URL (the path's trailing segment is the db). Durable by default (group-committed F_FULLFSYNC). *)
module Burrow_engine = Burrow.Engine

let embedded_engines : (string, Burrow_engine.t) Hashtbl.t = Hashtbl.create 8

let rec mkdir_p dir =
  if not (Sys.file_exists dir) then begin
    mkdir_p (Filename.dirname dir);
    (try Unix.mkdir dir 0o755 with Unix.Unix_error (Unix.EEXIST, _, _) -> ())
  end

let embedded_engine ~sw dir =
  match Hashtbl.find_opt embedded_engines dir with
  | Some e -> e
  | None ->
    mkdir_p dir;
    (* the writer fiber runs under [sw] — the server's long-lived switch, captured at first open *)
    let e = Burrow_engine.open_ ~sw dir in
    Hashtbl.replace embedded_engines dir e;
    e

type embedded = { eng : Burrow_engine.t; ecoll : Burrow_engine.collection }

let embedded_collection ~sw ~dir ~name =
  let eng = embedded_engine ~sw dir in
  { eng; ecoll = Burrow_engine.collection eng name }

let bw_find e (q : Backend.query) =
  Burrow_engine.find e.eng e.ecoll ~selector:q.Backend.selector ~sort:q.Backend.sort ~skip:q.Backend.skip
    ~limit:q.Backend.limit ~fields:q.Backend.fields

let bw_find_one e (q : Backend.query) =
  Burrow_engine.find_one e.eng e.ecoll ~selector:q.Backend.selector ~sort:q.Backend.sort
    ~skip:q.Backend.skip ~fields:q.Backend.fields

let bw_observe e (q : Backend.query) ~added ~changed ~removed =
  { Backend.stop =
      Burrow_engine.observe_changes e.eng e.ecoll ~selector:q.Backend.selector ~sort:q.Backend.sort
        ~skip:q.Backend.skip ~limit:q.Backend.limit ~fields:q.Backend.fields ~added ~changed ~removed }

(* ---- runtime-selectable backend ----------------------------------------- *)

(* A runtime-selectable backend: in-memory, this native driver, OR the embedded Burrow engine, behind
   ONE Backend.S, so an app picks at boot (real mongo / burrow:// / :memory:) with no type change
   downstream. The per-op dispatch references the outer (native / burrow) ops — non-recursive [let],
   so no shadowing. *)
module Dynamic = struct
  module Mini = Backend.Mini

  type collection = Mem of Minimongo.t | Native of Coll.t | Embedded of embedded | Missing of string

  let mem m = Mem m
  let real ?poll ~sw conn ~db ~name = Native (collection ?poll ~sw conn ~db ~name)
  let missing message = Missing message
  let unavailable message = failwith ("Fennec.Mongo: " ^ message)

  (* The convention the fennec CLI speaks: MONGO_URL is the one database location. `fennec dev`
     auto-starts/adopts a local mongod when possible; `fennec test` sets :memory: by default and
     `fennec test --mongo` supplies a per-suite real URL. Missing MONGO_URL is not a hidden memory
     fallback; operations fail clearly. Build it in [Fennec.serve ~on_start] so the [sw] it captures
     drives Live's change-stream daemons. *)
  let mongo_url_env = Runtime.mongo_url_env

  (* the single MONGO_URL parser ({!Runtime.backend}) picks the engine: minimongo (:memory:), the
     embedded Burrow engine (burrow://, data dir = base/db from the URL path), or the native driver
     (mongodb://). The db comes from the URL, not from app code. *)
  let from_env ?poll ~sw ~name () =
    match Runtime.backend () with
    | Runtime.Missing -> missing (Runtime.unavailable_message ())
    | Runtime.Memory -> mem (Minimongo.create ())
    | Runtime.Burrow { base; db; _ } -> Embedded (embedded_collection ~sw ~dir:(Filename.concat base db) ~name)
    | Runtime.Mongo { uri; db } -> real ?poll ~sw (connect uri) ~db ~name

  (* The ambient Eio switch the data layer forks its long-lived daemons into (Live's change-streams, the
     embedded engine, the wire endpoint). [Fennec.serve] installs it ONCE, inside its switch, at startup
     (see {!Fennec.serve}); then app and accounts code open a backend collection by name via [collection]
     without threading [sw] through every call. Set before any collection is opened, so a plain ref set
     once on the boot fiber is enough — no cross-domain sharing. *)
  let ambient_switch : Eio.Switch.t option ref = ref None
  let set_switch sw = ambient_switch := Some sw

  (* Open the MONGO_URL-selected collection [name] on the ambient switch — the no-thread entry point for
     [Fennec.serve]-hosted code. Before the switch is installed (only possible by reaching the data layer
     outside [serve]) it yields a [Missing] collection whose ops fail with a clear message, never silently. *)
  let collection ?poll ~name () =
    match !ambient_switch with
    | Some sw -> from_env ?poll ~sw ~name ()
    | None -> missing "Fennec data layer is not booted — Fennec.serve installs the Eio switch at startup"

  let insert c d =
    match c with
    | Mem m -> Mini.insert m d
    | Native r -> insert r d
    | Embedded e -> Burrow_engine.insert e.eng e.ecoll d
    | Missing message -> unavailable message
  let update c ~multi ~upsert s m =
    match c with
    | Mem mm -> Mini.update mm ~multi ~upsert s m
    | Native r -> update r ~multi ~upsert s m
    | Embedded e -> Burrow_engine.update e.eng e.ecoll ~multi ~upsert s m
    | Missing message -> unavailable message
  let remove c s =
    match c with
    | Mem m -> Mini.remove m s
    | Native r -> remove r s
    | Embedded e -> Burrow_engine.remove e.eng e.ecoll s
    | Missing message -> unavailable message
  let find c q =
    match c with Mem m -> Mini.find m q | Native r -> find r q | Embedded e -> bw_find e q | Missing message -> unavailable message
  let find_one c q =
    match c with Mem m -> Mini.find_one m q | Native r -> find_one r q | Embedded e -> bw_find_one e q | Missing message -> unavailable message
  let count c s =
    match c with
    | Mem m -> Mini.count m s
    | Native r -> count r s
    | Embedded e -> Burrow_engine.count e.eng e.ecoll ~selector:s
    | Missing message -> unavailable message
  let aggregate c ?(lookup = fun _ -> []) p =
    match c with
    | Mem m -> Mini.aggregate m ~lookup p
    | Native r -> aggregate r ~lookup p
    | Embedded e -> Burrow_engine.aggregate e.eng e.ecoll ~lookup p
    | Missing message -> unavailable message
  let distinct c k s =
    match c with
    | Mem m -> Mini.distinct m k s
    | Native r -> distinct r k s
    | Embedded e -> Burrow_engine.distinct e.eng e.ecoll ~key:k ~selector:s
    | Missing message -> unavailable message

  let observe_changes c q ~added ~changed ~removed =
    match c with
    | Mem m -> Mini.observe_changes m q ~added ~changed ~removed
    | Native r -> observe_changes r q ~added ~changed ~removed
    | Embedded e -> bw_observe e q ~added ~changed ~removed
    | Missing message -> unavailable message

  let fence c k =
    match c with
    | Mem m -> Mini.fence m k
    | Native r -> fence r k
    | Embedded e -> Burrow_engine.fence e.eng e.ecoll k
    | Missing message -> unavailable message
  let ensure_index c ~name ~keys ~unique ~sparse =
    match c with
    | Mem m -> Mini.ensure_index m ~name ~keys ~unique ~sparse
    | Native r -> ensure_index r ~name ~keys ~unique ~sparse
    | Embedded e -> Burrow_engine.ensure_index e.eng e.ecoll ~name ~keys ~unique ~sparse
    | Missing message -> unavailable message
  let drop_index c ~name =
    match c with
    | Mem m -> Mini.drop_index m ~name
    | Native r -> drop_index r ~name
    | Embedded e -> Burrow_engine.drop_index e.eng e.ecoll ~name
    | Missing message -> unavailable message
  let index_names c =
    match c with
    | Mem m -> Mini.index_names m
    | Native r -> index_names r
    | Embedded e -> Burrow_engine.index_names e.ecoll
    | Missing message -> unavailable message

  let ensure_validator c v =
    match c with
    | Mem m -> Mini.ensure_validator m v
    | Native r -> ensure_validator r v
    | Embedded e -> Burrow_engine.set_validator e.eng e.ecoll v
    | Missing message -> unavailable message
end

(* ---- mongosh / driver exposure: the embedded engine over the MongoDB wire protocol ----------

   {!expose} opens a MongoDB wire-protocol TCP listener in front of the embedded Burrow engine, so any
   real client — mongosh, Compass, a driver — connects exactly as it would to a hosted mongod and runs
   ad-hoc queries. It shares the SAME engine cache as the burrow:// backend (keyed by directory), so
   it sees the app's live data and its writes funnel through the same group-committing writer (no
   concurrent-writer hazard). Secure by default: binds loopback, requires SCRAM-SHA-256 when any user is
   configured, never speaks [$where]/JS, caps message size, and offers a read-only mode + TLS. *)

let wire_rng_ready = ref false
let ensure_wire_rng () = if not !wire_rng_ready then (Mirage_crypto_rng_unix.use_default (); wire_rng_ready := true)

type wire_user = { wu_name : string; wu_password : string }

let wire_user ~user ~password = { wu_name = user; wu_password = password }

(* a burrow:// authority host -> an Eio bind address (0.0.0.0/empty => all interfaces; else loopback or
   a dotted-quad) *)
let ipaddr_of_host h =
  match h with
  | "" | "0.0.0.0" | "::" -> Eio.Net.Ipaddr.V4.any
  | "localhost" | "127.0.0.1" -> Eio.Net.Ipaddr.V4.loopback
  | _ -> (
    match List.map int_of_string_opt (String.split_on_char '.' h) with
    | [ Some a; Some b; Some c; Some d ] when a < 256 && b < 256 && c < 256 && d < 256 ->
      Eio.Net.Ipaddr.of_raw (String.init 4 (fun i -> Char.chr (List.nth [ a; b; c; d ] i)))
    | _ -> Eio.Net.Ipaddr.V4.loopback)

let expose ~sw ~net ?(addr = `Tcp (Eio.Net.Ipaddr.V4.loopback, Runtime.default_wire_port)) ?(base_dir = "./.fennec/burrow")
    ?(users = []) ?require_auth ?(read_only = false) ?tls ?(max_message_bytes = 48_000_000) ?(max_connections = 1000) () =
  ensure_wire_rng ();
  let require_auth = match require_auth with Some b -> b | None -> users <> [] in
  if require_auth && users = [] then
    invalid_arg "Fennec_mongo.expose: require_auth is set but no users were configured (nobody could connect)";
  (* derive a SCRAM-SHA-256 verifier per user once; the plaintext password is never retained *)
  let table = Hashtbl.create 8 in
  List.iter
    (fun u ->
      (* MongoDB's SCRAM salt length is (hash_size - 4): SHA-256 -> 28, SHA-1 -> 16. The server reserves
         the trailing 4 bytes for PBKDF2's block index, and drivers (libmongoc, mongosh) enforce exactly
         this length — a 16- or 32-byte salt is rejected outright. *)
      let salt = Mirage_crypto_rng.generate 28 in
      Hashtbl.replace table u.wu_name (Mongo_wire.Scram.make_credential ~salt ~iterations:15000 ~password:u.wu_password))
    users;
  let dir_of db = Filename.concat base_dir db in
  let module Db = struct
    type coll = embedded

    let collection ~db ~name = embedded_collection ~sw ~dir:(dir_of db) ~name
    let collection_names ~db = Burrow_engine.collection_names (embedded_engine ~sw (dir_of db))

    let database_names () =
      if (try Sys.is_directory base_dir with _ -> false) then
        Sys.readdir base_dir |> Array.to_list
        |> List.filter (fun n -> try Sys.is_directory (Filename.concat base_dir n) with _ -> false)
      else []

    let insert e d = Burrow_engine.insert e.eng e.ecoll d
    let find e ~selector ~sort ~skip ~limit ~fields = Burrow_engine.find e.eng e.ecoll ~selector ~sort ~skip ~limit ~fields
    let count e sel = Burrow_engine.count e.eng e.ecoll ~selector:sel
    let update e ~multi ~upsert q m = Burrow_engine.update e.eng e.ecoll ~multi ~upsert q m
    let remove e sel = Burrow_engine.remove e.eng e.ecoll sel

    (* $lookup / $unionWith resolve sibling collections in the same engine *)
    let aggregate e p =
      let empty = Bson.Document [] in
      let lookup name =
        let c = Burrow_engine.collection e.eng name in
        Burrow_engine.find e.eng c ~selector:empty ~sort:empty ~skip:0 ~limit:0 ~fields:empty
      in
      Burrow_engine.aggregate e.eng e.ecoll ~lookup p

    let distinct e key sel = Burrow_engine.distinct e.eng e.ecoll ~key ~selector:sel
    let ensure_index e ~name ~keys ~unique ~sparse = Burrow_engine.ensure_index e.eng e.ecoll ~name ~keys ~unique ~sparse
    let drop_index e ~name = Burrow_engine.drop_index e.eng e.ecoll ~name
    let index_specs e = Burrow_engine.index_specs e.ecoll

    (* v1 drop empties the collection (its catalog entry/sub-DBs remain); a true drop awaits engine support *)
    let drop_collection ~db ~name =
      let e = embedded_collection ~sw ~dir:(dir_of db) ~name in
      ignore (Burrow_engine.remove e.eng e.ecoll (Bson.Document []))
  end in
  let module Server = Wire_server.Make (Db) in
  let config : Wire_server.config =
    { require_auth;
      lookup = (fun name -> Hashtbl.find_opt table name);
      read_only;
      max_message_bytes;
      max_connections;
      server_nonce = (fun () -> Base64.encode_string (Mirage_crypto_rng.generate 18));
      tls }
  in
  let where = match addr with `Tcp (ip, port) -> Format.asprintf "%a:%d" Eio.Net.Ipaddr.pp ip port | `Unix p -> p in
  (* binding the side-channel endpoint is best-effort: a clash (another dev server already holding the
     port) or any listen error must never take down the app's web server — log and carry on *)
  try
    Server.run ~sw ~net ~addr config;
    Printf.printf "fennec: mongo-wire endpoint on %s (auth=%b, read_only=%b, tls=%b)\n%!" where require_auth read_only (tls <> None)
  with e -> Printf.eprintf "fennec: mongo-wire endpoint NOT started on %s — %s\n%!" where (Printexc.to_string e)

(* The auto path the framework calls (Fennec_pulse_app.start). The MONGO_URL alone decides everything:
   if it is a burrow:// URL WITH an authority, open the mongosh wire endpoint there — bind host:port,
   SCRAM when the URL carries user:pass, read-only when ?readonly. No authority (or any other backend)
   => no endpoint. So dev's zero-config endpoint comes from `fennec dev` generating a loopback authority,
   not from a dev branch here. (?tls=true is parsed but TLS reuse for the wire endpoint is not yet wired
   — use an explicit {!expose} ~tls for a TLS wire endpoint; we log and serve plaintext.) *)
let expose_from_env ~sw ~net () =
  match Runtime.backend () with
  | Runtime.Burrow { base; expose = Some s; _ } ->
    if s.tls then
      Printf.eprintf "fennec: mongo-wire ?tls=true is not yet wired for the auto endpoint — serving plaintext on %s:%d\n%!" s.host s.port;
    let users = match (s.user, s.pass) with Some u, Some p -> [ wire_user ~user:u ~password:p ] | _ -> [] in
    expose ~sw ~net ~addr:(`Tcp (ipaddr_of_host s.host, s.port)) ~base_dir:base ~users ~require_auth:(s.user <> None)
      ~read_only:s.read_only ()
  | _ -> ()

(* The one data-layer boot the framework runs inside [Fennec.serve]'s switch, before any connection is
   served: install the ambient Eio switch (so app + accounts collections open by name, no [sw] threading)
   and — for a burrow:// URL with an authority — front the embedded engine over the MongoDB wire protocol
   so `mongosh` connects. MONGO_URL alone decides everything; there is no app-level "start". *)
let boot ~sw ~net () =
  Dynamic.set_switch sw;
  expose_from_env ~sw ~net ()
