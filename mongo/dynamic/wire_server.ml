(* The MongoDB wire-protocol SERVER — the only IO layer of the mongosh/driver exposure. It drives the
   pure {!Mongo_wire} core (framing, BSON, SCRAM) over an Eio TCP listener (optionally TLS), keeps the
   per-connection auth + cursor state, enforces the resource limits, and dispatches each command to a
   backend.

   It is a functor over {!DB} — a small operations interface (richer than {!Fennec_mongo_backend.S}
   only by the catalog listing the wire needs) — so the same server fronts the embedded Burrow engine
   (the point) and is unit-testable over an in-memory backend with no socket. It does NOT depend on
   [Backend], [mirage-crypto], or the engine: randomness enters as the [server_nonce] closure the caller
   supplies, so the security-sensitive pieces stay either pure (the wire core) or injected.

   Security posture (the caller chooses via {!config}): bind address is the caller's (loopback by
   default in {!Fennec_mongo_dynamic.expose}); SCRAM-SHA-256 auth gates every non-handshake command when
   [require_auth]; an oversized or malformed frame drops only that connection (never the server); there
   is no [$where]/JS, so there is no server-side code injection; a read-only mode rejects every mutation;
   and only a vetted command set is implemented (no [shutdown]/[eval]/[fsync]/...). *)

module Frame = Mongo_wire.Frame
module Bw = Mongo_wire.Bson_wire
module Scram = Mongo_wire.Scram
module B = Bson

(* ---- the backend the wire protocol drives -------------------------------------------------- *)

module type DB = sig
  type coll

  val collection : db:string -> name:string -> coll
  val collection_names : db:string -> string list
  val database_names : unit -> string list
  val insert : coll -> B.t -> string
  val find : coll -> selector:B.t -> sort:B.t -> skip:int -> limit:int -> fields:B.t -> B.t list
  val count : coll -> B.t -> int
  val explain : coll -> selector:B.t -> sort:B.t -> B.t
  val update : coll -> multi:bool -> upsert:bool -> B.t -> B.t -> int
  val remove : coll -> B.t -> int
  val aggregate : coll -> B.t list -> B.t list
  val distinct : coll -> string -> B.t -> B.t list
  val ensure_index : coll -> name:string -> keys:B.t -> unique:bool -> sparse:bool -> unit
  val drop_index : coll -> name:string -> unit
  val index_specs : coll -> (string * (string * int) list * bool) list  (* name, keys, unique *)
  val drop_collection : db:string -> name:string -> unit
end

(* the security audit trail: the wire server emits one event per authentication, failed authentication, and
   authorization denial to the configured sink (default no-op) — the operator ships it to a log / SIEM. *)
module Audit = struct
  type outcome =
    | Authenticated  (** SCRAM authentication succeeded *)
    | Auth_failed  (** authentication failed (unknown user or bad password) *)
    | Denied of string * string  (** an authenticated user was denied — [(command, database)] *)

  type event = { at : float; peer : string; user : string option; outcome : outcome }
end

type config = {
  require_auth : bool;
  lookup : string -> Scram.credential option;  (* username -> stored verifier (for SCRAM) *)
  authorize : string -> write:bool -> db:string -> bool;  (* per-user, per-database authorization (post-auth) *)
  audit : Audit.event -> unit;  (* security audit sink: auth ok / failed + authz denials *)
  read_only : bool;
  max_message_bytes : int;
  max_connections : int;
  server_nonce : unit -> string;  (* a fresh, unpredictable nonce per SCRAM start (caller's CSPRNG) *)
  tls : Tls.Config.server option;
}

(* ---- small BSON helpers + reply builders --------------------------------------------------- *)

let ok = ("ok", B.Float 1.0)

let err ?(code = 59) msg = B.Document [ ("ok", B.Float 0.0); ("errmsg", B.String msg); ("code", B.Int code) ]

let to_int = function
  | B.Int n -> Some n
  | B.Int64 n -> Some (Int64.to_int n)
  | B.Float f -> Some (int_of_float f)
  | _ -> None

let dget d k = match d with B.Document f -> List.assoc_opt k f | _ -> None
let dstr d k = match dget d k with Some (B.String s) -> Some s | _ -> None
let ddoc d k = match dget d k with Some (B.Document _ as x) -> x | _ -> B.Document []
let dint d k = match dget d k with Some v -> to_int v | None -> None
let dbool d k = match dget d k with Some (B.Bool b) -> b | _ -> false
let darr d k = match dget d k with Some (B.Array xs) -> xs | _ -> []

let rec split_at n xs =
  if n <= 0 then ([], xs)
  else match xs with [] -> ([], []) | x :: r -> let a, b = split_at (n - 1) r in (x :: a, b)

let default_index_name = function
  | B.Document f -> String.concat "_" (List.concat_map (fun (k, v) -> [ k; (match to_int v with Some d -> string_of_int d | None -> "1") ]) f)
  | _ -> "idx"

let now_ms () = Int64.of_float (Unix.gettimeofday () *. 1000.)

(* case-insensitive substring test — for classifying error messages without pulling in a regex lib *)
let contains_ci hay sub =
  let hay = String.lowercase_ascii hay and sub = String.lowercase_ascii sub in
  let hl = String.length hay and sl = String.length sub in
  let rec go i = i + sl <= hl && (String.sub hay i sl = sub || go (i + 1)) in
  sl = 0 || go 0

let conn_counter = Atomic.make 0
let next_conn_id () = Atomic.fetch_and_add conn_counter 1

let debug = try Sys.getenv "FENNEC_WIRE_DEBUG" = "1" with Not_found -> false
let trace fmt = if debug then Printf.eprintf ("[wire] " ^^ fmt ^^ "\n%!") else Printf.ifprintf stderr fmt

(* a command's per-database authorization requirement: [`Write] mutates, [`Read] observes, [`Exempt] is
   handshake / auth / server-admin (no per-database check). This classification IS the authz boundary, so
   it lives next to the dispatch and is unit-tested — a mutating command MUST appear here or it would slip
   past authorization. *)
let access_of = function
  | "insert" | "update" | "delete" | "findAndModify" | "createIndexes" | "dropIndexes" | "drop" | "create"
  | "dropDatabase" ->
    `Write
  | "find" | "getMore" | "count" | "distinct" | "aggregate" | "explain" | "listCollections" | "listIndexes" ->
    `Read
  | _ -> `Exempt

(* ---- the server ---------------------------------------------------------------------------- *)

module Make (Db : DB) = struct
  type cursor = { ns : string; mutable remaining : B.t list }

  type conn = {
    id : int;
    peer : string;
    mutable user : string option;            (* Some once SCRAM-authenticated *)
    mutable scram : (Scram.t * string) option;  (* in-flight conversation + its username *)
    mutable conv_id : int;
    cursors : (int64, cursor) Hashtbl.t;
    mutable next_cursor : int64;
  }

  let fresh_conn ~peer =
    { id = next_conn_id (); peer; user = None; scram = None; conv_id = 0; cursors = Hashtbl.create 8; next_cursor = 1L }

  (* server-level observability (this server instance): uptime, live connections, and per-type op tallies —
     read by [serverStatus]. A plain atomic incr per command/connection, never per record. *)
  let start_time = Unix.gettimeofday ()
  let active_conns = Atomic.make 0
  let op_insert = Atomic.make 0
  let op_query = Atomic.make 0
  let op_update = Atomic.make 0
  let op_delete = Atomic.make 0
  let op_command = Atomic.make 0

  let bump = function
    | "insert" -> Atomic.incr op_insert
    | "find" | "getMore" -> Atomic.incr op_query
    | "update" -> Atomic.incr op_update
    | "delete" -> Atomic.incr op_delete
    | _ -> Atomic.incr op_command

  let server_status config =
    let up = Unix.gettimeofday () -. start_time in
    let cur = Atomic.get active_conns in
    B.Document
      [ ("host", B.String "burrow"); ("version", B.String "6.0.0"); ("process", B.String "burrow");
        ("uptime", B.Float up); ("uptimeMillis", B.Int64 (Int64.of_float (up *. 1000.)));
        ( "connections",
          B.Document
            [ ("current", B.Int cur); ("available", B.Int (max 0 (config.max_connections - cur)));
              ("totalCreated", B.Int (Atomic.get conn_counter)) ] );
        ( "opcounters",
          B.Document
            [ ("insert", B.Int (Atomic.get op_insert)); ("query", B.Int (Atomic.get op_query));
              ("update", B.Int (Atomic.get op_update)); ("delete", B.Int (Atomic.get op_delete));
              ("command", B.Int (Atomic.get op_command)) ] );
        ok ]

  (* split a result set into the first/next batch, registering a cursor for the remainder *)
  let batch_reply conn ~ns ~field ~size docs =
    let first, rest = split_at size docs in
    let id =
      if rest = [] then 0L
      else begin
        let id = conn.next_cursor in
        conn.next_cursor <- Int64.add id 1L;
        Hashtbl.replace conn.cursors id { ns; remaining = rest };
        id
      end
    in
    B.Document [ ("cursor", B.Document [ ("id", B.Int64 id); ("ns", B.String ns); (field, B.Array first) ]); ok ]

  let hello config conn cmd =
    let mechs =
      if config.require_auth && dget cmd "saslSupportedMechs" <> None then
        [ ("saslSupportedMechs", B.Array [ B.String "SCRAM-SHA-256" ]) ]
      else []
    in
    B.Document
      ([ ("isWritablePrimary", B.Bool true);
         ("ismaster", B.Bool true);
         ("maxBsonObjectSize", B.Int 16777216);
         ("maxMessageSizeBytes", B.Int config.max_message_bytes);
         ("maxWriteBatchSize", B.Int 100000);
         ("localTime", B.Date (now_ms ()));
         ("logicalSessionTimeoutMinutes", B.Int 30);
         ("connectionId", B.Int conn.id);
         ("minWireVersion", B.Int 0);
         ("maxWireVersion", B.Int 17);
         ("readOnly", B.Bool config.read_only) ]
      @ mechs
      @ [ ok ])

  let build_info () =
    B.Document
      [ ("version", B.String "6.0.0");
        ("versionArray", B.Array [ B.Int 6; B.Int 0; B.Int 0; B.Int 0 ]);
        ("gitVersion", B.String "fennec-burrow");
        ("maxBsonObjectSize", B.Int 16777216);
        ("bits", B.Int 64);
        ("debug", B.Bool false);
        ok ]

  (* ---- SCRAM-SHA-256 over saslStart / saslContinue ---- *)

  let sasl_payload cmd =
    match dget cmd "payload" with
    | Some (B.Binary { base64; _ }) -> ( try Base64.decode_exn base64 with _ -> raise (Scram.Auth_failed "malformed payload"))
    | Some (B.String s) -> s
    | _ -> raise (Scram.Auth_failed "missing payload")

  let reply_sasl ~conv ~done_ payload =
    B.Document
      [ ("conversationId", B.Int conv);
        ("done", B.Bool done_);
        ("payload", B.Binary { subtype = "00"; base64 = Base64.encode_string payload });
        ok ]

  let sasl_start config conn cmd =
    match dstr cmd "mechanism" with
    | Some "SCRAM-SHA-256" ->
      let payload = sasl_payload cmd in
      let user, server_first, st = Scram.server_first ~lookup:config.lookup ~server_nonce:(config.server_nonce ()) payload in
      conn.scram <- Some (st, user);
      conn.conv_id <- conn.conv_id + 1;
      reply_sasl ~conv:conn.conv_id ~done_:false server_first
    | _ -> err ~code:2 "unsupported authentication mechanism (only SCRAM-SHA-256)"

  let sasl_continue config conn cmd =
    match conn.scram with
    | None -> err ~code:2 "no SCRAM conversation in progress"
    | Some (st, user) ->
      let server_final = Scram.server_final st (sasl_payload cmd) in
      conn.user <- Some user;
      conn.scram <- None;
      config.audit { Audit.at = Unix.gettimeofday (); peer = conn.peer; user = Some user; outcome = Authenticated };
      reply_sasl ~conv:conn.conv_id ~done_:true server_final

  (* ---- data commands ---- *)

  let empty = B.Document []

  let do_find conn cmd db coll =
    let c = Db.collection ~db ~name:coll in
    let limit = Option.value ~default:0 (dint cmd "limit") in
    let docs =
      Db.find c ~selector:(ddoc cmd "filter") ~sort:(ddoc cmd "sort") ~skip:(Option.value ~default:0 (dint cmd "skip"))
        ~limit:(abs limit) ~fields:(ddoc cmd "projection")
    in
    let one_batch = limit < 0 || dbool cmd "singleBatch" in
    let size = if one_batch then max_int else Option.value ~default:101 (dint cmd "batchSize") in
    batch_reply conn ~ns:(db ^ "." ^ coll) ~field:"firstBatch" ~size docs

  let do_get_more conn cmd =
    let id =
      match dget cmd "getMore" with Some (B.Int64 n) -> n | Some (B.Int n) -> Int64.of_int n | _ -> 0L
    in
    let size = Option.value ~default:101 (dint cmd "batchSize") in
    match Hashtbl.find_opt conn.cursors id with
    | None -> err ~code:43 "cursor not found"
    | Some cur ->
      let first, rest = split_at size cur.remaining in
      let id' = if rest = [] then (Hashtbl.remove conn.cursors id; 0L) else (cur.remaining <- rest; id) in
      B.Document [ ("cursor", B.Document [ ("id", B.Int64 id'); ("ns", B.String cur.ns); ("nextBatch", B.Array first) ]); ok ]

  let do_kill_cursors conn cmd =
    let ids = List.filter_map (function B.Int64 n -> Some n | B.Int n -> Some (Int64.of_int n) | _ -> None) (darr cmd "cursors") in
    List.iter (Hashtbl.remove conn.cursors) ids;
    B.Document
      [ ("cursorsKilled", B.Array (List.map (fun n -> B.Int64 n) ids));
        ("cursorsNotFound", B.Array []);
        ("cursorsAlive", B.Array []);
        ("cursorsUnknown", B.Array []);
        ok ]

  let do_count cmd db coll =
    let c = Db.collection ~db ~name:coll in
    B.Document [ ("n", B.Int (Db.count c (ddoc cmd "query"))); ok ]

  (* a dup-key error carries Mongo code 11000; anything else is a generic write error (code 2) *)
  let write_error i e =
    let s = Printexc.to_string e in
    let dup = contains_ci s "duplicate" in
    B.Document [ ("index", B.Int i); ("code", B.Int (if dup then 11000 else 2)); ("errmsg", B.String s) ]

  let do_insert cmd db coll =
    let c = Db.collection ~db ~name:coll in
    let n = ref 0 and werrs = ref [] in
    List.iteri (fun i d -> try ignore (Db.insert c d); incr n with e -> werrs := write_error i e :: !werrs) (darr cmd "documents");
    B.Document (("n", B.Int !n) :: (if !werrs = [] then [] else [ ("writeErrors", B.Array (List.rev !werrs)) ]) @ [ ok ])

  let do_update cmd db coll =
    let c = Db.collection ~db ~name:coll in
    let n = ref 0 in
    List.iter
      (fun u -> n := !n + Db.update c ~multi:(dbool u "multi") ~upsert:(dbool u "upsert") (ddoc u "q") (ddoc u "u"))
      (darr cmd "updates");
    B.Document [ ("n", B.Int !n); ("nModified", B.Int !n); ok ]

  let do_delete cmd db coll =
    let c = Db.collection ~db ~name:coll in
    let n = ref 0 in
    List.iter
      (fun d ->
        let q = ddoc d "q" in
        (* limit:1 is deleteOne — remove exactly the first match by its _id; limit:0 removes all *)
        let removed =
          if dint d "limit" = Some 1 then
            match Db.find c ~selector:q ~sort:empty ~skip:0 ~limit:1 ~fields:empty with
            | doc :: _ -> ( match B.get doc "_id" with Some id -> Db.remove c (B.Document [ ("_id", id) ]) | None -> 0)
            | [] -> 0
          else Db.remove c q
        in
        n := !n + removed)
      (darr cmd "deletes");
    B.Document [ ("n", B.Int !n); ok ]

  let do_aggregate conn cmd db coll =
    let c = Db.collection ~db ~name:coll in
    let results = Db.aggregate c (darr cmd "pipeline") in
    let size = match dget cmd "cursor" with Some cur -> Option.value ~default:101 (dint cur "batchSize") | None -> 101 in
    batch_reply conn ~ns:(db ^ "." ^ coll) ~field:"firstBatch" ~size results

  let do_distinct cmd db coll =
    let c = Db.collection ~db ~name:coll in
    B.Document [ ("values", B.Array (Db.distinct c (Option.value ~default:"" (dstr cmd "key")) (ddoc cmd "query"))); ok ]

  let id_index = B.Document [ ("v", B.Int 2); ("key", B.Document [ ("_id", B.Int 1) ]); ("name", B.String "_id_") ]

  let do_list_collections config db =
    let docs =
      List.map
        (fun nm ->
          B.Document
            [ ("name", B.String nm);
              ("type", B.String "collection");
              ("options", B.Document []);
              ("info", B.Document [ ("readOnly", B.Bool config.read_only) ]);
              ("idIndex", id_index) ])
        (Db.collection_names ~db)
    in
    B.Document
      [ ("cursor", B.Document [ ("id", B.Int64 0L); ("ns", B.String (db ^ ".$cmd.listCollections")); ("firstBatch", B.Array docs) ]); ok ]

  let do_list_indexes db coll =
    let c = Db.collection ~db ~name:coll in
    let specs =
      List.map
        (fun (nm, keys, uniq) ->
          B.Document
            ([ ("v", B.Int 2);
               ("key", B.Document (List.map (fun (f, d) -> (f, B.Int d)) keys));
               ("name", B.String nm) ]
            @ (if uniq then [ ("unique", B.Bool true) ] else [])))
        (Db.index_specs c)
    in
    B.Document
      [ ("cursor", B.Document [ ("id", B.Int64 0L); ("ns", B.String (db ^ "." ^ coll)); ("firstBatch", B.Array (id_index :: specs)) ]); ok ]

  let do_create_indexes cmd db coll =
    let c = Db.collection ~db ~name:coll in
    let before = List.length (Db.index_specs c) + 1 in
    List.iter
      (fun ix ->
        let keys = ddoc ix "key" in
        Db.ensure_index c ~name:(Option.value ~default:(default_index_name keys) (dstr ix "name")) ~keys
          ~unique:(dbool ix "unique") ~sparse:(dbool ix "sparse"))
      (darr cmd "indexes");
    let after = List.length (Db.index_specs c) + 1 in
    B.Document [ ("createdCollectionAutomatically", B.Bool false); ("numIndexesBefore", B.Int before); ("numIndexesAfter", B.Int after); ok ]

  let do_drop_indexes cmd db coll =
    let c = Db.collection ~db ~name:coll in
    let was = List.length (Db.index_specs c) + 1 in
    (match dget cmd "index" with
     | Some (B.String "*") -> List.iter (fun (nm, _, _) -> Db.drop_index c ~name:nm) (Db.index_specs c)
     | Some (B.String nm) -> Db.drop_index c ~name:nm
     | _ -> ());
    B.Document [ ("nIndexesWas", B.Int was); ok ]

  let do_drop db coll =
    let c = Db.collection ~db ~name:coll in
    let was = List.length (Db.index_specs c) + 1 in
    Db.drop_collection ~db ~name:coll;
    B.Document [ ("ns", B.String (db ^ "." ^ coll)); ("nIndexesWas", B.Int was); ok ]

  let do_list_databases () =
    let dbs =
      List.map (fun nm -> B.Document [ ("name", B.String nm); ("sizeOnDisk", B.Float 0.); ("empty", B.Bool false) ]) (Db.database_names ())
    in
    B.Document [ ("databases", B.Array dbs); ("totalSize", B.Float 0.); ok ]

  let get_parameter cmd =
    match dget cmd "featureCompatibilityVersion" with
    | Some _ -> B.Document [ ("featureCompatibilityVersion", B.Document [ ("version", B.String "6.0") ]); ok ]
    | None -> B.Document [ ok ]

  let connection_status conn =
    let users = match conn.user with Some u -> [ B.Document [ ("user", B.String u); ("db", B.String "admin") ] ] | None -> [] in
    B.Document [ ("authInfo", B.Document [ ("authenticatedUsers", B.Array users); ("authenticatedUserRoles", B.Array []) ]); ok ]

  (* commands runnable before authentication completes *)
  (* explain: return the winningPlan the planner would choose for the wrapped find, WITHOUT executing it *)
  let do_explain cmd db =
    let inner = ddoc cmd "explain" in
    let coll = Option.value ~default:"" (dstr inner "find") in
    let winning = Db.explain (Db.collection ~db ~name:coll) ~selector:(ddoc inner "filter") ~sort:(ddoc inner "sort") in
    B.Document
      [ ("queryPlanner", B.Document [ ("namespace", B.String (db ^ "." ^ coll)); ("winningPlan", winning) ]);
        ("ok", B.Float 1.0) ]

  let pre_auth = [ "hello"; "isMaster"; "ismaster"; "ping"; "saslStart"; "saslContinue"; "logout"; "endSessions" ]

  (* per-user authorization: a [`Write]/[`Read] command needs the matching access on its db; the rest is
     exempt. Reached only when [require_auth] and the user is authenticated (the presence check ran). *)
  let authz_ok config conn db name =
    match access_of name with
    | `Exempt -> true
    | `Read -> config.authorize (Option.value ~default:"" conn.user) ~write:false ~db
    | `Write -> config.authorize (Option.value ~default:"" conn.user) ~write:true ~db

  let coll_of cmd name = match dget cmd name with Some (B.String c) -> c | _ -> ""

  let handle conn config cmd : B.t =
    let name = Option.value ~default:"" (Frame.command_name cmd) in
    bump name;
    let db = Option.value ~default:"" (Frame.db cmd) in
    let coll () = coll_of cmd name in
    let guard_write f = if config.read_only then err ~code:13 "not authorized: server is read-only" else f () in
    if config.require_auth && conn.user = None && not (List.mem name pre_auth) then
      err ~code:13 (Printf.sprintf "command %s requires authentication" name)
    else if config.require_auth && not (authz_ok config conn db name) then begin
      config.audit { Audit.at = Unix.gettimeofday (); peer = conn.peer; user = conn.user; outcome = Denied (name, db) };
      err ~code:13 (Printf.sprintf "not authorized on %s to execute command '%s'" db name)
    end
    else
      match name with
      | "hello" | "isMaster" | "ismaster" -> hello config conn cmd
      | "ping" -> B.Document [ ok ]
      | "saslStart" -> sasl_start config conn cmd
      | "saslContinue" -> sasl_continue config conn cmd
      | "logout" -> conn.user <- None; B.Document [ ok ]
      | "buildInfo" | "buildinfo" -> build_info ()
      | "find" -> do_find conn cmd db (coll ())
      | "getMore" -> do_get_more conn cmd
      | "killCursors" -> do_kill_cursors conn cmd
      | "count" -> do_count cmd db (coll ())
      | "insert" -> guard_write (fun () -> do_insert cmd db (coll ()))
      | "update" -> guard_write (fun () -> do_update cmd db (coll ()))
      | "delete" -> guard_write (fun () -> do_delete cmd db (coll ()))
      | "aggregate" -> do_aggregate conn cmd db (coll ())
      | "distinct" -> do_distinct cmd db (coll ())
      | "explain" -> do_explain cmd db
      | "listCollections" -> do_list_collections config db
      | "listIndexes" -> do_list_indexes db (coll ())
      | "createIndexes" -> guard_write (fun () -> do_create_indexes cmd db (coll ()))
      | "dropIndexes" -> guard_write (fun () -> do_drop_indexes cmd db (coll ()))
      | "drop" -> guard_write (fun () -> do_drop db (coll ()))
      | "create" -> guard_write (fun () -> ignore (Db.collection ~db ~name:(coll ())); B.Document [ ok ])
      | "dropDatabase" -> guard_write (fun () -> B.Document [ ("dropped", B.String db); ok ])
      | "listDatabases" -> do_list_databases ()
      | "getParameter" -> get_parameter cmd
      | "serverStatus" -> server_status config
      | "getLog" -> B.Document [ ("totalLinesWritten", B.Int 0); ("log", B.Array []); ok ]
      | "connectionStatus" -> connection_status conn
      | "whatsmyuri" -> B.Document [ ("you", B.String conn.peer); ok ]
      | "getCmdLineOpts" -> B.Document [ ("argv", B.Array []); ("parsed", B.Document []); ok ]
      | "hostInfo" | "endSessions" | "refreshSessions" | "getDefaultRWConcern" -> B.Document [ ok ]
      | other -> err ~code:59 (Printf.sprintf "no such command: '%s'" other)

  (* ---- the connection loop + accept loop ---- *)

  let read_message r ~max =
    let hdr = Eio.Buf_read.take 4 r in
    let len = Frame.message_length hdr in
    if len < 16 || len > max then raise (Frame.Protocol_error "bad message length");
    hdr ^ Eio.Buf_read.take (len - 4) r

  let serve_conn config flow addr =
    let r = Eio.Buf_read.of_flow flow ~max_size:(config.max_message_bytes + 1024) in
    let conn = fresh_conn ~peer:(Format.asprintf "%a" Eio.Net.Sockaddr.pp addr) in
    trace "conn %d from %s" conn.id conn.peer;
    Atomic.incr active_conns;
    let rec loop () =
      let inc = Frame.parse (read_message r ~max:config.max_message_bytes) in
      trace "conn %d <- %s (db=%s, more_to_come=%b)" conn.id (Option.value ~default:"?" (Frame.command_name inc.command))
        (Option.value ~default:"" (Frame.db inc.command)) inc.more_to_come;
      let reply_doc =
        try handle conn config inc.command with
        | Scram.Auth_failed _ ->
          config.audit
            { Audit.at = Unix.gettimeofday (); peer = conn.peer;
              user = (match conn.scram with Some (_, u) -> Some u | None -> None); outcome = Auth_failed };
          err ~code:18 "Authentication failed."
        | Bw.Unsupported m -> err ~code:2 m
        | e -> err ~code:1 ("internal error: " ^ Printexc.to_string e)
      in
      trace "conn %d -> ok=%s" conn.id (match dget reply_doc "ok" with Some (B.Float f) -> string_of_float f | _ -> "?");
      if not inc.more_to_come then Eio.Flow.copy_string (Frame.reply ~response_to:inc.request_id inc.reply_kind reply_doc) flow;
      loop ()
    in
    Fun.protect
      ~finally:(fun () -> Atomic.decr active_conns)
      (fun () -> try loop () with e -> trace "conn %d closed: %s" conn.id (Printexc.to_string e))  (* EOF / bad frame / reset *)

  let run ~sw ~net ~addr config =
    let socket = Eio.Net.listen ~sw ~backlog:128 ~reuse_addr:true net addr in
    let handle_flow flow addr =
      match config.tls with
      | None -> serve_conn config flow addr
      | Some cfg -> ( match (try Some (Tls_eio.server_of_flow cfg flow) with _ -> None) with Some tls -> serve_conn config tls addr | None -> ())
    in
    let on_error e =
      let s = Printexc.to_string e in
      let gone = e = End_of_file || contains_ci s "reset" || contains_ci s "broken pipe" || contains_ci s "epipe" in
      if not gone then Printf.eprintf "fennec mongo-wire: connection error: %s\n%!" s
    in
    (* a daemon fiber: it serves in the background without keeping [sw] alive, and is cancelled cleanly
       when the switch ends (app shutdown / test teardown) — so [expose] returns immediately and the
       endpoint lives exactly as long as the server *)
    Eio.Fiber.fork_daemon ~sw (fun () ->
        (try Eio.Net.run_server ~max_connections:config.max_connections ~on_error socket handle_flow
         with Eio.Cancel.Cancelled _ -> ());
        `Stop_daemon)
end
