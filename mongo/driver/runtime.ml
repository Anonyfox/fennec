(* The one database knob: MONGO_URL. This module parses it — once — into a structured backend choice.
   Three schemes, mirroring the SQLite -> server progression:
     :memory:                              the in-process Mongo-shaped store (minimongo); ephemeral
     burrow://[user:pass@][host[:port]]/<abs-path>[?tls&readonly]   the embedded on-disk engine
     mongodb://… (or mongodb+srv://…)      a real MongoDB server, via the native driver

   burrow:// is the mirror image of mongodb://: a mongodb:// URL is the coordinates of a server you
   CONNECT to; a burrow:// URL is the coordinates an embedded server you HOST listens on, plus the
   filesystem path where its data lives. So the authority drives the optional mongosh wire endpoint
   (absent authority => storage only), and the path's trailing segment is the database name AND its
   on-disk directory (so `mongosh use other` maps to a sibling dir under the same base). Paths are
   absolute. The whole module is pure string parsing — no IO, no driver. *)

let mongo_url_env = "MONGO_URL"
let memory_url = ":memory:"
let default_db = "fennec"
let burrow_scheme = "burrow://"
let default_wire_port = 27017
let default_bind = "127.0.0.1"
let missing_warning_printed = Atomic.make false

(* the optional mongosh wire endpoint a burrow:// authority asks for *)
type expose = {
  host : string;  (** bind address (default 127.0.0.1) *)
  port : int;  (** wire port (default 27017) *)
  user : string option;  (** SCRAM username — presence of creds => auth required *)
  pass : string option;
  tls : bool;  (** [?tls=true] — reuse the app's TLS certificate *)
  read_only : bool;  (** [?readonly=true] *)
}

type backend =
  | Missing
  | Memory
  | Burrow of { base : string; db : string; expose : expose option }  (** engine dir = [base]/[db] *)
  | Mongo of { uri : string; db : string }

let trimmed_env name =
  match Sys.getenv_opt name with
  | Some value ->
    let value = String.trim value in
    if value = "" then None else Some value
  | None -> None

let url () = trimmed_env mongo_url_env
let db () = match trimmed_env "FENNEC_DB" with Some db -> db | None -> default_db
let is_memory_url url = String.trim url = memory_url

let starts_with pfx s = String.length s >= String.length pfx && String.sub s 0 (String.length pfx) = pfx

(* index of the first occurrence of [sub] in [s], or None *)
let index_of s sub =
  let ls = String.length s and lsub = String.length sub in
  let rec go i = if i + lsub > ls then None else if String.sub s i lsub = sub then Some i else go (i + 1) in
  go 0

(* ---- burrow:// ---------------------------------------------------------------------------- *)

(* "tls=true&readonly=1" -> (tls, read_only); a bare flag or =true/=1 counts as set *)
let parse_query q =
  let set k = List.exists (fun p -> p = k || p = k ^ "=true" || p = k ^ "=1") (String.split_on_char '&' q) in
  (set "tls", set "readonly")

(* "[user:pass@][host][:port]" -> an expose record (tls/read_only filled in by the caller) *)
let parse_authority a =
  let creds, hostport =
    match String.rindex_opt a '@' with
    | Some i -> (Some (String.sub a 0 i), String.sub a (i + 1) (String.length a - i - 1))
    | None -> (None, a)
  in
  let host, port =
    match String.rindex_opt hostport ':' with
    | Some i ->
      let h = String.sub hostport 0 i and p = String.sub hostport (i + 1) (String.length hostport - i - 1) in
      ((if h = "" then default_bind else h), Option.value (int_of_string_opt p) ~default:default_wire_port)
    | None -> ((if hostport = "" then default_bind else hostport), default_wire_port)
  in
  let user, pass =
    match creds with
    | None -> (None, None)
    | Some c -> (
      match String.index_opt c ':' with
      | Some i -> (Some (String.sub c 0 i), Some (String.sub c (i + 1) (String.length c - i - 1)))
      | None -> (Some c, None))
  in
  { host; port; user; pass; tls = false; read_only = false }

(* [rest] is everything after "burrow://": "[authority]/<abs-path>[?query]" *)
let parse_burrow rest =
  let authority, pathq =
    match String.index_opt rest '/' with
    | Some i -> (String.sub rest 0 i, String.sub rest i (String.length rest - i))
    | None -> (rest, "")
  in
  let path, query =
    match String.index_opt pathq '?' with
    | Some i -> (String.sub pathq 0 i, String.sub pathq (i + 1) (String.length pathq - i - 1))
    | None -> (pathq, "")
  in
  let tls, read_only = parse_query query in
  let expose = if authority = "" then None else Some { (parse_authority authority) with tls; read_only } in
  let path = if path = "" then "/" else path in
  Burrow { base = Filename.dirname path; db = Filename.basename path; expose }

(* ---- mongodb:// db ------------------------------------------------------------------------ *)

(* the database in "mongodb://[auth@]hosts/<db>[?opts]", else the FENNEC_DB / default fallback *)
let mongo_db_of_uri uri ~default =
  match index_of uri "://" with
  | None -> default
  | Some i -> (
    let after = String.sub uri (i + 3) (String.length uri - i - 3) in
    match String.index_opt after '/' with
    | None -> default
    | Some j ->
      let dbq = String.sub after (j + 1) (String.length after - j - 1) in
      let db = match String.index_opt dbq '?' with Some k -> String.sub dbq 0 k | None -> dbq in
      if db = "" then default else db)

(* ---- the one classifier ------------------------------------------------------------------- *)

let backend () =
  match url () with
  | None -> Missing
  | Some u when is_memory_url u -> Memory
  | Some u when starts_with burrow_scheme u ->
    parse_burrow (String.sub u (String.length burrow_scheme) (String.length u - String.length burrow_scheme))
  | Some uri -> Mongo { uri; db = mongo_db_of_uri uri ~default:(db ()) }

let unavailable_message () =
  "MONGO_URL is not set. Database-backed Fennec features are unavailable in this process. Set MONGO_URL \
   for production (a mongodb:// URI, or burrow://[user:pass@host:port]/abs/path for the in-process \
   engine), run through `fennec dev` (which defaults to the embedded engine — no mongod needed), or set \
   MONGO_URL=:memory: explicitly for tests."

let warn_if_missing () =
  match backend () with
  | Missing ->
    if Atomic.compare_and_set missing_warning_printed false true then
      Printf.eprintf "fennec: WARNING: %s\n%!" (unavailable_message ())
  | Memory | Burrow _ | Mongo _ -> ()
