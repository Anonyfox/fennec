let mongo_url_env = "MONGO_URL"
let memory_url = ":memory:"
let default_db = "fennec"
let missing_warning_printed = Atomic.make false

type state =
  | Missing
  | Memory
  | Mongo of { uri : string; db : string }

let trimmed_env name =
  match Sys.getenv_opt name with
  | Some value ->
    let value = String.trim value in
    if value = "" then None else Some value
  | None -> None

let url () = trimmed_env mongo_url_env

let db () =
  match trimmed_env "FENNEC_DB" with
  | Some db -> db
  | None -> default_db

let is_memory_url url = String.trim url = memory_url

let state () =
  match url () with
  | None -> Missing
  | Some uri when is_memory_url uri -> Memory
  | Some uri -> Mongo { uri; db = db () }

let unavailable_message () =
  "MONGO_URL is not set. Database-backed Fennec features are unavailable in this process. Set \
   MONGO_URL for production, run through `fennec dev` for an auto-managed local MongoDB when \
   mongod is installed, or set MONGO_URL=:memory: explicitly for tests."

let warn_if_missing () =
  match state () with
  | Missing ->
    if Atomic.compare_and_set missing_warning_printed false true then
      Printf.eprintf "fennec: WARNING: %s\n%!" (unavailable_message ())
  | Memory | Mongo _ -> ()
