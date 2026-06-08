(* Change streams — the reactive primitive. A blocking watch cursor runs in an Eio systhread;
   [next] returns [None] on a server-side timeout (no deadlock) and [Some event] on a real change.
   {!Live} turns that into a Meteor-style livequery fan-out. Requires a replica set (even a single
   node) — see {!Server}. *)

module Mongo_ffi = Fennec_mongo_ffi.Mongo_ffi
module Bson_json = Fennec_mongo_bson_json.Bson_json

type t = Mongo_ffi.change_stream

(* The kind of change, parsed once from the wire's [operationType] string into a closed variant.
   Downstream code matches on this instead of comparing strings, so a typo can't silently misroute
   an event and the compiler enforces that every operation is handled. *)
type operation =
  | Insert
  | Update
  | Replace
  | Delete
  | Invalidate
  | Drop
  | Drop_database
  | Rename
  | Other of string

let operation_of_string = function
  | "insert" -> Insert
  | "update" -> Update
  | "replace" -> Replace
  | "delete" -> Delete
  | "invalidate" -> Invalidate
  | "drop" -> Drop
  | "dropDatabase" -> Drop_database
  | "rename" -> Rename
  | s -> Other s

let string_of_operation = function
  | Insert -> "insert"
  | Update -> "update"
  | Replace -> "replace"
  | Delete -> "delete"
  | Invalidate -> "invalidate"
  | Drop -> "drop"
  | Drop_database -> "dropDatabase"
  | Rename -> "rename"
  | Other s -> s

type event = {
  op : operation; (* the change kind, parsed from operationType *)
  ns : string; (* "db.collection" *)
  document_key : Bson.t; (* { _id: … } *)
  full_document : Bson.t option; (* present for inserts, or with updateLookup *)
  resume_token : Bson.t; (* the change's _id; persist to resume later *)
  raw : Bson.t; (* the whole change event *)
}

let watch (col : Collection.t) ?(pipeline = Bson.Array []) ?(full_document = false) ?(max_await_ms = 1000)
    ?(resume_after = None) () : t =
  let opts =
    Bson.Document
      ([ ("maxAwaitTimeMS", Bson.Int max_await_ms) ]
      @ (if full_document then [ ("fullDocument", Bson.String "updateLookup") ] else [])
      @ (match resume_after with Some tok -> [ ("resumeAfter", tok) ] | None -> []))
  in
  Mongo_ffi.watch_open col.Collection.client.Client.pool col.Collection.db col.Collection.name
    (Bson_json.to_string pipeline) (Bson_json.to_string opts)

let parse raw =
  let b = Bson_json.of_string raw in
  let ns =
    match Bson.get b "ns" with
    | Some ns_doc -> (
        match (Bson.get ns_doc "db", Bson.get ns_doc "coll") with
        | Some (Bson.String d), Some (Bson.String c) -> d ^ "." ^ c
        | _ -> "")
    | _ -> ""
  in
  {
    op = (match Bson.get b "operationType" with Some (Bson.String s) -> operation_of_string s | _ -> Other "unknown");
    ns;
    document_key = Option.value ~default:Bson.Null (Bson.get b "documentKey");
    full_document = Bson.get b "fullDocument";
    resume_token = Option.value ~default:Bson.Null (Bson.get b "_id");
    raw = b;
  }

let next (t : t) : event option = Internal.run (fun () -> Mongo_ffi.watch_next t) |> Option.map parse
let close (t : t) = Mongo_ffi.watch_close t
