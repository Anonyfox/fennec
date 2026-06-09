(* Collection-scoped operations. A [Collection.t] bundles a client with a db+collection name; the
   real work happens in libmongoc, off the scheduler via Internal.run. Replies come back as [Bson.t]
   (the driver's result document).

   The reset helpers (drop / clear / delete_many) are built on Client.command, not new C stubs — they
   are the everyday "give me a clean slate" primitives for tests and dev. *)

module Mongo_ffi = Fennec_mongo_ffi.Mongo_ffi
module Bson_json = Fennec_mongo_bson_json.Bson_json

type t = { client : Client.t; db : string; name : string }

let create client ~db ~name = { client; db; name }

(* substring test for the idempotent-drop guard below *)
let contains ~needle s =
  let nl = String.length needle and sl = String.length s in
  if nl = 0 then true
  else
    let rec go i = i + nl <= sl && (String.sub s i nl = needle || go (i + 1)) in
    go 0

let find t ?(filter = Bson.Document []) ?(opts = Bson.Document []) () =
  Internal.run (fun () ->
      Mongo_ffi.find t.client.Client.pool t.db t.name (Bson_json.to_string filter) (Bson_json.to_string opts))
  |> Bson_json.list_of_string

let find_one t ?(filter = Bson.Document []) () =
  match find t ~filter ~opts:(Bson.Document [ ("limit", Bson.Int 1) ]) () with x :: _ -> Some x | [] -> None

let insert_one t doc =
  Internal.run (fun () -> Mongo_ffi.insert_one t.client.Client.pool t.db t.name (Bson_json.to_string doc))
  |> Bson_json.of_string

let update_one t ~filter ~update =
  Internal.run (fun () ->
      Mongo_ffi.update_one t.client.Client.pool t.db t.name (Bson_json.to_string filter) (Bson_json.to_string update))
  |> Bson_json.of_string

let delete_one t ~filter =
  Internal.run (fun () -> Mongo_ffi.delete_one t.client.Client.pool t.db t.name (Bson_json.to_string filter))
  |> Bson_json.of_string

(* Delete every document matching [filter] in one command (limit:0 = no cap). *)
let delete_many t ~filter =
  Client.command t.client ~db:t.db
    (Bson.Document
       [ ("delete", Bson.String t.name);
         ("deletes", Bson.Array [ Bson.Document [ ("q", filter); ("limit", Bson.Int 0) ] ]) ])

(* Empty the collection but keep it (and its indexes) around. *)
let clear t = ignore (delete_many t ~filter:(Bson.Document []))

(* Drop the collection entirely. Idempotent: dropping a collection that does not exist is a no-op
   rather than an error, which is what reset/teardown code wants. *)
let drop t =
  try ignore (Client.command t.client ~db:t.db (Bson.Document [ ("drop", Bson.String t.name) ]))
  with Failure msg when contains ~needle:"not found" msg -> ()

(* Run an aggregation pipeline. [pipeline] is the array of stages. *)
let aggregate t ?(pipeline = Bson.Array []) ?(opts = Bson.Document []) () =
  Internal.run (fun () ->
      Mongo_ffi.aggregate t.client.Client.pool t.db t.name (Bson_json.to_string pipeline) (Bson_json.to_string opts))
  |> Bson_json.list_of_string

(* Distinct values of [key] over the documents matching [filter]. *)
let distinct t ~key ?(filter = Bson.Document []) () =
  let reply =
    Client.command t.client ~db:t.db
      (Bson.Document [ ("distinct", Bson.String t.name); ("key", Bson.String key); ("query", filter) ])
  in
  match Bson.get reply "values" with Some (Bson.Array xs) -> xs | _ -> []

(* Build index(es). [keys] is the key spec document, e.g. {"a":1,"b":-1}. Returns the createIndexes
   reply. *)
let create_index t ~keys ?(opts = []) ?name () =
  let index_name =
    match name with
    | Some n -> n
    | None -> (
        match keys with
        | Bson.Document kvs ->
            String.concat "_"
              (List.concat_map
                 (fun (k, v) ->
                   let dir =
                     match v with
                     | Bson.Int n -> string_of_int n
                     | Bson.Float f -> string_of_int (int_of_float f)
                     | Bson.String s -> s
                     | _ -> "1"
                   in
                   [ k; dir ])
                 kvs)
        | _ -> "index")
  in
  let spec = Bson.Document (("key", keys) :: ("name", Bson.String index_name) :: opts) in
  Client.command t.client ~db:t.db
    (Bson.Document [ ("createIndexes", Bson.String t.name); ("indexes", Bson.Array [ spec ]) ])

(* Drop an index by name (or "*" for all). Idempotent on a missing index. *)
let drop_index t ~name =
  try
    ignore
      (Client.command t.client ~db:t.db
         (Bson.Document [ ("dropIndexes", Bson.String t.name); ("index", Bson.String name) ]))
  with Failure msg when contains ~needle:"not found" msg -> ()

(* List the collection's indexes as documents. *)
let list_indexes t =
  let reply = Client.command t.client ~db:t.db (Bson.Document [ ("listIndexes", Bson.String t.name) ]) in
  match Bson.get reply "cursor" with
  | Some (Bson.Document c) -> ( match List.assoc_opt "firstBatch" c with Some (Bson.Array xs) -> xs | _ -> [])
  | _ -> []

let count t ?(filter = Bson.Document []) () =
  let reply = Client.command t.client ~db:t.db (Bson.Document [ ("count", Bson.String t.name); ("query", filter) ]) in
  match Bson.get reply "n" with Some (Bson.Int n) -> n | Some (Bson.Float f) -> int_of_float f | _ -> 0
