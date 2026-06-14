module Store = Burrow_store.Store
module Bin = Burrow_binary
module Key_codec = Burrow_codec.Key_codec

type t = Store.db

let make db = db

let key_of_id (id : Bson.t) : string =
  try Key_codec.encode id
  with Key_codec.Unsupported s -> invalid_arg ("Burrow: unsupported _id type (" ^ s ^ ")")

let get txn (t : t) ~id = Option.map Bin.decode (Store.get txn t (key_of_id id))
let get_by_key txn (t : t) key = Option.map Bin.decode (Store.get txn t key)
let mem txn (t : t) ~id = Store.get txn t (key_of_id id) <> None
let put txn (t : t) ~id doc = Store.put txn t (key_of_id id) (Bin.encode doc)
let delete txn (t : t) ~id = Store.del txn t (key_of_id id)

let iter txn (t : t) f = Store.iter txn t (fun ~key ~data -> f ~id_key:key ~doc:(Bin.decode data))
