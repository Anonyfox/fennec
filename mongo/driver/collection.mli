(** Collection-scoped operations — CRUD, aggregation, and the everyday admin/reset primitives, all
    over libmongoc and off the Eio scheduler. Replies are the driver's raw result documents.

    {[
      let users = create client ~db:"app" ~name:"users" in
      ignore (insert_one users (Bson.doc [ ("name", Bson.str "ada"); ("age", Bson.int 36) ]));
      ignore (update_one users
                ~filter:(Bson.doc [ ("name", Bson.str "ada") ])
                ~update:(Bson.doc [ ("$inc", Bson.doc [ ("age", Bson.int 1) ]) ]));
      match find_one users ~filter:(Bson.doc [ ("name", Bson.str "ada") ]) () with
      | Some doc -> Bson.get_int doc "age"   (* Some 37 *)
      | None -> None
    ]} *)

(** A collection handle: a client bundled with a db + collection name. *)
type t = { client : Client.t; db : string; name : string }

(** [create client ~db ~name] names a collection on [client]. *)
val create : Client.t -> db:string -> name:string -> t

(** [find t ?filter ?opts ()] returns the matching documents. [opts] carries projection/sort/skip/
    limit as a document (e.g. [{ "limit": 10, "sort": {…} }]). *)
val find : t -> ?filter:Bson.t -> ?opts:Bson.t -> unit -> Bson.t list

(** The first matching document, or [None]. *)
val find_one : t -> ?filter:Bson.t -> unit -> Bson.t option

(** [insert_one t doc] inserts [doc]; returns the driver reply. *)
val insert_one : t -> Bson.t -> Bson.t

(** [update_one t ~filter ~update] applies [update] to the first match; returns the driver reply. *)
val update_one : t -> filter:Bson.t -> update:Bson.t -> Bson.t

(** [delete_one t ~filter] removes the first match; returns the driver reply. *)
val delete_one : t -> filter:Bson.t -> Bson.t

(** [delete_many t ~filter] removes every match in one command (no cap); returns the reply. *)
val delete_many : t -> filter:Bson.t -> Bson.t

(** Empty the collection but keep it (and its indexes). *)
val clear : t -> unit

(** Drop the collection entirely. Idempotent: dropping a missing collection is a no-op. *)
val drop : t -> unit

(** [aggregate t ?pipeline ?opts ()] runs the pipeline (an array of stage documents); returns the
    result documents. *)
val aggregate : t -> ?pipeline:Bson.t -> ?opts:Bson.t -> unit -> Bson.t list

(** [distinct t ~key ?filter ()] — the distinct values of [key] over matching documents. *)
val distinct : t -> key:string -> ?filter:Bson.t -> unit -> Bson.t list

(** [create_index t ~keys ?opts ?name ()] builds an index from the key spec [keys] (e.g.
    [{"a":1,"b":-1}]); a name is derived if not given. Returns the createIndexes reply. *)
val create_index : t -> keys:Bson.t -> ?opts:(string * Bson.t) list -> ?name:string -> unit -> Bson.t

(** [drop_index t ~name] drops an index by name (or ["*"] for all). Idempotent on a missing index. *)
val drop_index : t -> name:string -> unit

(** The collection's indexes, as documents. *)
val list_indexes : t -> Bson.t list

(** [count t ?filter ()] — the number of documents matching [filter]. *)
val count : t -> ?filter:Bson.t -> unit -> int
