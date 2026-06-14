module Store = Burrow_store.Store
module Bin = Burrow_binary
module B = Bson

type index = { iname : string; keys : (string * int) list; unique : bool; db : Store.db }

type collection = {
  name : string;
  records : Record.t;
  mutable indexes : index list;
  mutable validator : B.t option;
}

type t = { store : Store.t; meta : Store.db; colls : (string, collection) Hashtbl.t }

let store t = t.store

(* meta-db key layout: a 1-char tag, NUL, then NUL-separated names. These are KEYS (length-delimited
   MDB_vals), so embedded NULs are fine. *)
let ckey name = "c\000" ^ name
let ikey coll iname = "i\000" ^ coll ^ "\000" ^ iname
let vkey coll = "v\000" ^ coll

(* Sub-DB NAMES, by contrast, are passed to mdb_dbi_open as C strings (NUL-terminated), so they MUST
   be NUL-free or they'd truncate and collide (every "idx\000..." -> "idx"). Length-prefix the
   collection so the (coll, iname) split is unambiguous regardless of separator characters. *)
let rec_db_name coll = "rec:" ^ coll
let idx_db_name coll iname = Printf.sprintf "idx:%d:%s:%s" (String.length coll) coll iname

let parse_keys : B.t -> (string * int) list = function
  | B.Document kvs ->
    List.map
      (fun (f, v) ->
        ( f,
          match v with
          | B.Int n -> n
          | B.Int64 n -> Int64.to_int n
          | B.Float x -> int_of_float x
          | _ -> 1 ))
      kvs
  | _ -> []

let keys_doc (keys : (string * int) list) : B.t = B.Document (List.map (fun (f, d) -> (f, B.Int d)) keys)
let index_spec_doc keys unique = B.Document [ ("keys", keys_doc keys); ("unique", B.Bool unique) ]

let open_ store =
  let meta = Store.db store "meta" in
  let t = { store; meta; colls = Hashtbl.create 16 } in
  (* phase 1: scan meta into plain data (read txn; no sub-DB opens inside it) *)
  let coll_names = ref [] and idx_entries = ref [] and validators = ref [] in
  Store.read store (fun txn ->
      Store.iter txn meta (fun ~key ~data ->
          let n = String.length key in
          if n >= 2 && key.[1] = '\000' then begin
            let rest = String.sub key 2 (n - 2) in
            match key.[0] with
            | 'c' -> coll_names := rest :: !coll_names
            | 'i' -> (
              match String.index_opt rest '\000' with
              | Some j ->
                let coll = String.sub rest 0 j
                and iname = String.sub rest (j + 1) (String.length rest - j - 1) in
                idx_entries := (coll, iname, data) :: !idx_entries
              | None -> ())
            | 'v' -> validators := (rest, data) :: !validators
            | _ -> ()
          end;
          true));
  (* phase 2: open record sub-DBs (outside the read txn) *)
  List.iter
    (fun name ->
      let records = Record.make (Store.db store (rec_db_name name)) in
      Hashtbl.replace t.colls name { name; records; indexes = []; validator = None })
    !coll_names;
  (* phase 3: attach indexes *)
  List.iter
    (fun (coll, iname, data) ->
      match Hashtbl.find_opt t.colls coll with
      | None -> ()
      | Some c ->
        let spec = Bin.decode data in
        let kd = Option.value ~default:(B.Document []) (B.get spec "keys") in
        let unique = match B.get spec "unique" with Some (B.Bool b) -> b | _ -> false in
        let db = Store.db store (idx_db_name coll iname) in
        c.indexes <- { iname; keys = parse_keys kd; unique; db } :: c.indexes)
    !idx_entries;
  (* phase 4: validators *)
  List.iter
    (fun (coll, data) ->
      match Hashtbl.find_opt t.colls coll with Some c -> c.validator <- Some (Bin.decode data) | None -> ())
    !validators;
  t

let collection_opt t name = Hashtbl.find_opt t.colls name
let collections t = Hashtbl.fold (fun _ c acc -> c :: acc) t.colls []

let collection t name =
  match Hashtbl.find_opt t.colls name with
  | Some c -> c
  | None ->
    let records = Record.make (Store.db t.store (rec_db_name name)) in
    Store.write t.store (fun txn -> Store.put txn t.meta (ckey name) "");
    let c = { name; records; indexes = []; validator = None } in
    Hashtbl.replace t.colls name c;
    c

let index_names c = List.map (fun i -> i.iname) c.indexes

let ensure_index t c ~name ~keys ~unique =
  if List.exists (fun i -> i.iname = name) c.indexes then None
  else begin
    let parsed = parse_keys keys in
    let db = Store.db t.store (idx_db_name c.name name) in
    Store.write t.store (fun txn ->
        Store.put txn t.meta (ikey c.name name) (Bin.encode (index_spec_doc parsed unique)));
    let idx = { iname = name; keys = parsed; unique; db } in
    c.indexes <- idx :: c.indexes;
    Some idx
  end

let drop_index t c ~name =
  match List.find_opt (fun i -> i.iname = name) c.indexes with
  | None -> ()
  | Some idx ->
    c.indexes <- List.filter (fun i -> i.iname <> name) c.indexes;
    Store.write t.store (fun txn ->
        ignore (Store.del txn t.meta (ikey c.name name));
        Store.clear txn idx.db)

let validator c = c.validator

let set_validator t c v =
  c.validator <- v;
  Store.write t.store (fun txn ->
      match v with
      | Some schema -> Store.put txn t.meta (vkey c.name) (Bin.encode schema)
      | None -> ignore (Store.del txn t.meta (vkey c.name)))
