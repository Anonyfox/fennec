(* Oplog — the engine-wide change log. Each committed write appends one entry PER affected document into the
   capped `_oplog` sub-DB, keyed by a monotonic LSN, WITHIN the same write txn (atomic with the data, no
   extra fsync). Idempotent by construction: an entry carries the RESULTING document (insert / update) or
   just the _id (delete), so applying it downstream is a put-or-delete by _id — safe to replay. Consumers
   (change streams, PITR, replication) tail it from an LSN; entries past the retention cap are trimmed. The
   single writer assigns the LSN with no CAS (the write lock already serializes every append). *)

module Store = Burrow_store.Store
module Bin = Burrow_binary
module B = Bson

type op = Insert | Update | Delete

let op_str = function Insert -> "i" | Update -> "u" | Delete -> "d"
let op_of_str = function "i" -> Insert | "u" -> Update | _ -> Delete

type entry = { lsn : int64; ts : float; op : op; ns : string; id : B.t; doc : B.t option }

type t = { db : Store.db; keep : int; mutable next_lsn : int64 }

(* an 8-byte big-endian key, so LMDB's byte order = LSN order (LSNs are positive, monotonic from 1) *)
let lsn_key (lsn : int64) =
  let b = Bytes.create 8 in
  Bytes.set_int64_be b 0 lsn;
  Bytes.to_string b

let lsn_of_key k = Bytes.get_int64_be (Bytes.unsafe_of_string k) 0

let entry_doc ~lsn ~ts ~op ~ns ~id ~doc =
  let base =
    [ ("lsn", B.Int64 lsn); ("ts", B.Float ts); ("op", B.String (op_str op)); ("ns", B.String ns); ("id", id) ]
  in
  B.Document (match doc with Some d -> base @ [ ("o", d) ] | None -> base)

(* parse an entry from its Bson form — the internal record, OR the wire's raw {lsn,op,ns,id,o?} where [ts]
   is absent (defaults to 0). Exposed so a replica can rebuild entries pulled over the wire. *)
let entry_of_bson d =
  let get k = match B.get d k with Some v -> v | None -> B.Null in
  { lsn = (match get "lsn" with B.Int64 n -> n | B.Int n -> Int64.of_int n | _ -> 0L);
    ts = (match get "ts" with B.Float f -> f | _ -> 0.);
    op = (match get "op" with B.String s -> op_of_str s | _ -> Delete);
    ns = (match get "ns" with B.String s -> s | _ -> "");
    id = get "id";
    doc = B.get d "o" }

let decode_entry data = entry_of_bson (Bin.decode data)

(* open/create the oplog sub-DB and resume the LSN from its highest key (empty -> the first LSN is 1) *)
let make ?(keep = 1_000_000) store =
  let db = Store.db store "_oplog" in
  let max = ref 0L in
  Store.read store (fun txn -> Store.iter txn db ~rev:true (fun ~key ~data:_ -> max := lsn_of_key key; false));
  { db; keep; next_lsn = Int64.add !max 1L }

let current_lsn t = Int64.sub t.next_lsn 1L

(* trim to the most recent [keep] entries (capped) — the oldest below the cutoff are deleted in this txn *)
let trim t txn =
  let cutoff = Int64.sub t.next_lsn (Int64.of_int t.keep) in
  if Int64.compare cutoff 1L > 0 then begin
    let stale = ref [] in
    Store.iter txn t.db (fun ~key ~data:_ ->
        if Int64.compare (lsn_of_key key) cutoff < 0 then (stale := key :: !stale; true) else false);
    List.iter (fun k -> ignore (Store.del txn t.db k)) !stale
  end

let append t txn ~op ~ns ~id ~doc =
  let lsn = t.next_lsn in
  t.next_lsn <- Int64.add lsn 1L;
  Store.put txn t.db (lsn_key lsn) (Bin.encode (entry_doc ~lsn ~ts:(Unix.gettimeofday ()) ~op ~ns ~id ~doc));
  trim t txn

let tail t txn ~from_lsn ~limit =
  let acc = ref [] and n = ref 0 in
  Store.iter txn t.db ~from:(lsn_key (Int64.add from_lsn 1L)) (fun ~key:_ ~data ->
      acc := decode_entry data :: !acc;
      incr n;
      !n < limit);
  List.rev !acc
