(* Replication — the wire-backed transport for {!Burrow.Replica}: fetch a source database's oplog over the
   MongoDB wire (the [oplogFetch] command) so a follower converges to a REMOTE source. Pair it with a local
   [Burrow.Replica] over the follower's engine. Apply is idempotent, so a retried batch is safe. *)

module B = Bson

(* fetch the source [db]'s oplog after [from_lsn] over the wire: returns [(retention_floor, entries)]. The
   [floor] (the source's oldest retained LSN) feeds {!Burrow.Replica.too_stale} — if the follower has fallen
   below it, tailing would silently skip trimmed entries and it must re-initial-sync; the entries feed
   {!Burrow.Replica.apply}. *)
let fetch client ~db ~limit ~from_lsn =
  let reply =
    Wire_client.run client
      (B.Document
         [ ("oplogFetch", B.Int 1); ("$db", B.String db); ("fromLsn", B.Int64 from_lsn); ("limit", B.Int limit) ])
  in
  let floor = match B.get reply "floor" with Some (B.Int64 n) -> n | Some (B.Int n) -> Int64.of_int n | _ -> 0L in
  let entries =
    match B.get reply "entries" with Some (B.Array es) -> List.map Burrow.Oplog.entry_of_bson es | _ -> []
  in
  (floor, entries)

(* a [pull] for {!Burrow.Replica.step} — the entries only (no staleness check). Partially apply
   [pull client ~db ~limit] to get the [~from_lsn ->] pull [Replica.step] expects. *)
let pull client ~db ~limit ~from_lsn = snd (fetch client ~db ~limit ~from_lsn)
