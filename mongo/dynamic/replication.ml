(* Replication — the wire-backed transport for {!Burrow.Replica}: a [pull] that fetches a source database's
   oplog over the MongoDB wire (the [oplogFetch] command) so a follower converges to a REMOTE source. Pair
   it with a local [Burrow.Replica] over the follower's engine and drive [Replica.step ~pull]. The apply is
   idempotent, so a retried batch is safe. *)

module B = Bson

(* a [pull] for {!Burrow.Replica.step}: fetch up to [limit] entries after [from_lsn] from [client]'s source
   [db] over the wire, rebuilt as {!Burrow.Oplog.entry} values ready to apply. Partially apply
   [pull client ~db ~limit] to get the [~from_lsn ->] pull [Replica.step] expects. *)
let pull client ~db ~limit ~from_lsn =
  let reply =
    Wire_client.run client
      (B.Document
         [ ("oplogFetch", B.Int 1); ("$db", B.String db); ("fromLsn", B.Int64 from_lsn); ("limit", B.Int limit) ])
  in
  match B.get reply "entries" with
  | Some (B.Array entries) -> List.map Burrow.Oplog.entry_of_bson entries
  | _ -> []
