(* Replication — the wire-backed transport for {!Burrow.Replica}: fetch a remote source database's oplog
   over the MongoDB wire (the [oplogFetch] command), so a follower converges to a source it reaches by
   socket. Pair with a local [Burrow.Replica]; apply is idempotent, so a retried batch is safe. *)

val fetch :
  Wire_client.t -> db:string -> limit:int -> from_lsn:int64 -> int64 * Burrow.Oplog.entry list
(** The source [db]'s oplog entries after [from_lsn], as [(retention_floor, entries)]. The floor (the
    source's oldest retained LSN) feeds {!Burrow.Replica.too_stale} — a follower below it cannot catch up
    by tailing and must re-initial-sync; the entries feed {!Burrow.Replica.apply}. *)

val pull : Wire_client.t -> db:string -> limit:int -> from_lsn:int64 -> Burrow.Oplog.entry list
(** The entries only (no staleness check) — partially apply [pull client ~db ~limit] to get the
    [~from_lsn -> entries] pull that {!Burrow.Replica.step} expects. *)
