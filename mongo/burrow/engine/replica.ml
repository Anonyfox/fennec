(* Replica — the follower side of async replication: apply a source's oplog onto a local engine, tracking
   the last-applied LSN. Transport-agnostic — [pull] fetches the next batch of entries (in-process:
   [Engine.oplog_tail source]; networked: a wire fetch), so one convergence loop drives a local mirror or a
   remote node. Apply is idempotent ([Engine.oplog_apply]), so a re-delivered or replayed batch is safe. *)

type t = { engine : Engine.t; mutable last_applied : int64 }

(* A follower over [engine] starts at its current oplog LSN. Seed the initial sync by opening [engine] from
   a restored hot backup: the backup is one MVCC snapshot and its own [_oplog] pins the exact LSN it
   reflects, so [last_applied] begins there — snapshot and position agree, atomically. *)
let create engine = { engine; last_applied = Engine.oplog_lsn engine }

let last_applied t = t.last_applied

let step t ~pull =
  let batch = pull ~from_lsn:t.last_applied in
  List.iter (Engine.oplog_apply t.engine) batch;
  List.iter (fun (e : Oplog.entry) -> if e.Oplog.lsn > t.last_applied then t.last_applied <- e.Oplog.lsn) batch;
  List.length batch

let lag t ~source_lsn = Int64.sub source_lsn t.last_applied
