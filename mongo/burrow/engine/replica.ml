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

let apply t batch =
  List.iter (Engine.oplog_apply t.engine) batch;
  List.iter (fun (e : Oplog.entry) -> if e.Oplog.lsn > t.last_applied then t.last_applied <- e.Oplog.lsn) batch;
  List.length batch

let step t ~pull = apply t (pull ~from_lsn:t.last_applied)
let lag t ~source_lsn = Int64.sub source_lsn t.last_applied

(* the follower can't catch up by tailing: the next entry it needs ([last_applied + 1]) fell below the
   source's oldest retained LSN and was trimmed — it must re-initial-sync from a fresh snapshot. Exact even
   with abort holes in the LSN sequence: it compares against the floor (smallest live key), not batch gaps. *)
let too_stale t ~source_floor = Int64.compare (Int64.add t.last_applied 1L) source_floor < 0
