(* Replica — the follower side of async replication over the oplog (see the .ml): a transport-agnostic
   apply-side state machine. The network layer injects a wire-backed [pull]; the same convergence logic
   serves a local mirror. Built on the idempotent {!Engine.oplog_apply}, so redelivery is safe. *)

type t

val create : Engine.t -> t
(** A follower over [engine], starting at its current oplog LSN. Open [engine] from a restored hot backup to
    seed the initial sync — the backup is one MVCC snapshot and its own [_oplog] pins the exact LSN it
    reflects, so [last_applied] begins there. *)

val last_applied : t -> int64
(** The highest source LSN this follower has applied. *)

val step : t -> pull:(from_lsn:int64 -> Oplog.entry list) -> int
(** Pull the source's entries after [last_applied] and apply them idempotently, advancing [last_applied];
    returns how many were applied. [pull] is the transport — [Engine.oplog_tail source] for a local mirror,
    or a wire fetch for a remote node — so the same convergence logic drives either. *)

val lag : t -> source_lsn:int64 -> int64
(** How far behind the source this follower is: [source_lsn - last_applied] (0 = caught up). *)
