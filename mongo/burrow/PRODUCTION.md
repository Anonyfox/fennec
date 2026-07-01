# Burrow — The Road to Production Grade

> **Burrow is SQLite for MongoDB.** An embedded, zero-ops, single-node database that speaks the MongoDB
> wire protocol byte-for-byte — but runs *in your process*, on LMDB, with no daemon, no IPC, and a
> fraction of the overhead. The day you genuinely need sharding or consensus failover, the answer is
> "point your `mongodb://` URL at real Mongo" — Burrow is wire-compatible **precisely so that migration is
> a config change, not a rewrite.**

This is the standing plan for taking Burrow from *"works embedded"* to *"a company can bet its data on it,
unattended, for single-node-with-warm-standby."* That last clause is the entire scope. [README.md](README.md)
is the current design; this is the forward map, and §2 is the law every item must obey.

---

## 1. What Burrow is — and is not

**Is:** the embedded MongoDB. Single writer, lock-free MVCC readers, group-committed durability, a real
query planner, byte-exact wire compatibility (validated against libmongoc *and* mongosh). It wins on
**latency** (the query runs in your address space — no network or IPC hop), **footprint** (one mmap'd file,
no second process), and **simplicity** (no cluster to operate).

**Is not, and will never be:** a distributed database. Sharding, consensus-based automatic failover
(Raft/Paxos), multi-primary writes, cross-shard transactions, geo-distribution — **explicitly out of
scope** (§7). That is mongod's job, and Burrow's byte-compatibility is the off-ramp to it.

**The target deployment:** one primary, optional warm read-replicas, orchestrated failover, real backups,
real observability, real access control. This covers the overwhelming majority of apps that never needed a
cluster — they only reached for one because the embedded option wasn't credible. Burrow makes it credible.

---

## 2. The performance covenant (non-negotiable)

Burrow exists for **swift, lean throughput with tiny overhead.** Every item below is admissible *only* if
it honors these rules. A feature that taxes the hot path is rejected, redesigned, or made opt-in-at-zero-cost.

1. **The hot read/write path gains nothing it doesn't pay for.** No allocation, no lock, no branch on the
   per-op path for a disabled feature. The embedded dev default stays exactly as lean as today.
2. **The single-writer group-commit is sacred.** One `F_FULLFSYNC` amortized across a batch (the writer
   loop in `engine.ml`). New durable state — the oplog above all — rides **inside the existing parent write
   txn**. Never a second transaction, never a second fsync.
3. **Reads stay lock-free on MVCC snapshots.** Nothing introduces a reader↔writer lock. Out-of-band work
   (backup, compaction, replication streaming) runs off a read snapshot or a background fiber and **never
   blocks the writer**.
4. **Observability is pull-based and near-free.** Because there is one writer, write-path counters are
   plain (non-atomic) increments it owns — zero contention; read-path counters shard or use atomics. All
   aggregation happens only when someone calls `serverStatus`/`explain`. Slow-query logging is one gated
   compare, off by default.
5. **Compatibility is byte-exact or it doesn't ship.** Every command's wire output must match MongoDB
   exactly, proven by the differential harness — not "close enough."
6. **Everything new is optional and zero-cost when off.** Replication, oplog, auth, authz, TLS, profiling,
   encryption: all configurable, all free in the embedded default.

> **The architectural gift.** The single writer already establishes a **total order** on all writes. Most
> engines pay dearly for that ordering; Burrow gets it for free — which is *why* backup, the oplog,
> replication, and online index builds below can be built **with** the grain instead of against it. The
> same fact caps single-stream write throughput; that trade is the deal, and it's the right one for the
> target deployment.

---

## 3. Where Burrow stands today

The core is done and **differential-tested against minimongo and real mongod** — more correctness
assurance than most engines ever get:

- [x] **Durability** — group-commit `F_FULLFSYNC`; `Full` / `No_meta_sync` / `No_sync` modes; LMDB is
      crash-safe by design (copy-on-write, no torn writes). The README reports ~20k+ durable writes/s at
      200 concurrent writers.
- [x] **Concurrency** — single-writer fiber + lock-free MVCC reads (`MDB_NOTLS`).
- [x] **Workflow transactions** — `begin_txn`/`commit_txn`/`abort_txn`, read-your-writes, atomic rollback.
- [x] **Query + aggregation** — comprehensive operators and stages, shared with minimongo, differential-tested.
- [x] **Indexing** — single / compound / unique / sparse / multikey + a selectivity-scoring planner with sort-via-index.
- [x] **Wire protocol** — CRUD, aggregation, indexes, cursors/getMore, SCRAM-SHA-256 **authentication**, TLS;
      validated vs libmongoc + mongosh (wire version 17 / Mongo 6.0).
- [x] **Schema validation** — `$jsonSchema` structural subset, persisted per collection.

**Honest starting-line caveats** (each addressed below): no backup binding yet (the C lib supports
compacting hot-copy; we expose only `env_sync` — Tier 0); **authentication but no authorization** — any
authenticated user can do anything (Tier 1); a workflow transaction holds the global write lock for its
whole span (Tier 0); the env opens with `max_dbs:1024` and each collection costs one sub-DB plus one per
index (Tier 2); fixed 128 GB virtual map (Tier 0); no observability (Tier 2); single-node only (Tier 3).

The gaps are not in the core. They are the operational envelope that turns an engine into a system you
trust unattended.

---

## 4. The roadmap

Tiered by what production actually requires; ship top-down. Tier 0 is disqualifying-if-absent.

### Tier 0 — Data safety & availability (a DB without these is not production grade)

- [x] **Backup primitive — online hot-copy.** `Engine.backup ~dir ~compact` (binds `mdb_env_copy2`): a
      consistent MVCC snapshot copied **off the write path** with zero write downtime, defragging as it
      copies; the result is a standalone, openable database (restore = open it). Proven by `test_backup`.
- [ ] **Backup operations** — a CLI / scheduled trigger, retention, off-host shipping, and **periodic
      restore verification** (an untested backup is not a backup) layer on the primitive. *Covenant:
      out-of-band on a read snapshot; the writer never sees it.*
- [x] **Write-lock-hold observable.** `begin_txn` holds the single write lock until commit/abort; a slow
      workflow — or *any* external I/O done while holding it — stalls **all** writes engine-wide.
      `Engine.transaction_held_for` now exposes how long the current txn has held the lock (`None` when
      idle), so observability / a watchdog can alarm. The sanctioned FIX stays the **effects outbox** (defer
      mail/HTTP/all I/O to after commit, never inside the txn); auto-aborting a runaway holder is unsafe (you
      can't abort another fiber's txn mid-op), so this is detection, not enforcement. *The price of the
      single-writer model: a held write txn is globally exclusive — keep it microseconds, not milliseconds.*
- [ ] **Map sizing & disk-full — two distinct failures.** (1) *Map limit:* the LMDB map is sparse —
      virtually allocated, the file grows only to bytes actually written — so the lean fix is to **size it
      generously at open** and alarm at a high-water mark. **`Engine.usage` ships** (bytes-in-use vs the
      ceiling + ratio, a cheap metadata read) — the alarm primitive; generous-default sizing and the alarm
      policy layer on it. Runtime grow (`mdb_env_set_mapsize`) needs *all* txns quiesced, so it stays the
      rare fallback. (2) *Physical disk-full (ENOSPC):* LMDB's CoW guarantees the last committed state
      survives a failed commit — surface it as "reject writes with a clean error, stay readable, alert,"
      and test that path.
- [x] **Query resource governance — Phase 1 (materialization cap).** `Engine.open_ ~query_limit:n` caps the
      documents a single read may materialize; `find`/`find_one`/`distinct`/`aggregate` raise a clean
      `Executor.Result_too_large` past it instead of OOM-ing the host. One check per query at the `matched`
      chokepoint (`compare_length_with`, O(min(len,cap))) — the per-record scan loops stay byte-identical
      (covenant). Default unlimited. Proven by `test_governance`.
- [ ] **Query governance — Phase 2.** A wall-time budget, and spill-to-disk external sort/group behind an
      `allowDiskUse`-style flag, so a limited query streams instead of materializing the whole matched set.
- [x] **Online index builds.** A NON-unique index builds WITHOUT holding the write lock for the walk:
      registered with a persisted `ready=false` (the planner skips it — query-invisible — while new writes
      still maintain it, since it's in `c.indexes`), backfilled in chunks submitted through the writer (live
      writes interleave between chunks; index puts idempotent by `encoded-key ++ _id`), then flipped
      `ready=true`. **Restart-safe:** a crash mid-build leaves `ready=false` on disk, which `Catalog.open_`
      drops on reopen — a partial index is never served. UNIQUE indexes stay synchronous (a partial unique
      index can't validate concurrent writes). `ready` persisted additively (missing ⇒ true). Proven by
      `test_online_index` (multi-chunk build is used + complete, post-build writes maintained, survives
      reopen, planner-skip, reopen-drops-incomplete).

### Tier 1 — Security & access control (regulated workloads gate on this)

- [x] **Role-based authorization.** `wire_user ~roles:[ Role.Read "db" | Role.Read_write "db" | Role.Root ]`
      (`"*"` db = all databases; default `Root` for back-compat). Enforced at command dispatch: every
      command is classified (`Wire_server.access_of` → read / write / exempt) and checked against the
      user's roles on the target database before it runs — one role lookup per command, only under
      `require_auth`. Proven by `test_authz` (the pure decision + classification matrix) + a libmongoc-driver
      integration test (a read-only user's write is denied over the wire). Per-collection privileges +
      populating `authenticatedUserRoles` in `connectionStatus` are the remaining polish.
- [x] **Encryption at rest — posture.** The burrow data file is a plain LMDB file, so the recommended,
      zero-overhead EAR is **volume/filesystem-level encryption** (LUKS / FileVault / cloud-volume EBS/PD),
      documented in the README. Engine-level page encryption (the DB custodying its own keys) stays a
      deferred opt-in for compliance regimes that require app-custodied keys, accepting the per-page crypto
      cost — no tax on everyone else. No engine code: the lean answer is the storage layer, not the engine.
- [x] **Audit logging (security events).** `expose ~audit:(fun ev -> …)` — a sink receiving an
      `Audit.event` per authentication, failed authentication, and authorization denial (with the command +
      database), default no-op, so the operator ships it to a log / SIEM at zero cost when unused. Proven by
      the wire integration test (auth + a denial captured with their command/db). *Data-change* auditing
      (every write, stamped with the actor) rides the oplog (Tier 3) — deferred until it exists.

### Tier 2 — Operability (you cannot run unattended what you cannot see)

- [x] **`serverStatus` + metrics.** The wire `serverStatus` command reports uptime, connections (current /
      available / totalCreated), and opcounters (insert / query / update / delete / command) — per-server
      atomic counters bumped once per command (never per record, covenant-clean). Proven via real mongosh
      (`db.serverStatus()`). Deeper gauges (write-queue depth, per-collection sizes via `mdb_stat`, fsync
      latency) layer on the same command when wanted.
- [x] **`explain`.** `Engine.explain` + the wire `explain` command surface the planner's winningPlan
      (COLLSCAN / IXSCAN+indexName / IDHACK / OR / AND_SORTED + direction), Mongo queryPlanner-shaped — so
      `db.coll.find().explain()` works and a Fennec app can see its access paths. No hot-path change (it runs
      the pure planner without executing). Proven by `test_explain`.
- [ ] **Slow-query log** — one gated compare on a per-query duration, off by default, logging the selector +
      chosen plan + timing past a threshold.
- [ ] **Structured logging.** Beyond `FENNEC_WIRE_DEBUG` stderr tracing — a real leveled/structured log
      (reuse Paw's logger style) for prod ingestion.
- [ ] **Resource & DoS hardening.** Max connections, idle-cursor + idle-session reaping, per-connection
      caps, and a **fuzzed wire-frame parser** (a malformed frame must drop only its connection).
- [ ] **Wire-protocol test suite.** The wire layer is validated *manually* today. Production needs an
      automated driver-compat matrix (libmongoc / Node / Python) + the parser fuzzer in CI.
- [ ] **Collection/index ceiling visibility.** Each collection is one record sub-DB + one sub-DB per index +
      a shared meta DB, under `max_dbs:1024` (e.g. ~150 collections × 5 indexes ≈ 900). Make it configurable,
      monitor headroom, and fail creation with a clear error well before LMDB's cryptic one.

### Tier 3 — The oplog keystone, then async HA (the one scoped step toward distribution)

- [x] **The oplog** — a capped, LSN-ordered `_oplog` sub-DB (the `Oplog` module) the single writer fills for
      free: every committed write (insert/update/delete, non-txn AND workflow-txn) appends one idempotent
      entry per affected doc — the RESULTING document (insert/update) or the `_id` (delete) — INSIDE the same
      write txn (atomic, no extra fsync). Crash-resumes the LSN from the log's max on open; trims to
      `~oplog_keep`. Consumers read it via `Engine.oplog_tail ~from_lsn`. Proven by `test_oplog` (append +
      tail, idempotent shape, crash-resume, cap, workflow commit-vs-abort). DDL logging (createIndex/drop) is
      the one deferred piece before it's replication-complete.
- [x] **Resumable change streams** — the wire `$changeStream` (`db.coll.watch()`): `aggregate` with a
      `$changeStream` first stage opens a tailing cursor; `getMore` tails the oplog via `oplog_since`,
      formatting each entry as a Mongo change event (operationType / ns / documentKey / fullDocument) with
      the LSN as the resume token — so a client resumes across reconnect/restart via
      `$changeStream.resumeAfter`. Per-collection or per-db. Proven by a pure `change_event` test + a real
      mongosh `db.coll.watch()` smoke (insert → the insert event). Replaces in-process-only `observe_changes`
      for external consumers.
- [x] **Point-in-time recovery** — the replay primitive `Engine.oplog_apply` (idempotent replace-or-insert
      / delete by `_id`, index-maintaining, NOT re-logged) composes backup + `oplog_tail` into PITR: restore
      a hot backup (frozen at its LSN), then replay the source's oplog `(backup_lsn, target]` onto it to roll
      forward. Proven by `test_pitr` (an update + delete + insert replayed onto a restored backup reconstruct
      the source; re-replay is a no-op). Recovers within the oplog retention window; continuous oplog
      ARCHIVING (to roll forward past the cap) is the deferred extension.
- [x] **Async read-replicas** — the follower state machine `Replica` (transport-agnostic: `create` from a
      restored backup seeds `last_applied` at the backup's pinned LSN; `step ~pull` applies the source's
      oplog idempotently + advances; `lag` reports the gap) AND the wire transport: an `oplogFetch` command
      (source) + a minimal `Wire_client` + `Replication.pull` (a wire-backed `pull` over it), so a follower
      converges to a REMOTE source. `Wire_client` authenticates with SCRAM-SHA-256 (mutual auth) or runs
      plaintext on a trusted network. Proven by `test_replica` (in-process convergence) + `test_replication`
      (a follower converging over the MongoDB wire on a Unix socket, BOTH unauthenticated and over
      SCRAM-SHA-256) + `test_scram` (the client reproduces the RFC 7677 vector). Too-stale DETECTION is done
      (`Replica.too_stale ~source_floor` / `Engine.oplog_floor`, surfaced in the `oplogFetch` reply — a
      lagged follower knows tailing would skip trimmed entries and it must re-sync). Remaining:
      read-preference routing + the automatic re-sync ACTION (app-driven: catch `too_stale`, re-initial-sync
      from a fresh backup).
- [ ] **Orchestrated failover** — promote/demote mechanism, old-primary tail rollback, honest RPO; fencing
      is the orchestrator's job (no consensus). See §5.
- [ ] **Consistency testing** — jepsen-style: inject partitions/crashes during failover; assert no
      acknowledged-write loss beyond the stated RPO and no split-brain.

### Tier 4 — Compatibility & longevity polish

- [ ] **`findAndModify`** — atomic find-and-update; drivers and account flows rely on it. Notable wire gap.
- [ ] **Key-size safety** — the ~511-byte LMDB key limit surfaces as a cryptic `MDB_KEY_FULL` on wide
      compound indexes. Reject at index-create/insert with a clear error, or hash-suffix long keys.
- [ ] **Online compaction** — free-list bloat over long-running engines; folds into the `mdb_env_copy2` work.
- [ ] **Catalog format version + upgrade path** — so a Burrow version bump never strands on-disk data.
- [ ] **Dotted-array index optimization** — `emails.address` matches correctly via the residual matcher
      today, but isn't always index-*served*; teach the planner to use the multikey entries.
- [ ] **TTL indexes** — auto-expire documents (sessions, tokens, caches): a common Mongo feature. The
      single writer makes the reaper clean — a background fiber enqueues expiry deletes; no concurrent race.
- [ ] **Text / geo index acceleration** — these queries *work* via residual scan today; indexed acceleration
      (full-text, 2dsphere) is deferred — a heavy subsystem, SQLite-FTS-style optional rather than core.
- [ ] **Aggregation completeness** — the remaining stages (`$graphLookup`, `$out`, `$merge`, `$geoNear`,
      `$setWindowFields`, …) currently fall through; add as demand warrants, byte-exact per the covenant.
- [ ] **Decimal128 fidelity** — stored as string today (round-trips losslessly, not the 16-byte wire form).
      Only matters for fintech; low priority.

---

## 5. The oplog: one keystone, three features

**Status: the log itself is BUILT** — the `Oplog` module + `Engine.oplog_tail` / `oplog_lsn`; CRUD entries,
idempotent (resulting doc / `_id`), appended in the write txn, crash-resumed, capped (`test_oplog`). The
three consumers below (change streams, PITR, replication) and DDL logging are the follow-on units.

The single highest-leverage missing piece. The write path already reserves the hook (`engine/write.mli`:
*"Oplog append for change streams layers on here later"*). Build it once and change streams, PITR, and
replication all follow — none needs distributed consensus, **because the single writer already establishes
a total order.**

**The log itself**
- A **capped, ordered sub-DB `_oplog`**, keyed by a monotonic **LSN** (int64) the single writer assigns —
  no CAS, no election; the LSN is just `last + 1`.
- Each committed write appends its op(s) — `{ lsn, ts, op, ns, _id, doc-or-Δ }` — **inside the same parent
  write txn** as the data change, so it is atomic with it, with **no extra fsync** (covenant rule 2).
  Marginal cost: one `put`.
- **Idempotent by construction.** Apply must be safe to replay (crash/resume, re-sync). Non-idempotent
  modifiers are *transformed at append time* to their resulting state — `$inc` → `$set:<result>`, `$push` →
  the resulting array — exactly as MongoDB's oplog does; insert/replace/delete are already idempotent by `_id`.
- **DDL is logged too.** Index and collection create/drop and validator changes go in the oplog alongside
  CRUD, so replicas and PITR reconstruct *schema*, not just data — otherwise a replica silently drifts.
- **Capped.** The single writer trims entries past the retention window/size during commit (amortized, cheap).
- **Crash-safe resume.** Entry and data share a txn, so LMDB recovery leaves them coherent; on restart the
  next LSN is `max(_oplog) + 1`.

**Consumer 1 — change streams. (DONE — the wire `$changeStream` / `db.coll.watch()`.)** Tail `_oplog` from a
resume-token LSN (the LSN is the token, on disk, so it's durable across reconnect/restart). Replaces the
in-process-only `observe_changes` for external consumers.

**Consumer 2 — point-in-time recovery. (DONE within the retention window — `Engine.oplog_apply` composes
backup + `oplog_tail` into restore-and-roll-forward; `test_pitr`.)** A hot-copy backup is **self-stamping** —
it contains `_oplog` up to some LSN *X*. To recover past *X* you need **continuous oplog archiving** to cold
storage; PITR = restore the backup, replay archived oplog `(X, target]`. Recovery granularity = archive cadence.

**Consumer 3 — async replication. (DONE — the `Replica` follower + a wire transport via the `oplogFetch`
command / `Wire_client` (with SCRAM-SHA-256 client auth, mutual) / `Replication.pull`; `test_replica` +
`test_replication` + `test_scram`; too-stale DETECTION via `Replica.too_stale` / `Engine.oplog_floor`.
Remaining: read-preference routing + the automatic re-sync action.)** A
new follower's **initial sync** needs a snapshot *and* the exact LSN
it reflects, atomically — and the MVCC gift delivers it: open **one read txn**; it sees a consistent data
snapshot *and* `max(_oplog)` coherently (same snapshot). Copy the snapshot, then tail `_oplog` from that
LSN. Steady state: pull `(last_applied, …]`, apply in order; apply is idempotent, so a crash mid-batch is
safe to retry.
- **Too-stale handling. (Detection DONE.)** A follower that lags past the capped retention window can't catch
  up by tailing and must re-initial-sync from a fresh snapshot. `Replica.too_stale ~source_floor` detects it
  exactly (`last_applied + 1 < floor`, where `floor = Engine.oplog_floor` the oldest retained LSN, carried in
  the `oplogFetch` reply) — exact even with abort holes in the LSN sequence, since it compares the floor not
  batch gaps. Size retention ≥ the longest tolerable follower outage; the app acts on the "stale" state.
- **Read-consistency contract.** Replicas serve **eventually consistent** reads, bounded by lag. Expose it
  honestly via read-preference routing so clients pick primary (read-your-writes) or replica (scale, stale).

**Failover — orchestrated, not consensus.**
- Async replication means the primary may have committed writes not yet shipped — **the RPO is the lag**,
  not zero. State it. On promotion those un-shipped writes leave the new timeline.
- When the old primary rejoins, its un-replicated tail must be **rolled back** (archived for forensics) to
  match the new primary's history — exactly MongoDB's rollback.
- **Fencing is the orchestrator's job.** With no consensus, an external coordinator must guarantee the old
  primary is demoted/stopped before the new one accepts writes, or you get split-brain. Burrow provides the
  *mechanism* (promote/demote, LSN comparison, rollback); the operator owns the *policy*.

That boundary is the honest line: Burrow does HA *mechanism*; *consensus* is out of scope (§7).

---

## 6. Definition of "production grade" for Burrow

Burrow is production grade when an operator can, **unattended**, trust it with real data:

> **Everything through Tier 3** (data safety → security → operability → the oplog → async replica →
> orchestrated failover). Durable, backed up, point-in-time recoverable, access-controlled, observable,
> governed against OOM, with a warm standby and orchestrated failover — all while preserving today's
> embedded speed and tiny overhead (§2).

Tier 4 is compatibility & longevity polish that raises the surface area but doesn't gate the "bet your data
on it" line.

---

## 7. Explicitly out of scope (use real Mongo)

Saying this clearly is a feature, not an admission — and byte-compatibility makes the handoff a URL change:

- **Sharding / horizontal data partitioning.**
- **Consensus-based automatic failover** (Raft/Paxos election). Burrow does *orchestrated* failover only.
- **Multi-primary / active-active writes** and **cross-shard distributed transactions.**
- **Geo-distribution.**
- Anything that violates §2 — server-side JavaScript (`$where`), arbitrary scripting, or any feature that
  taxes the hot path for everyone to serve a few.

When you need these, you have outgrown the embedded model. Switch the connection string to mongod; your
application code, written against the byte-compatible wire, does not change. **That clean exit is the point.**
