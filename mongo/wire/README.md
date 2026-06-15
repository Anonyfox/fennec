# fennec-mongo.wire — the MongoDB wire protocol

Lets a real MongoDB client — **mongosh**, Compass, any official driver — connect to the embedded
[Burrow](../burrow/README.md) engine *exactly* as it would to a hosted mongod, for ad-hoc queries. It is
the MongoDB **wire protocol over TCP** (optionally TLS), not HTTP — which is precisely why unmodified
mongosh works (mongosh speaks the wire protocol; there is no HTTP/REST path it can use).

It is the same clean discipline as the engine: a **pure, IO-free core** with the **thin IO edge** one
layer up, so the security-critical parsing and auth are plain values you can unit/fuzz test without a
socket.

## Module cut (deps point down only)

```
fennec-mongo.wire            (PURE — no sockets, no backend, no Eio)
  ├─ Bson_wire   spec-exact BSON <-> bytes. A SEPARATE codec from Burrow's record codec: the wire is
  │              decoded by real driver code, so Binary is raw bytes + subtype and every type is spec
  │              exact. Bounds-safe (a bad length raises, never over-reads). Decimal128 -> Unsupported.
  ├─ Frame       OP_MSG (2013, incl kind-1 document sequences) + legacy OP_QUERY (2004) parse/reply;
  │              reassembles the logical command ($db injected for OP_QUERY). Bad framing -> Protocol_error.
  └─ Scram       SCRAM-SHA-256 server (RFC 5802/7677) as a pure state machine on digestif. Stores no
                 plaintext password (PBKDF2 -> StoredKey/ServerKey). Verified against the RFC 7677 vector.

fennec.pulse.mongo  (the ONLY IO — Eio + tls-eio; Wire_server.Make over a small DB interface)
  └─ Wire_server  TCP accept loop (daemon fiber); per-conn auth + cursor state; resource limits; TLS.
                  Dispatches a parsed command -> a DB op -> a reply doc. Backend-agnostic.
     expose       Fennec_pulse_mongo.expose — fronts the embedded Burrow engine, sharing its per-dir
                  engine cache so mongosh sees the app's live data and writes use the same writer.
```

Why this cut: `fennec-mongo.wire` has zero IO and zero backend, so `Bson_wire`/`Frame`/`Scram` are
tested in isolation (byte vectors, hand-built frames, the RFC auth vector). `Wire_server` is the only
place with sockets/TLS/state — one small surface to audit for the limits and the bind address.

## Security (safe by default)

- **Loopback bind** by default — exposing `0.0.0.0` is an explicit opt-in (the classic Mongo disaster is
  an unauthenticated, internet-exposed instance).
- **SCRAM-SHA-256 required** whenever any user is configured; passwords are salted challenge-response,
  never sent. (Salt is 28 bytes = `hash_size - 4`, the MongoDB convention drivers enforce.)
- **No `$where` / JS** anywhere in the engine, so server-side code injection is structurally impossible.
- **Resource caps**: oversized/malformed frames are rejected before allocating and drop only that
  connection; max-connections and max-message-size are configurable.
- **Read-only mode** rejects every mutation; only a vetted command set is implemented (no
  `shutdown`/`eval`/`fsync`/...). **TLS** optional via `tls-eio`.

## The one knob: `MONGO_URL`

Backend, database, and any mongosh endpoint all come from a single env var — no userland config. Three
schemes (the SQLite → server progression):

| `MONGO_URL` | backend | mongosh |
|---|---|---|
| `:memory:` | minimongo (ephemeral) | — |
| `burrow:///abs/path` | embedded engine, storage only | — |
| `burrow://[user:pass@]host:port/abs/path[?tls&readonly]` | embedded + wire endpoint | yes |
| `mongodb://…` | real server | (mongosh the server directly) |

For `burrow://` the **authority is the wire endpoint** — the mirror of `mongodb://`'s `host:port`:
present ⇒ expose there (SCRAM when `user:pass`, else unauthenticated — only sane on loopback); absent ⇒
storage only. The path's **trailing segment is the database name** and its on-disk directory (`mongosh
use other` ⇒ a sibling dir).

- **Dev** (type nothing): `fennec dev` generates `burrow://localhost:27017/<abs>/…/dev-<port>/fennec`, so
  `mongosh "mongodb://localhost:27017"` just works. No dev special-case — the URL carries the authority.
- **Prod, exposed**: `MONGO_URL=burrow://admin:secret@0.0.0.0:27017/var/lib/app` → `mongosh
  "mongodb://admin:secret@host:27017"`.
- **Prod, storage only**: `MONGO_URL=burrow:///var/lib/app`.
- **Tests**: `:memory:` (unit) or a storage-only `burrow:///tmp/…` per suite (integration, isolated, no
  endpoint).

`Fennec_pulse_app.start ~sw ~net ()` calls `expose_from_env`, which reads the URL and opens the endpoint
when the authority is present — zero app code. `Fennec_pulse_mongo.expose ~sw ~net ~users … ?tls ()` stays
as an escape hatch (custom users, or a TLS wire endpoint until `?tls` is wired for the auto path).

## Validation

- `test/`: exact BSON byte vectors + round-trip + bounds-safety; hand-built OP_MSG/OP_QUERY frames; the
  **RFC 7677** SCRAM vector (so a real client's proof verifies byte-for-byte).
- `../../fennec/pulse/mongo/test/test_wire_server.ml`: the **real libmongoc driver** authenticates over
  SCRAM and runs full CRUD against the server (and a wrong password is rejected).
- `../../fennec/pulse/mongo/test/test_wire_mongosh.ml`: **real mongosh** connects, authenticates, and
  runs insertMany / aggregate / countDocuments / sorted-find — asserting on what mongosh printed.
- `../../fennec/pulse/mongo/test/test_wire_tls.ml`: the driver authenticates + runs CRUD **over TLS**
  (self-signed cert, `Tls_eio` termination) — SCRAM over the encrypted channel.
- `test_wire_server.ml` also drives `expose_from_env` directly: a `burrow://` authority with no creds
  opens an unauthenticated endpoint fronting the engine the path points at.
- `runtime`'s `test_runtime.ml` covers the URL parser (the three schemes + authority/db parsing).

## Known gaps

- `Decimal128` on the wire is unimplemented (raises `Unsupported`, surfaced as a Mongo error — never
  corrupts the stream); rare in practice (only an explicit `NumberDecimal`).
- `drop` empties a collection (its catalog entry/sub-DBs remain) until the engine grows a true drop.
- No change streams / `$changeStream` over the wire (in-process `observe_changes` is unaffected); no
  compression (`OP_COMPRESSED`) — clients fall back to uncompressed.
