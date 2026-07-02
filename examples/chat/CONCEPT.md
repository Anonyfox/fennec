# Yipchat — the realtime app archetype

Den chat rooms: live messages, presence, typing indicators, optimistic sends. The **pure DDP
showcase** — the smallest app whose entire value is reactivity, isolating fennec's live-query
pipeline (publish → subscribe → merge box → client minimongo → Fur signals) with nothing else in
the way. Also the fleet's PWA carrier: installable, with an offline shell.

## The category

Realtime apps are a distinct family the galleries track explicitly ([Vercel — Realtime Apps
category](https://vercel.com/templates)): chat and messaging, collaborative editors and boards,
live dashboards, virtual events, multiplayer anything. The taxonomies class them under dynamic
SPAs with push transports
([Octahedroid](https://octahedroid.com/blog/web-applications-types-and-examples-know-2025)).
The defining traits:

- **The server pushes.** Polling is disqualifying; the transport (websocket) and the diff protocol
  are the architecture.
- **Perceived latency is the UX.** Optimistic writes with rollback (latency compensation) are what
  separate "feels native" from "feels web".
- **Presence is a first-class noun**: who's here, who's typing, unread counts — ephemeral state
  with different persistence rules than the data.
- Chat is the canonical hello-world of the family: every realtime framework (Meteor, Phoenix,
  Firebase) demos itself with one, because it exercises the full loop in ~3 collections.

## The app

**Yipchat** — every den gets a room. Features:

- Rooms list (`#oasis`, `#den-building`, `#night-howls` seeded) + create-a-room.
- The room: live message stream, member list with presence dots, "fox is typing…" indicators,
  unread badges on the room list, infinite-scroll history.
- Login via `Accounts` (username + password — this is the fleet's first *social* login need);
  display handle + a generated fox avatar (deterministic from the handle).
- Optimistic sending: the message renders instantly from the local cache, reconciles on ack —
  demonstrate by testing against an artificially slowed method.
- PWA overlay: installable, app icon, offline shell that shows cached rooms + a clear
  "reconnecting…" state (honest offline, not fake offline).

## How it works (the fennec shape)

Axes: **full live SPA** (one hydrated app) · reactive collections · user accounts · DDP
methods + subscriptions (no REST) · async = none.

- Collections: `rooms`, `messages` (room, author, body, at), `presence` (ephemeral: user, room,
  last-seen, typing-until). One Sift codec each.
- Publications: rooms (all), messages per room (windowed: last N + older-on-demand — the
  pagination-by-subscription pattern), presence per room.
- Methods: `send_message`, `set_typing`, `create_room` — each with an optimistic client
  simulation so the UI predicts, the server settles.
- Presence is heartbeat-based: a client method every ~20s + `typing-until` timestamps; a `[@every]`
  job sweeps stale rows. Presence rows are the fleet's first deliberately-ephemeral data —
  document why they're a collection anyway (they ride the same reactive pipeline).
- The client is one Fur app; every list is a live signal over the subscribed subset. No handler
  forms anywhere — this app is methods-only by design (the counterpoint to `brochure/`).

Target layout:

```
examples/chat/
  server.ml            # endpoint + serve_ddp + accounts
  web/
    apps/main/         # the one SPA shell (+ PWA manifest/service-worker wiring)
    components/        # room_list.mlx, message_stream.mlx, composer.mlx, presence_bar.mlx …
    store/             # rooms.ml, messages.ml, presence.ml
  workflows/           # presence_sweep.ml ([@every 30.])
  test/
    http/              # SSR shell, auth gate
    browser/           # TWO pages: send in one, assert it appears in the other (the real proof)
    system/            # boot + reconnect behaviour
```

## Implementation hints

- **The two-browser test is the crown**: hunt's browser cut drives two isolated pages — login as
  two foxes, send from one, `expect_text` in the other, assert typing indicator appears/expires.
  This single test certifies the whole realtime pipeline.
- **Windowed history**: subscribe to the last 50; "load older" widens the window (or a second
  parameterized subscription); test that scrollback doesn't duplicate or reorder (stable sort:
  at + `_id`).
- **Optimistic UX honestly**: show the message immediately with a subtle pending state; on method
  error, remove + toast. The sim/latency-compensation seam is the thing to demonstrate — add a
  dev-only `?slow=1` that sleeps the method so reviewers can SEE the compensation.
- **Presence pragmatics**: unread = last-read timestamp per (user, room) — a tiny fourth
  collection, updated on room focus; presence dots go stale-grey after a missed heartbeat, swept by
  the `[@every]` job (both intervals as named constants with the why).
- **Security basics still apply**: methods validate room membership… which rooms are open here —
  so validate body length + rate-limit `send_message` per user (the accounts throttle pattern);
  escape everything (message bodies are text nodes, never raw).
- **PWA overlay**: manifest + icons (via `fennec image`), the offline shell caching the app bundle
  + last room list, visible connection state (a reconnecting banner driven by the DDP client's
  status signal). Keep the service worker minimal and legible — it's example code.
- **Scroll behavior**: pin-to-bottom when at bottom, preserve position when reading scrollback,
  "new messages ↓" chip otherwise — small, and it's the difference between demo and product.
- Discover first: `fennec discover "publish a live collection"`,
  `fennec discover "call a method with optimistic UI"`, `fennec discover "run a job every N seconds"`.

## Sources

[Vercel — templates gallery (Realtime Apps)](https://vercel.com/templates) ·
[Octahedroid — web application types 2025](https://octahedroid.com/blog/web-applications-types-and-examples-know-2025) ·
[Titancorp — web application development guide 2026](https://titancorpvn.com/insight/technology-insights/the-complete-guide-to-web-application-development-in-2025)
