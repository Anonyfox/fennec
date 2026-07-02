# Burrowbook — the internal tool / CRM archetype

A lightweight CRM for the den-to-den sales trade: contacts, a pipeline, notes, CSV import/export.
The **data-dense internal tool** shape — what agencies and in-house teams quietly build more of
than anything else: tables you live in, bulk operations, imports from the old spreadsheet, and
role-gated views. No public face at all; every user is staff.

## The category

The internal family is the web's dark matter: admin panels, CRMs, customer portals, intranets,
project trackers, inventory apps — "organizations are building everything from client portals and
internal tools to CRMs, intranets, project trackers, and inventory apps"
([Jetadmin — CRM portals](https://www.jetadmin.io/blog/6-best-crm-portals-for-your-business/); the
CRM+portal pattern in [Softr's survey](https://www.softr.io/blog/crm-with-client-portal); the whole
low-code industry — Retool, Airtable — exists to serve it). The defining traits:

- **Tables are the UI**: sort, filter, paginate, bulk-select, inline status — the craft is density
  + keyboard speed, not visual flash.
- **Data arrives dirty**: CSV import (with a dry-run preview) is a core feature, not an
  afterthought; export is how data leaves for the tools you don't control.
- **Views are role-shaped**: reps see their patch, managers see everything, read-only exists for
  the curious; every record answers "who touched this, when".
- **The timeline is the memory**: notes + activity per record is what makes a CRM a CRM rather
  than a contact list.

This is also where fennec's pitch lands hardest: internal tools are exactly the apps that suffer
most from accidental complexity — a lean fullstack answer with realtime for free is the dream
internal-tools stack.

## The app

**Burrowbook** — "every den on your route." A CRM for a desert supplies wholesaler. Features:

- Contacts: dens (companies) + foxes (people at them), with region, size, tags.
- The pipeline: deals per den — `sniffing → visited → offer → won/lost` — as both a table view
  and a kanban-lite board view (columns fixed by stage), with per-rep filtering.
- The record page: den details, its foxes, open deals, and the **timeline** — notes (markdown-lite)
  + automatic activity entries (stage moves, assignments, imports), newest first.
- Assignment: each den has an owner rep; "my dens" is the default view; managers reassign in bulk.
- Import: upload the legacy CSV → mapped-columns preview with per-row validation verdicts →
  confirm → rows land (with an import-batch id for undo); Export: current filtered view as CSV.
- Roles: rep (own patch), manager (all + reassign + import), viewer (read-only).
- A small SSR dashboard: pipeline totals by stage, activity this week, stale deals (no touch in
  14 days) — server-rendered numbers + hand-drawn SVG bars, no chart library.

## How it works (the fennec shape)

Axes: lightly-reactive SSR (tables live-update, but pages are pages) · CRUD-heavy + batch ·
accounts + 3 roles · forms + methods + file both ways · async = a stale-deal `[@cron]` nudge.

- One auth-gated endpoint (no public routes beyond login); the roles via the RBAC policy.
- Collections: `dens`, `foxes`, `deals`, `timeline`, `import_batches` — Sift codecs, indexes on
  the filter/sort fields (owner, stage, region, updated-at).
- Tables render server-side with URL-carried state (`?sort=&stage=&owner=&page=` — shareable,
  back-button-correct); the reactive layer live-refreshes rows (a colleague's edit appears) —
  demonstrating that SSR-first and realtime aren't opposites.
- Import: multipart upload → parse + validate per-row against the Sift codec → a preview page
  (first 20 verdicts + counts) → confirm applies as one workflow transaction stamped with the
  batch id; "undo import" removes the batch. Export via the attachment-download helper.
- Bulk actions: select-all-in-filter semantics (the count is the filter's count, not the page's),
  applied as one method with the filter re-evaluated server-side.

Target layout:

```
examples/crm/
  server.ml
  web/
    apps/main/         # the tool (login-walled)
    components/        # data_table.mlx, stage_board.mlx, timeline.mlx, import_preview.mlx,
                       # bulk_bar.mlx, svg_bars.mlx …
    handlers/          # den_form.mlx, note_form.mlx, import.mlx (upload → preview → confirm)
    store/             # dens.ml, foxes.ml, deals.ml, timeline.ml, import_batches.ml
  workflows/           # apply_import.ml, undo_import.ml, stale_nudge.ml ([@cron])
  seed/                # a believable book of dens + a deliberately-dirty legacy.csv
  test/
    http/              # role matrix, filter/sort URLs, import dry-run verdicts, export shape
    browser/           # table sort/filter/bulk; import wizard end-to-end; timeline updates live
    system/            # import+undo as a transaction; the stale-deal cron via tick
```

## Implementation hints

- **The table component is the deliverable**: one generic `data_table.mlx` (columns, sort, page,
  selection) reused for dens/foxes/deals — if it comes out clean, it graduates to a documented
  pattern; if it fights the framework, that's a DX finding to feed back.
- **URL-as-state discipline**: every view reachable by URL, every filter bookmarkable; http tests
  assert the URL grammar. Keyboard: `/` focuses search, `j/k` row-walk, `x` select — cheap, and
  it's what "internal tool people" notice first.
- **Import is a wizard, honestly**: the dirty seed CSV includes every failure class (bad email,
  unknown region, duplicate den, empty row) so the preview verdicts DEMO the validation; per-row
  errors name the column; nothing writes until confirm; the batch id makes undo trivial and
  auditable in the timeline.
- **Select-all semantics**: "all 240 matching" vs "these 20 on this page" — label the bulk bar
  explicitly (the classic internal-tool betrayal); the method takes the FILTER, not an id list,
  past a page-size threshold.
- **Timeline automatics**: `[@after]` hooks on stage-move/assign/import write the entries —
  reactions again doing product work; notes are user-authored entries in the same collection.
- **The SVG dashboard**: computed server-side, rendered as sized `<rect>`s with real numbers in
  `<title>`s — proof you don't need a chart lib for honest internal graphs.
- **Stale-deal nudge**: the `[@cron]` finds deals untouched 14 days, emails the owner a short
  list (one email per rep per day max), and timestamps the nudge so it never nags twice.
- **Export fidelity**: the CSV round-trips through the importer cleanly (the ultimate
  serialization test — write it as a system test).
- Discover first: `fennec discover "parse an uploaded CSV"`,
  `fennec discover "download a generated file"`, `fennec discover "bulk update matching documents"`.

## Sources

[Jetadmin — CRM portals for business](https://www.jetadmin.io/blog/6-best-crm-portals-for-your-business/) ·
[Softr — CRM with client portal](https://www.softr.io/blog/crm-with-client-portal) ·
[OctopusPro — CRM for bookings/service businesses](https://octopuspro.com/customer-relationship-management/) ·
[HubSpot — 28 types of websites (membership)](https://blog.hubspot.com/website/types-of-websites)
