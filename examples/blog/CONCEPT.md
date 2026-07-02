# The Burrow Post — the content / publication archetype

A desert magazine: articles with tags and authors, an RSS feed, and a private authoring app. This is
the **read-mostly + write-rarely** shape behind blogs, news sites, documentation, knowledge bases,
and wikis — the first app in the fleet with a database and the first with an admin surface.

## The category

Content sites are the second-biggest family after brochures: blogs and magazines, news, docs and
knowledge bases, wikis, recipe and course sites
([HubSpot's 28 types](https://blog.hubspot.com/website/types-of-websites) counts at least six
flavors; every template gallery has a Blog + Documentation category —
[Vercel](https://vercel.com/templates)). The defining traits:

- **The content outlives the visit.** URLs are permanent (slugs, canonicals), feeds syndicate
  (RSS/Atom), and search engines are the real audience half the time.
- **Two faces**: the public read side (fast, SEO'd, cacheable) and the private write side (an
  editor with drafts and preview). Most "CMS" complexity is just this split done carefully.
- **Chronology + taxonomy**: publish dates, tag/category pages, pagination, archives — boring,
  expected, and where sloppy implementations leak (off-by-one pagination, duplicate tag URLs).

## The app

**The Burrow Post** — "news from under the sand." Features:

- Public: `/` (latest, paginated) · `/post/:slug` · `/tag/:tag` · `/authors/:handle` · `/feed.xml`.
- Articles: title, slug, markdown body, tags, author, hero image, published-at, draft flag.
- Authoring: a separate **admin app** (second endpoint) with login, an article list (drafts
  first), a markdown editor with live preview, and one-click publish/unpublish.
- Content is fennec-world flavored: dispatches about dune life, burrow engineering deep-dives,
  interviews with local foxes — enough seed articles to make pagination and tags real.

## How it works (the fennec shape)

Axes: SSR rendering (public) + a lightly-interactive admin · **content collections** in the embedded
DB · **accounts for authors only** (public side is anonymous) · forms for authoring · async = none.

- Two endpoints: `web` (public) and `admin` (author-only, `Accounts` login + `require_user`).
- An `articles` collection defined by one Sift codec — the same shape drives validation, the
  editor's form, and the `$jsonSchema` the database enforces.
- Public pages query with typed filters (`published = true`, tag membership) and render fully
  server-side; the ONLY public JS is none at all (reading needs no bundle).
- The markdown pipeline runs server-side at render (or pre-rendered at save — an implementation
  decision to document either way); the editor preview reuses the same renderer.
- Seed content ships via the dev seed path (durable dev DB, seed-if-empty), so `fennec dev` opens a
  living magazine, not an empty shell.

Target layout:

```
examples/blog/
  server.ml            # two endpoints: web + admin
  web/
    apps/main/         # public shell
    apps/admin/        # authoring shell (own css, own bundle)
    components/        # article_card.mlx, tag_list.mlx, pagination.mlx, editor.mlx …
    handlers/          # admin/article_form.mlx (view + load + submit)
    store/             # articles.ml (the Sift codec + collection def)
  seed/                # the starter articles (markdown + front-matter or .ml data)
  test/
    http/              # routes, pagination edges, feed validity, draft invisibility
    browser/           # editor preview updates; publish flow
```

## Implementation hints

- **Slugs are forever**: generate from the title once, never regenerate on edit; unique index on
  `slug`; 404 unknown slugs (no fuzzy redirects in the toy).
- **Drafts must not leak**: every public query filters `published`; add an http test that a known
  draft slug 404s publicly while the admin sees it — the classic CMS bug, pinned by a test.
- **RSS/Atom**: hand-built XML (it's ~30 lines — no dep), correct `Content-Type`, absolute URLs,
  RFC-822 dates; validate the feed shape in an http test. Per-tag feeds are a one-line extension
  that impresses.
- **Pagination**: `?page=N`, stable ordering (published-at DESC, `_id` tiebreak), `rel=prev/next`
  links in `Head`, and an http test for the last-page edge.
- **Reading niceties**: reading-time estimate (word count / 200), published + updated dates,
  author bio block, prev/next article links.
- **Markdown**: pick the smallest correct path (a vendored pure-OCaml renderer or a strict subset)
  and document the choice in the code header; escape everything by default — raw HTML in articles
  is off unless explicitly enabled (XSS posture; `Fur.raw` only around the SANITIZED render).
- **Admin ergonomics**: drafts-first list with status chips, save-and-preview without publish,
  a "view public" link per article; login via `Accounts` defaults (password), seeded author user.
- **Caching**: public pages ETag'd; the feed gets a short max-age; admin `no-store`.
- **Testing**: http covers routes/feeds/drafts/pagination; browser covers the editor
  (type → preview updates → publish → appears on `/`). System cut not needed.
- Discover first: `fennec discover "define a typed collection"`,
  `fennec discover "require a logged-in user for an endpoint"`,
  `fennec discover "seed the dev database"`.

## Sources

[HubSpot — 28 types of websites](https://blog.hubspot.com/website/types-of-websites) ·
[Vercel — blog templates](https://vercel.com/templates) ·
[Forbes — types of websites](https://www.forbes.com/advisor/business/software/types-of-websites/) ·
[Ramotion — website taxonomy](https://www.ramotion.com/blog/website-taxonomy/)
