# Sandpaw Coffee — the brochure / one-pager archetype

A desert roastery's marketing site: a handful of server-rendered pages, one playful client island,
one contact form. **The simplest thing people ship, and the most common** — this app is the floor of
the fleet: it proves fennec is pleasant when you need almost nothing, and its folder layout teaches
"you only pay for what you use" by what it *doesn't* contain.

## The category

The presence/marketing family is the largest slice of the web by count: landing pages, business and
brochure sites, portfolios and resumes, nonprofit/cause sites, event and wedding pages, restaurant
and local-storefront sites ([HubSpot's 28 types](https://blog.hubspot.com/website/types-of-websites),
[Forbes — types of websites](https://www.forbes.com/advisor/business/software/types-of-websites/)).
The defining traits:

- **Read-mostly, SEO-critical.** Content changes rarely; discovery is organic search + social
  shares, so meta/OG tags, structured data, sitemaps, and fast first paint are the whole game.
- **One conversion action.** A landing page "eliminates distractions and guides visitors toward
  completing one specific action" ([nonprofit landing-page guide](https://www.trajectorywebdesign.com/blog/nonprofit-landing-page-best-practices)) —
  here: the contact/order-inquiry form.
- **Optional client-side delight.** Static at heart, but a small interactive moment (a configurator,
  a gallery, a map) is common — an *island*, not an app.

Every static-site generator and template gallery leads with this family
([Vercel — marketing site templates](https://vercel.com/templates)); a fullstack framework must make
it feel *lighter* than reaching for a generator, not heavier.

## The app

**Sandpaw Coffee** roasts beans in the dunes. Pages:

- `/` — the one-pager: hero, story, three signature roasts, visit-us block (hours + map image), and
  the contact form.
- `/roasts` — the small catalog page (static content, no commerce — that's `shop/`'s job).
- The island: **the Blend Builder** — pick body/acidity/roast on three sliders, get a playful
  recommendation ("Dune Dust — for the bold") computed client-side. Pure whimsy, zero backend.
- The form: name + email + message → validation → an email to the roastery via the outbox, plus a
  friendly SSR "thanks" state. No database collection needed — the email IS the record.

## How it works (the fennec shape)

Axes: **SSR-only** rendering (+1 hydrated island) · **no database** · **no accounts** · forms only ·
async = one outbox email.

- One `Paw.Endpoint` with the standard middleware trio (`Logger`, `Security_headers`,
  `Normalize_path`) and `Fennec.static` for assets.
- Pages are `.mlx` components rendered via the SSR handler; the Blend Builder is the only component
  whose signals matter client-side — everything else ships as static HTML.
- The contact form is a **handler** (`web/handlers/contact.mlx`: view + typed `submit` via a Sift
  codec) — the server-rendered form story, no SPA machinery.
- `Mail.send` for the notification; in dev it lands in the captured dev mailbox (`MAIL_URL` unset →
  logged/captured), so the loop is testable with zero SMTP setup.

Target layout (note the absences — no `store/`, no `workflows/`, no admin app):

```
examples/brochure/
  server.ml            # endpoint + serve, ~40 lines
  web/
    apps/main/         # the one app shell (document, head, css manifest)
    components/        # hero.mlx, roasts.mlx, blend_builder.mlx, footer.mlx …
    handlers/          # contact.mlx (view + submit)
  assets/              # images, favicon set
  test/
    http/              # pages 200, form happy/sad paths, headers
    browser/           # the island hydrates; slider → recommendation text
```

## Implementation hints

- **SEO pass is the point.** Per-page `<title>`/description via Fur's `Head`; OG + twitter cards;
  JSON-LD `LocalBusiness` (hand-written `Fur.raw` block — server-only, exactly what `raw` is for);
  `sitemap.xml` + `robots.txt` as two tiny routes; canonical URLs.
- **Favicons via the CLI**: generate the whole set from one source PNG with `fennec image` — the
  example should *use* the toolchain it ships with.
- **Form hygiene**: Sift codec validation (name/email/message lengths), a honeypot field (paw-level
  reject, no error shown), a per-IP `Rate_limit` on POST, CSRF via the session paw, and a
  post/redirect/get "thanks" state so refresh never re-submits.
- **Cache posture**: static assets far-future (prod) via the built-in content-aware caching; pages
  ETag'd for cheap 304s. No client bundle on pages without the island — verify with the browser test
  that only `/` loads JS.
- **The 404**: a branded not-found page via `Status_pages` / `~on_error` — small, but it's the first
  thing a reviewer clicks.
- **Testing**: `let%http` — every page 200 + key strings, form validation errors render inline, the
  honeypot 200s-but-sends-nothing, security headers present. `let%browser` — the Blend Builder
  hydrates and reacts. No system cut needed.
- **Done when**: `fennec dev` boots it with zero config, `fennec test all` is green, Lighthouse-style
  sanity holds (no JS on static pages, images sized), and the whole server.ml fits on one screen.
- Before wiring anything unfamiliar: `fennec discover "send an email from a form"`,
  `fennec discover "set per-page meta tags"`.

## Sources

[HubSpot — 28 types of websites](https://blog.hubspot.com/website/types-of-websites) ·
[Forbes — types of websites and their features](https://www.forbes.com/advisor/business/software/types-of-websites/) ·
[Trajectory — nonprofit landing page best practices](https://www.trajectorywebdesign.com/blog/nonprofit-landing-page-best-practices) ·
[Orizon — landing pages that convert (2025)](https://www.orizon.co/blog/our-10-favourite-landing-page-designs-in-fall-2025-and-why-they-convert) ·
[Vercel — templates gallery](https://vercel.com/templates)
