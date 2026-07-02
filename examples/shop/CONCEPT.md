# Fennec Supply Co. — the e-commerce archetype

A desert-expedition gear store: catalog, cart, checkout, orders — **without a payment provider**.
Order placement is the fleet's flagship *workflow transaction* (decrement inventory + create order +
send confirmation, atomically). This is the archetype with the highest expectation baggage: everyone
knows what a shop must do, so every corner cut is visible.

## The category

E-commerce is its own industry-sized family: storefronts, digital-goods shops, single-product
brands, local boutiques, plus the coupon/comparison satellites
([HubSpot](https://blog.hubspot.com/website/types-of-websites),
[Forbes](https://www.forbes.com/advisor/business/software/types-of-websites/); every gallery has an
Ecommerce category — [Vercel](https://vercel.com/templates/)). The defining traits:

- **The catalog is SEO surface** (product pages with structured data, categories, search/filters)
  while **the cart/checkout is transactional state** — two very different halves in one app.
- **Inventory is a consistency problem**: overselling the last item is THE classic bug; carts,
  stock, and orders need clear atomicity boundaries.
- **The order is a state machine** (placed → paid → shipped → done/cancelled) with emails at the
  transitions; real shops live in the admin half as much as the storefront.
- Payments are always a *provider integration* (Stripe et al.) — which is exactly why the toy
  designs the **seam** and stops there: an order goes to `awaiting_payment` with a documented
  "wire your provider here" hook, keeping the example provider-neutral and offline-runnable.

## The app

**Fennec Supply Co.** — outfitting desert expeditions since forever. Features:

- Storefront: `/` (featured + categories), `/gear/:slug` (product page), `/category/:cat`,
  search + filter (category, price band, in-stock).
- Products: name, slug, description, price (integer cents!), images, category, stock count.
  Seeded catalog: sand goggles, burrow shovels, ember stoves, night-howl whistles…
- Cart: session-backed (works logged-out), add/remove/quantity, a cart badge on every page.
- Checkout: address + email form → **the placement workflow** → confirmation page + email.
  No card fields anywhere; the order lands as `awaiting_payment` with the provider seam stubbed.
- Account-optional: guests check out with an email; an account shows order history.
- Admin app: orders list with status transitions (mark paid / shipped / cancelled — each sends
  the right email), low-stock view, product CRUD.

## How it works (the fennec shape)

Axes: SSR storefront + islands (cart interactions) · CRUD + transactional writes · optional
accounts + admin roles · forms (checkout) + a few methods (cart) · async = emails via outbox,
a low-stock `[@cron]` digest.

- Storefront pages are SSR (SEO), with small islands: the add-to-cart button (optimistic badge
  bump) and the cart page quantities. Checkout is a **handler** form (typed via Sift) — the
  post/redirect/get flow, deliberately NOT a SPA.
- The cart lives in the session (id list + quantities), priced server-side on every render —
  never trust a client-side price.
- **The placement workflow** (`let[@workflow] place_order`): re-validate stock, decrement
  inventory, create the order, enqueue the confirmation email — commit or roll back as one unit.
  A raised workflow (stock raced away) re-renders checkout with the honest "sold out while you
  shopped" state. This is the transactional-integrity showcase of the whole fleet.
- Status transitions are small workflows too, each `[@after]`-hooked to its email.

Target layout:

```
examples/shop/
  server.ml            # web + admin endpoints
  web/
    apps/main/         # storefront shell
    apps/admin/        # back office
    components/        # product_card.mlx, cart_badge.mlx, filters.mlx, order_row.mlx …
    handlers/          # checkout.mlx (view + load + submit), admin/product_form.mlx
    store/             # products.ml, orders.ml (codecs, collections, indexes)
  workflows/           # place_order.ml, transitions.ml, low_stock_digest.ml
  seed/                # the catalog + demo images
  test/
    http/              # catalog, filters, cart math, checkout happy/sad, admin auth
    browser/           # add-to-cart badge, checkout flow end-to-end
    system/            # the oversell race: two concurrent checkouts, one wins, stock never < 0
```

## Implementation hints

- **Money is integer cents** with one formatting helper — no floats anywhere; test the rounding
  edges (quantity × price, totals).
- **The oversell test is the trophy**: a system test firing two concurrent checkouts for the last
  item — exactly one order succeeds, stock ends at 0, the loser sees the sold-out state. This
  proves the workflow transaction claim with a real race.
- **Product images**: thumbnails + product sizes generated via `fennec image` at seed time —
  the example uses the shipped pipeline; `srcset` on cards.
- **Structured data**: JSON-LD `Product` + `Offer` on product pages (price, availability), OG
  images — the SEO half done properly.
- **Cart correctness**: line-item snapshot copied ONTO the order at placement (products change
  later; orders don't); cart survives login (merge session cart into account cart — document the
  chosen merge rule).
- **The payment seam**: one module (`payments.ml`) with the narrow intended interface
  (`start_payment : order -> [redirect | skip]`) and a default no-op provider that marks the flow
  `awaiting_payment` — the comment block IS the integration guide.
- **Emails**: confirmation, shipped (with a fake tracking number), cancelled — all through the
  outbox; the dev mailbox makes the whole loop visible locally.
- **Admin niceties**: status buttons guard illegal transitions (state machine as a `match`, not
  ifs); low-stock (< 3) view + a weekly `[@cron]` digest email to the shopkeeper.
- **Search/filter**: server-side, indexed fields, URLs carry the filter state (`?cat=&max=` —
  shareable, testable); an http test per filter combination.
- Discover first: `fennec discover "run several writes as one transaction"`,
  `fennec discover "resize an image"`, `fennec discover "send an email after a workflow"`.

## Sources

[HubSpot — 28 types of websites (Ecommerce, Coupon)](https://blog.hubspot.com/website/types-of-websites) ·
[Vercel — ecommerce templates](https://vercel.com/templates) ·
[Forbes — types of websites](https://www.forbes.com/advisor/business/software/types-of-websites/) ·
[Kombee — types of business websites](https://www.kombee.com/blogs/business-website-types)
