# HTTPS — in-process TLS termination + automatic ACME certificates

Fennec terminates HTTPS **in-process** — no nginx, no reverse proxy. Two modes, one parameter each
on `Fennec.serve`:

| You have… | Use | What happens |
|---|---|---|
| a certificate (certbot, cloud, corporate CA, self-signed dev) | `~tls` | terminate TLS with it |
| just a domain + an email | `~acme` | obtain + auto-renew Let's Encrypt certs |

Both ride the pure-OCaml TLS stack (`tls` / `x509` / `mirage-crypto`) — no OpenSSL, no Lwt. See the
[build-matrix note](#build-matrix--static-binaries) for the one native dependency this pulls in.

## Bring your own certificate — `~tls`

```ocaml
let () =
  Fennec.serve
    ~tls:(Fennec.Tls.of_files ~cert:"/etc/ssl/site.pem" ~key:"/etc/ssl/site.key")
    [ web; admin ]
```

`Fennec.Tls.of_files` loads a PEM certificate chain + private key; `of_pem` takes the PEM strings
directly. A failed TLS handshake (a plain-HTTP client, an SNI mismatch) drops that one connection —
it never errors the server.

## Automatic certificates — `~acme`

```ocaml
let () =
  Fennec.serve
    ~acme:(Fennec.Acme.auto ())   (* email from FENNEC_ACME_EMAIL *)
    [ web; admin ]
```

That one line is the whole setup, and it's **dev-safe** — it does nothing in development (plain HTTP)
and turns into real HTTPS in production:

| Environment | Binds | Behavior |
|---|---|---|
| **dev** | `:4000` | plain HTTP — `~acme` no-ops, so a dev build never touches Let's Encrypt |
| **prod** (`FENNEC_ENV=production`) | **`:443`** (app) + **`:80`** | auto-HTTPS: `:80` serves the HTTP-01 challenge and `301`-redirects everything else to `https://` |

In production on boot Fennec **derives the domains** from your endpoints' concrete hosts, **obtains**
a certificate (or loads the cached one) *before* `:443` accepts a connection, and runs a **renewal
loop** that re-issues under 30 days and **hot-reloads** the live cert with no restart. The single
`:80` listener does double duty — ACME challenge *and* HTTP→HTTPS redirect — so there's no port
conflict and no manual redirect wiring.

### Configuration — sane defaults, env-overridable

Everything has a default; nothing is required in code beyond opting in. Env overrides code, so ops
can tune a build without recompiling:

| Knob | Default | Purpose |
|---|---|---|
| `FENNEC_ACME_EMAIL` | — (or `~email`) | ACME account email; absent ⇒ HTTPS stays off (logged) |
| `FENNEC_ACME` | prod-only | `1`/`0` to force ACME on/off regardless of env |
| `FENNEC_ACME_STAGING` | `0` | `1` ⇒ Let's Encrypt staging (untrusted, no rate limits — for testing) |
| `FENNEC_ACME_DIR` | `$XDG_STATE_HOME/fennec/acme` | the file cert store directory |
| `FENNEC_PORT` | dev `4000` / prod `443` (TLS) or `80` | override the base port |

In code, `~domains` overrides the derived set and `~store` swaps the cert store (below).

### Which domains get certificates

The ACME challenge type is forced by the endpoint's host pattern, so only some are auto-certifiable:

| Host pattern | Example | How it's certified |
|---|---|---|
| concrete host | `example.com`, `admin.example.com` | ✅ HTTP-01, automatic (one SAN cert) |
| wildcard | `*.example.com` | ✅ DNS-01 — needs `~dns_provider` (one cert covers every tenant subdomain) |
| dynamic / customer domains | added at runtime | ✅ on-demand — needs `~on_demand` (issued on first connect) |

Concrete hosts are always certified automatically. Wildcards and runtime domains are the
**multi-tenant** story below.

### Multi-tenant: wildcards (DNS-01) and on-demand

Two patterns, both opt-in:

**Subdomain-per-tenant** (`tenant1.app.com`, `tenant2.app.com`, …) → one **wildcard** cert via
DNS-01. Implement a tiny DNS provider for your DNS host and pass it; a `*.app.com` endpoint then
gets `*.app.com` certified:

```ocaml
let cloudflare : Fennec.Acme.dns_provider = {
  upsert_txt = (fun ~name ~value -> Cloudflare.set_txt ~name ~value);
  remove_txt = (fun ~name -> Cloudflare.del_txt ~name);
}
let () = Fennec.serve ~acme:(Fennec.Acme.auto ~dns_provider:cloudflare ()) [ app ]  (* app roots *.app.com *)
```

**Customer-brought domains** added at runtime (`theirbrand.com`, pointed at you by the customer) →
**on-demand**: the cert is obtained the first time that host connects, then cached. Gate it with an
allowlist so only domains you recognize can trigger issuance (no SNI-flood abuse):

```ocaml
let () =
  Fennec.serve
    ~acme:(Fennec.Acme.auto ~on_demand:(fun host -> Customers.domain_exists host) ())
    [ app ]
```

## Certificate storage (Ops)

Where the account key + certificate live depends on the deployment — so it's pluggable
(`Fennec.Cert_store`). The default is a **file** store; override `~store` when that doesn't fit:

| Deployment | Store | Why |
|---|---|---|
| VM / bare metal / docker volume / k8s **PVC** | `Cert_store.file ~dir` (default) | survives restarts |
| ephemeral container (k8s **without** a PVC) | **external** (k8s Secret / S3 / Redis / DB) | else every restart re-issues → rate-limit |
| multi-replica / HA | **shared external** + the lease | one replica issues, the rest read |
| dev / test | `Cert_store.memory ()` | no disk |

The default file store writes atomically (`0600`) under `$FENNEC_ACME_DIR` (else
`$XDG_STATE_HOME/fennec/acme`). An external backend is just a value — implement the four-function
record (`get` / `put` / `delete` / `with_lease`) over your store; no cloud SDKs are baked into Fennec:

```ocaml
let redis_store : Fennec.Cert_store.t = {
  get        = (fun key -> Redis.get key);
  put        = (fun key v -> Redis.set key v);
  delete     = (fun key -> Redis.del key);
  with_lease = (fun key f -> if Redis.setnx key then (Fun.protect ~finally:(fun () -> Redis.del key) f; true) else false);
}
let () = Fennec.serve ~acme:(Fennec.Acme.auto ~email:"ops@example.com" ~store:redis_store ()) [ web ]
```

### Multi-instance (the thundering herd)

If N replicas boot at once and all see "no certificate", they'd all order at once — and Let's
Encrypt's *duplicate-certificate* limit is low (5/week). `with_lease` is the fix: only the lease
holder issues; the others wait for the certificate to appear in the shared store. The `file` store's
lease is an `O_EXCL` lockfile (fine on a shared POSIX volume); an external store implements a real
distributed lock (Redis `SETNX`, etc.).

## Renewal + hot-reload

The renewal loop checks every ~12 h and re-issues under 30 days to expiry. The server reads the
certificate from a live source **per connection**, so a renewal swaps it instantly — new connections
get the new cert, in-flight ones finish on the old, nothing restarts.

## Behind a reverse proxy or PaaS

If something else terminates TLS — nginx, Caddy, a cloud load balancer, a k8s ingress, or a PaaS
(Heroku / Render / Fly) — the app should serve **plain HTTP** on the port it's handed and let the
edge do HTTPS. That's the default path: **don't** pass `~tls`/`~acme`, and either set `FENNEC_PORT`
or let the platform's `$PORT` be picked up automatically.

```sh
# nginx/Caddy/ingress forwards to the app on a high port:
FENNEC_PORT=8080 ./server
# on a PaaS that injects $PORT, nothing at all — fennec binds it:
./server
```

Port precedence is `FENNEC_PORT` › `$PORT` (prod) › `443` (when terminating TLS in-process) › `80`.
The proxy nuances are handled: `Force_https` and the session's Secure-cookie logic honor
`X-Forwarded-Proto`, and rate-limiting keys off `X-Forwarded-For` (so a client behind the proxy is
identified correctly, not lumped under the proxy's IP). Do **not** combine a proxy with `~acme`/`~tls`
— that makes the app *also* terminate TLS and bind `:80`, fighting the proxy.

## Build matrix / static binaries

The TLS stack is pure OCaml, but `mirage-crypto`'s RSA/DH math uses `zarith`, which links
**`libgmp`** — the one external C library HTTPS pulls in (the digestif / mirage-crypto / fiat-crypto
C *stubs* are baked into the binary and need nothing external). For a self-contained, statically
linked binary across the mac + linux (incl. musl) matrix, `libgmp` must be **statically linked**, not
left as a dynamic dependency:

- **Alpine / musl (fully static):** install `gmp-dev` (provides `libgmp.a`) and link with
  `-ccopt -static`; the linker then bakes in `libgmp.a` along with musl.
- **glibc Linux:** link `libgmp.a` (from `libgmp-dev`) statically, or ship `libgmp.so` alongside.
- **macOS:** fully-static isn't possible (libSystem is always dynamic), but `libgmp.a` (from
  `brew install gmp`) is linked in; only OS libraries remain dynamic.

Verify the result links only OS libraries (no `libgmp.*.dylib` / `libgmp.so`): `otool -L` on macOS,
`ldd` on Linux. This is the same "self-contained binary" guarantee the vendored libmongoc already
upholds; the portability check enforces it for the Mongo driver and the same allowlist applies here.
