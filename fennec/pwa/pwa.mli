(** PWA support, framework-owned: ONE declaration per app generates the manifest, the service
    worker, and the registration head snippet — no Workbox, no userland SW file. Because the
    framework owns the asset graph, the SW's precache list and cache version are {e exact}: the
    version digests the precached assets' contents, and activation atomically deletes older caches.
    Navigations are network-first (SSR freshness) with offline fallback to the last seen copy of the
    page — whose embedded data seed restores its content through the normal hydration path. Multiple
    PWAs per origin work via per-app [scope]s (one subpath each); multitenant-by-host is separate
    origins. Updates are user-confirmed: the new worker waits, {!Pwa_client.update_available} flips,
    [apply_update] swaps and reloads. *)

(** One manifest icon. *)
type icon

(** [icon ?mime ~sizes src] — e.g. [icon ~sizes:"512x512" "/icon-512.png"] (mime defaults to png). *)
val icon : ?mime:string -> sizes:string -> string -> icon

(** A PWA declaration for one app. *)
type t

(** [v ?short_name ?scope ?start_url ?display ?theme_color ?background_color ~icons name] — [scope]
    is the app's mount path (normalized to a trailing ["/"]; it is also the manifest scope and where
    [manifest.webmanifest] / [sw.js] are served); [start_url] defaults to [scope]; [display]
    defaults to ["standalone"]. *)
val v :
  ?short_name:string ->
  ?scope:string ->
  ?start_url:string ->
  ?display:string ->
  ?theme_color:string ->
  ?background_color:string ->
  icons:icon list ->
  string ->
  t

(** The generated web-app manifest (JSON). Exposed for tests; {!paw} serves it. *)
val manifest : t -> string

(** The generated service worker (JavaScript). Exposed for tests; {!paw} serves it. *)
val service_worker : t -> version:string -> precache:string list -> string

(** The digest over the precached assets' contents that names the cache — a redeploy with any
    changed asset rolls the SW cache atomically. *)
val version_of : assets:(string -> string option) -> string list -> string

(** The [<head>] snippet: manifest link, theme color, and the registration script (which surfaces
    the [fennec:sw-update] event and [window.__fennecApplyUpdate] for {!Pwa_client}). Include it in
    the app's document template. *)
val head_html : t -> string

(** [paw cfg ~assets ~precache] serves [<scope>manifest.webmanifest] and [<scope>sw.js] (with
    [Service-Worker-Allowed: <scope>]); [assets] is the same lookup the static paw serves from, and
    [precache] the asset URLs to install offline (bundles, css, icons). Declines everything else. *)
val paw : t -> assets:(string -> string option) -> precache:string list -> Fennec_paw.Paw.t
