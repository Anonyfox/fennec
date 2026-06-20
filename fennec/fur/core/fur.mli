(** The frozen public contract of the Fur runtime core.

    Abstract types make invalid states unrepresentable; this [.mli] is also a recompile
    firewall — editing the [.ml] body never recompiles dependents. Internals (reaction,
    run_effect, the effect tracker, flatten/escape, take_seed, Head counter, etc.) are hidden.

    {2 The component shape — ONE function returning JSX}

    A component is, at runtime, a SETUP thunk returning a RENDER thunk ({!comp} takes
    [unit -> (unit -> vnode)]). The boundary is the same as Solid/Svelte's "component body runs
    once, reactivity lives in the markup":
    - {b SETUP runs ONCE} per mounted instance — every [let … in] (and [let open … in], a
      [let () = …] side effect) in the body: create {!signal}s, {!Pulse.subscribe}, {!on_mount},
      derive-once values.
    - {b RENDER re-runs} whenever a signal it {!get}s changes — and the render is the {b trailing
      expression of the body}. The same code drives SSR and post-hydration interactivity.

    In a [.mlx] file the author writes the render as that trailing expression and the fur ppx
    emits the [fun () ->] wrapper — there is NO manual render-thunk split:

    {[
      (* the .mlx source — what userland writes: setup above, render JSX trailing. The markup is
         JSX-identical: bare text is text, {expr} interpolates a value, <Component/> nests. *)
      let make ?(label = "count") () =
        let count = signal 0 in              (* SETUP — runs once per <Counter/> *)
        <span className="counter">           (* RENDER — re-runs on `count` change *)
          {label ^ ": "}
          <button onClick=(count -= 1)>−</button>
          <span className="count">{!count}</span>
          <button onClick=(count += 1)>+</button>
        </span>
    ]}

    Resolution rules:
    - {b [let make … () = <trailing render>]} — a component whose param list ends in [()] (the
      instance contract; the JSX desugar always calls [make ~props ()]). The ppx wraps the
      trailing expression as the render thunk. The explicit [let make … () = … fun () -> <jsx>]
      shape still compiles unchanged (it is detected as already-thunked) — migration is opt-in.
    - {b [let view = <jsx>]} — a component with NO local state: module-level [view] (+ any
      module-level setup bindings) is folded into the same [make] for you.
    - {b [let make ctx = <html>…]} — a non-unit final parameter is a server-only document shell
      (a plain [ctx -> vnode]), NEVER wrapped: the full-power escape hatch.

    {b The one footgun} (the same one Solid has): a setup [let x = get s in …] computes [x] ONCE
    at mount, so it is NOT reactive. Read the signal IN the markup ([(get s)] / [(!s)]) or use a
    {!memo}. The ppx emits a non-fatal warning on exactly that pattern (a setup binding whose RHS
    is a bare tracking read [get s] / [!s]); {!peek} (a deliberate snapshot) never warns.

    {2 JSX surface in a [.mlx] component file}  (JSX-identical; pure ppx + pre-pass, no type-safety loss)

    The markup is the React/Svelte surface a frontend developer already knows — bare text, [{expr}]
    interpolation, [<Component/>], scoped [[%%style]] — with full OCaml type safety underneath. The
    child-text surface is {b exactly HTML/JSX, with no fennec-specific surprise}: whitespace collapses
    (HTML rule), [{expr}] is the one value escape (JSX rule), a literal [{] or [<letter] must be escaped
    or quoted (JSX rule), and a literal [(] is just text. The one-line table:
    {v
      whitespace collapses / trims  → HTML-standard
      {expr} is the value escape     → JSX-standard ({} is the ONLY escape)
      { and <letter need escaping    → JSX-standard
      ( ) and prose punctuation      → plain text (no escape — JSX adds none either)
    v}
    - {b bare text is text}: [<h1>Welcome to Fennec 🦊</h1>], [<a>Sign in</a>] ([in] is an OCaml
      keyword, yet it reads fine — a fennec-owned [.mlx] pre-pass quotes child text BEFORE the parser,
      run in-process by [fennec mlx-pp] over a vendored mlx parser, see [fennec/fur/prepass/]). A
      literal [(] is text like any other character:
      [<p>Call us (now) — it's free!</p>] needs no quoting. Whitespace collapses/trims like JSX (source
      indentation between elements is dropped; inline runs collapse to one space).
    - {b [{expr}] interpolates a VALUE} (JSX-identical) — auto-wrapped in {!node}, so an
      [int]/[float]/[string]/[vnode] all render and userland never writes [node]:
      [<span>{!count}</span>], [<span>{label ^ ":"}</span>], [<h3>{(Data.value info).name}</h3>].
    - {b [{expr}] is ALSO how you nest a list/conditional/match whose body is itself JSX} (JSX-identical:
      [{}] is the single escape for values AND control flow) — a child that evaluates to a [vnode list]
      (an [each] / [List.map]) auto-wraps in {!frag}, so the idiom is [{each xs (fun x -> …)}] — no
      [frag], no [Array.to_list] for a list — and a conditional is [{if !ready then <ul>…</ul> else
      <p>…</p>}], a match [{match x with … -> <span>…</span>}]. Nested JSX inside the [{…}] gets the same
      bare-text treatment, to any depth.
    - {b [ "string" ] quoted text is the escape hatch} for text that must contain a literal [{] or
      [<letter] (JSX-standard), or significant edge/double whitespace (HTML collapses it, so quoting is
      the HTML-standard way to force it — e.g. ["✉  Dev mailbox"] keeps the double space). A literal
      [(] does NOT need it.
    - [!s] reads a signal — the sugar for [(get s)] (writes stay explicit: [set]/[+=]/[-=]);
    - [attrs=(expr)] on a plain element SPREADS an [attr list] into the element's attributes (React's
      [{...spread}]) — e.g. [<input attrs=(Form.input_attrs Fields.email) name="email" />] drops the
      codec-driven HTML5 constraints ([required]/[maxlength]/[type=email]) straight onto the input.
      Repeatable; the spread merges after the element's own labeled attrs. (On a {!comp} it stays a
      normal [attrs] prop.) Attribute values accept [{expr}] too: [<input value={draft} />].

    The component above desugars to the runtime API below ([h]/{!node}/{!comp}/{!get}) — the ppx
    emits the [fun () ->] render thunk that wraps the trailing JSX:

    {[
      let make ?(label = "count") () =
        let count = signal 0 in
        fun () ->
          h "span" [ class_ "counter" ]
            [ text (label ^ ": ");
              h "button" [ on "click" (fun () -> count -= 1) ] [ text "−" ];
              h "span" [ class_ "count" ] [ node (get count) ];
              h "button" [ on "click" (fun () -> count += 1) ] [ text "+" ] ]
    ]} *)

(** {1 Reactivity} *)

(** A reactive value of type ['a]. Reading it inside a {!watch} or {!comp} body automatically
    tracks it as a dependency; writing it re-runs every dependent effect synchronously.

    This is the primitive for local client-side UI state: counters with +/- buttons, toggles,
    input text, selected tabs, optimistic form state, and other values owned by one component or
    page. It is not a server subscription; for cross-client/server live data use Pulse live data
    and read the returned signal in Fur. *)
type 'a signal

(** [signal ?eq init] — create a local reactive signal with initial value [init]. [eq] is the
    equality predicate used to suppress spurious re-runs (default: structural [=]). A typical
    interactive widget creates [let count = signal 0], renders [get count], and updates it from
    click handlers with {!set}, {!update}, [+=], or [-=]. *)
val signal : ?eq:('a -> 'a -> bool) -> 'a -> 'a signal

(** Read the current signal value WITHOUT registering it as a dependency. Use inside effects
    that need the current value but must not re-run when it changes. *)
val peek : 'a signal -> 'a

(** Read the signal value, registering it as a dependency of the enclosing reactive context
    ({!watch} or {!comp}). In a [.mlx] file the fur ppx accepts [!s] as the sugar for [get s]
    (a JSX value child [(!count)], an attribute [value=(!draft)], or an ordinary expression);
    writes stay explicit ([set]/[update]/[+=]/[-=]). Signals are not OCaml refs — a real ref in
    a [.mlx] is read with [r.contents]. *)
val get : 'a signal -> 'a

(** Set the signal to a new value; notifies dependents synchronously. Use from browser event
    handlers for local UI interactions such as button clicks. *)
val set : 'a signal -> 'a -> unit

(** [update s f] — set [s] to [f (peek s)]: a read-modify-write that does not track [s].
    This is the usual form for counters and other incremental local state. *)
val update : 'a signal -> ('a -> 'a) -> unit

(** [watch f] runs [f] immediately, tracking every {!get} inside it, then re-runs
    automatically whenever any tracked signal changes. Returns a stop function: call it to
    detach the effect permanently. *)
val watch : (unit -> unit) -> (unit -> unit)

(** [flush_sync f] runs [f] with signal writes batched, then SYNCHRONOUSLY flushes every pending
    re-render before returning (React [flushSync]-style). Inside [f] each {!set} only marks its
    dependents dirty and defers; when [f] returns, all dependent effects have re-run and the DOM
    reflects the final state. Use it when code must observe the post-write DOM in the same tick
    (otherwise the browser flush is a microtask). Nestable: only the outermost call flushes.

    Writes that re-render the same component collapse to a single pass — so [flush_sync] is also
    the explicit way to batch N writes (e.g. resetting several fields) into one render. *)
val flush_sync : (unit -> unit) -> unit

(** [batch f] groups the signal writes in [f] so dependents re-render once, like {!flush_sync}
    but read as "batch these writes" rather than "force a synchronous flush". Identical behavior;
    nestable. *)
val batch : (unit -> unit) -> unit

(** [memo ?eq f] is a derived, cached signal: [f] is a pure computation over other signals, and
    {!get} on the result returns its last value, recomputing lazily on first read after any
    dependency changed. Reading the memo many times between changes recomputes [f] at most once,
    and with batching it recomputes once per flush — so it is glitch-free and cheaper than a bare
    [get] that re-derives every read. [eq] (default structural) suppresses downstream re-runs when
    the recomputed value is unchanged. *)
val memo : ?eq:('a -> 'a -> bool) -> (unit -> 'a) -> 'a signal

(** [s += n] — increment an int signal by [n]. Sugar for [update s (( + ) n)]. *)
val ( += ) : int signal -> int -> unit

(** [s -= n] — decrement an int signal by [n]. *)
val ( -= ) : int signal -> int -> unit

(** {1 Lifecycle} *)

(** [true] when running in the browser (js_of_ocaml context); [false] in native/SSR.
    Set by the runtime on hydration start; mutable so tests can override. *)
val is_browser : bool ref

(** [on_mount f] schedules [f] to run once immediately after the enclosing component's DOM
    is first inserted (browser-only; no-op in SSR). Use for subscriptions, focus, and
    one-time setup that requires a live DOM. *)
val on_mount : (unit -> unit) -> unit

(** [on_cleanup f] registers [f] to run when the enclosing component unmounts — the
    structural teardown for subscriptions registered via {!on_mount} or {!watch}. *)
val on_cleanup : (unit -> unit) -> unit

(** Flush all pending {!on_mount} callbacks — called by the reconciler after a batch of DOM
    mutations, before returning control to the app. Not for userland code. *)
val flush_mounts : unit -> unit

(** {1 Ambient event (read inside DOM event handlers)} *)

(** The current input value ([event.target.value]) — valid only inside a DOM event handler. *)
val target_value : unit -> string

(** The current checkbox state ([event.target.checked]) — valid only inside a DOM event handler. *)
val target_checked : unit -> bool

(** The key string ([event.key]) — valid only inside a keyboard event handler. *)
val key : unit -> string

(** Call [event.preventDefault()] on the current event — suppress the browser's default action. *)
val prevent_default : unit -> unit

(** Browser-only side-effecting APIs. SSR stubs are no-ops or return safe defaults. *)
module Browser : sig
  (** Read a value from [localStorage]; [None] when absent or in SSR. *)
  val local_get : string -> string option

  (** Write a value to [localStorage]. No-op in SSR. *)
  val local_set : string -> string -> unit

  (** Remove a key from [localStorage]. No-op in SSR. *)
  val local_remove : string -> unit
end

(** {1 Virtual DOM} *)

(** An immutable virtual DOM node — element, text, fragment, or component output.
    Compared structurally by the reconciler to compute minimal DOM patches. *)
type vnode

(** An element attribute or event listener, constructed with {!attr}, {!class_}, or {!on}. *)
type attr

(** A text node. *)
val text : string -> vnode

(** A verbatim HTML fragment inserted as-is (for pre-rendered templates or dynamic markup).
    Skipped by the reconciler's structural diff. *)
val raw : string -> vnode

(** A fragment: multiple sibling nodes with no wrapper element. In [.mlx] the ppx inserts this
    automatically around a child that evaluates to a [vnode list] (an {!each} / [List.map]), so
    userland writes [{each xs f}] — never [frag (each xs f)]. *)
val frag : vnode list -> vnode

(** [h ?key tag attrs children] — an HTML element. [key] is the reconciler's stable identity
    hint for list items (prefer {!with_key} when constructing from a list). *)
val h : ?key:string -> string -> attr list -> vnode list -> vnode

(** [comp ~cid ?key f] — a component. [cid] is its stable string identity used by the
    reconciler to match across renders. [f ()] is called once; it returns the render thunk
    that re-runs on signal changes. *)
val comp : cid:string -> ?key:string -> (unit -> (unit -> vnode)) -> vnode

(** [on event handler] — an event-listener attribute. [event] is the DOM event name
    (e.g. ["click"]); [handler] is called synchronously on each event. *)
val on : string -> (unit -> unit) -> attr

(** [attr name value] — an HTML attribute (e.g. [attr "href" "/about"]). *)
val attr : string -> string -> attr

(** [class_ name] — shorthand for [attr "class" name]. *)
val class_ : string -> attr

(** [node v] — coerce a heterogeneous child: pass an [int], [float], [string], or [vnode]
    as a child of {!h} without explicit conversion. In [.mlx] userland writes the value escape
    [{expr}]; the pre-pass lowers it to a paren value-child and the ppx inserts {!node} there, so
    userland never spells [node] (an unsupported child type is still a compile error). Idempotent:
    an explicit [{node x}] is not re-wrapped. *)
val node : 'a -> vnode

(** [skey v] — coerce a stable key to a string ([int] or [string]). *)
val skey : 'a -> string

(** [each xs f] — [List.map f xs] accepting a mixed-type list via coercion: the one idiom for
    rendering a dynamic list of children. In [.mlx] a child [{each xs f}] is auto-wrapped in
    {!frag} (so no [frag]); for an array, convert at the call site with [Array.to_list]
    ([{each (Array.to_list arr) f}]) — one [each] typed over both list and array is not
    expressible in OCaml without modular implicits. *)
val each : 'a list -> ('a -> 'b) -> 'b list

(** Attach a stable reconciler key to an existing vnode (use when [h ~key] is inconvenient,
    e.g. on a component returned by a helper function). *)
val with_key : string -> vnode -> vnode

(** Serialize a vnode tree to HTML markup (SSR). Does NOT prepend a doctype. *)
val to_html : vnode -> string

(** Like {!to_html}, but for each element it merges the declarations returned by [style] into that
    element's inline [style] attribute (the stylesheet decls go first, so an explicit inline [style=]
    still wins). [style] sees the element's tag and its plain string attributes. This is the generic
    seam the email CSS inliner builds on — it has no email knowledge of its own. *)
val to_html_styled : style:(tag:string -> attrs:(string * string) list -> string option) -> vnode -> string

(** [document root] — serialize the full page: [<!doctype html>] followed by {!to_html root}. *)
val document : vnode -> string

(** {1 Head} *)

(** Document [<head>] management: components set metadata (title, OG tags, links) at any
    nesting depth; child-wins ordering means a page component's title overrides a layout's.
    {!Head.to_ssr} collects the resolved set for the SSR shell. *)
module Head : sig
  (** A single [<head>] tag: title, meta, link, inline script, or JSON-LD. *)
  type tag =
    | Title of string
    | Meta of (string * string) list
    | Link of (string * string) list
    | Script of (string * string) list * string
    | Json_ld of string

  (** Tag constructors — convenience wrappers over the [tag] type. *)
  module Tag : sig
    val title : string -> tag
    val meta : name:string -> string -> tag
    val og : string -> string -> tag
    val link : rel:string -> ?attrs:(string * string) list -> string -> tag
    val script : ?attrs:(string * string) list -> ?body:string -> unit -> tag
    val json_ld : string -> tag
  end

  (** The reactive source registry for the CURRENT request (server) / document (browser) — [(priority,
      tags)] pairs, one per {!use} call. It is per-request (fiber-local) on the concurrent server and the
      single global on the browser, so concurrent SSR renders never share or leak head tags. *)
  val sources : unit -> (int * tag list) list signal

  (** [use f] registers a dynamic (reactive) tag list from [f ()]. Re-evaluated when any
      signal read inside [f] changes. *)
  val use : (unit -> tag list) -> unit

  val title : string -> unit
  val description : string -> unit
  val meta : name:string -> string -> unit
  val og : string -> string -> unit
  val link : rel:string -> ?attrs:(string * string) list -> string -> unit
  val json_ld : string -> unit
  val tag_key : tag -> string

  (** Merge all sources by priority (child-wins), deduplicating by {!tag_key}. Pure. *)
  val resolve : (int * tag list) list -> tag list

  (** Render the current resolved head as an HTML string for SSR injection. *)
  val to_ssr : unit -> string
end

(** {1 Data} *)

(** Async data resources with fast-render seeds.

    A resource is declared once (e.g. [let user = Data.string "user_id"]), loads on first
    render, seeds the hydrated client from the SSR pass (so the first client render is
    synchronous from the seed), and exposes {!Data.refetch} for mutations.

    Use {!Data} for request/SSR data that should be fetched and seeded. For UI-only values owned
    by a component, use {!signal}. For shared realtime collections, use Pulse. *)
module Data : sig
  (** A loaded (or loading) resource of type ['a]. *)
  type 'a t

  (** The current request's seed table ([key → JSON-string], filled by the server, read by the client
      on hydration). It is {b fiber-local} on the concurrent server (see {!with_context}), so
      simultaneous SSR requests never share it. Mutate via {!put_seed} / {!clear_seed}. *)
  val seed_table : unit -> (string, string) Hashtbl.t

  (** [with_context f] runs [f] with a fresh, isolated per-request data context (the seed table + the
      fetch source). The SSR driver wraps each concurrent render in it; on the browser / outside an
      Eio run it is the single global context. *)
  val with_context : (unit -> 'a) -> 'a

  (** Write a key/value pair into the current request's seed (server-side). *)
  val put_seed : string -> string -> unit

  (** Clear all seed entries (e.g. between requests in a test). *)
  val clear_seed : unit -> unit

  (** [set_source f] installs the fetch strategy for the current request: [f key callback] resolves a
      resource key to a JSON string and calls [callback] with it. Set by the app's data layer / the
      SSR driver. *)
  val set_source : (string -> (string -> unit) -> unit) -> unit

  (** [resource ~key ?client_only ~fallback ~decode ()] — declare a typed resource.
      [key] identifies it in the seed; [decode] parses the JSON string; [fallback] is the
      value while loading. [~client_only] skips SSR (useful for purely client-local state). *)
  val resource : key:string -> ?client_only:bool -> fallback:'a -> decode:(string -> 'a) -> unit -> 'a t

  (** [string key ?fallback ?client_only ()] — a string resource (no decode needed). *)
  val string : string -> ?fallback:string -> ?client_only:bool -> unit -> string t

  (** [model codec key ?client_only ~fallback ()] — a TYPED resource over a {!Sift} codec: the
      one-liner over {!resource} that decodes the seeded / fetched JSON with {!Sift.decode_json}
      (relaxed-JSON decode + the model's full validation), SSR-seeded + refetchable like {!string} but
      yielding the typed ['a]. A malformed / invalid payload falls back to [fallback] (the resource never
      crashes the UI on foreign garbage). The [codec] is the SAME ['a Sift.t] the server's
      {!Sift.encode_json} produced — wire shape = model shape, no second serializer. *)
  val model : 'a Sift.t -> string -> ?client_only:bool -> fallback:'a -> unit -> 'a t

  (** {2 Co-located resources — the SERVER fetcher declared inline}

      {!string} / {!model} key a resource to a path, but the path's VALUE still lives elsewhere
      (server.ml): an SSR seed source AND a separate HTTP route, both keyed by a magic string — the
      data is invisible from the component. {!local} / {!model_local} kill that split: the component
      declares the server fetcher {b in the same file}, server-only, and the framework auto-registers
      BOTH the SSR seed source and the HTTP refetch route (path derived from the name, or [~path]). The
      app author wires nothing — the same fusion {!Pulse.publish} performs for a live publication.

      The fetcher is {!Server_only.fn}-wrapped, so it is leak-proof two ways: it has no {!Sift} (it can
      never be seeded — only the value it produces is), and the fur ppx STRIPS its body from the jsoo
      bundle (like a handler's [load]) — a secret inside it is a compile error there, and no server logic
      reaches the client. On the client {!local}/{!model_local} are just the seeded, refetchable resource
      reading the path. *)

  (** A SERVER-ONLY value: NO {!Sift}, so it can never be seeded — and the fur ppx replaces its body in
      the client build, so the wrapped server logic never links into the jsoo bundle. *)
  module Server_only : sig
    type 'a t

    (** [fn f] wraps a server fetcher [f] (run on the server to produce a resource's value). *)
    val fn : (unit -> 'a) -> 'a t
  end

  (** [local name ?path ~fallback fetch] — a STRING resource whose SERVER fetcher is declared INLINE.
      The served path is [~path] (verbatim) or ["/api/" ^ name] (a [name] already starting with ["/"] is
      taken as the path). Declaring it registers [fetch] as both the SSR seed source and the auto-mounted
      refetch route for that path; the returned resource reads the seed and refetches the path exactly
      like {!string}. On the client the ppx strips [fetch] — only the path crosses. *)
  val local : string -> ?path:string -> fallback:string -> string Server_only.t -> string t

  (** [model_local codec name ?path ~fallback fetch] — the TYPED twin of {!local} over a {!Sift} codec:
      the inline server [fetch] yields an ['a], seeded with {!Sift.encode_json} and decoded back with
      {!Sift.decode_json} — typed end to end, no stringly key, no manual decode. The same codec drives
      the registered SSR seed and the mounted route, so wire shape = model shape on both surfaces. *)
  val model_local : 'a Sift.t -> string -> ?path:string -> fallback:'a -> 'a Server_only.t -> 'a t

  (** The CLIENT lowering of {!local}/{!model_local}, emitted by the fur ppx ([-data-client]) — NOT for
      userland. They take no fetcher (it is stripped) and skip the server-only registration, so a
      co-located resource compiles on the client to exactly the bare {!string}/{!model} over the derived
      path: zero client growth versus the un-co-located form. *)
  val local_client : string -> ?path:string -> fallback:string -> unit -> string t

  val model_local_client : 'a Sift.t -> string -> ?path:string -> fallback:'a -> unit -> 'a t

  (** {3 The registry the framework drains (not userland)} *)

  (** One registered co-located source: an opaque handle the framework's route mounter + SSR source
      read through {!src_produce} / {!src_is_json}. *)
  type local_src

  (** Run a source's inline fetcher and return its bytes (the seed value / the route body). *)
  val src_produce : local_src -> string

  (** [true] when the source's bytes are JSON ({!model_local}) — the auto-mounted route serves
      [application/json]; [false] is the plain-string {!local}, served [text/plain]. *)
  val src_is_json : local_src -> bool

  (** Derive the served path from a name + optional [~path] (the rule {!local}/{!model_local} use). *)
  val local_path : ?path:string -> string -> string

  (** All registered co-located sources as [(path, src)] pairs: each is one auto-mounted refetch route
      AND one SSR seed source. {!Fennec.serve} and the SSR driver drain this at boot. *)
  val local_sources : unit -> (string * local_src) list

  (** The source registered for a path, if any — the SSR driver's in-process source consults this. *)
  val local_source : string -> local_src option

  (** The current resolved value of a resource (the fallback while loading). *)
  val value : 'a t -> 'a

  (** [true] while the resource's first fetch is in flight. *)
  val loading : 'a t -> bool

  (** Trigger a fresh fetch of the resource's data (clears the cached value until resolved). *)
  val refetch : 'a t -> unit

  (** Escape a string value for safe embedding in a [<script>] tag. *)
  val js_string : string -> string

  (** Render all seed values as an inline [<script>] tag for fast-render hydration. *)
  val to_script : unit -> string
end

(** {1 Routing} *)

(** Pure route-pattern matching utilities (used internally by {!Router} and exposed for
    custom routing logic or testing). Patterns may contain [:name] segments and a trailing
    [*name] splat. *)
module Matcher : sig
  (** Captured path parameters: [(name, value)] pairs. *)
  type params = (string * string) list

  (** Split a URL path into non-empty segments. Pure. *)
  val segments : string -> string list

  (** [match_one ~pattern path] — try one pattern against a path; [Some params] on match. *)
  val match_one : pattern:string -> string -> params option

  (** [find routes path] — find the first matching [(pattern, payload)] and return
      [(payload, captured_params)]. *)
  val find : (string * 'a) list -> string -> ('a * params) option

  (** Look up a named capture in a params list. *)
  val param : params -> string -> string option
end

(** SPA client-side router: declares pages as pattern → render-thunk pairs, tracks the
    current path as a reactive signal, and renders the matching page at {!Router.outlet}.
    On the server the path is the request URL; in the browser it syncs with [window.location]
    via the History API.

    In generated Fur apps, route files such as [products/id_.mlx] become dynamic route patterns,
    typed path/link helpers, and mounted pages. Use the generated routes/paths for app navigation
    and {!param} / {!param_or} for captured segments. *)
module Router : sig
  (** A router instance. *)
  type t

  (** A page render function: [unit -> (unit -> vnode)] — called once on mount, returns the
      reactive render thunk. *)
  type page = unit -> (unit -> vnode)

  (** [make ?base ?not_found ()] — create a router. [base] is the URL prefix stripped from
      paths (useful for apps mounted at a sub-path). *)
  val make : ?base:string -> ?not_found:page -> unit -> t

  (** [page ?name pattern render t] — register a page for a URL pattern. Patterns may contain
      dynamic segments; generated route modules produce these registrations from the file tree. *)
  val page : ?name:string -> string -> page -> t -> t

  (** The router's base path prefix. *)
  val base : t -> string

  (** [relativize base path] — strip [base] from [path]. Pure. *)
  val relativize : string -> string -> string

  (** [absolutize base path] — prepend [base] to [path]. Pure. *)
  val absolutize : string -> string -> string

  (** The currently matched path as a reactive value (signals re-render on navigation). *)
  val current_path : t -> string

  (** [set_path t path] — update the router's current path signal without a browser history push. *)
  val set_path : t -> string -> unit

  (** [build t path params] — build a URL with query params. *)
  val build : t -> string -> (string * string) list -> string

  (** [href t path params] — build a URL string, including query parameters, for links inside
      the app. *)
  val href : t -> string -> (string * string) list -> string

  (** [path t fmt ...] — build an in-app path using a format string. Generated typed path helpers
      call into this so dynamic route links stay compiler-checked. *)
  val path : t -> ('a, unit, string) format -> 'a

  (** [ext fmt ...] — build a raw URL (external or absolute) without the router's base. *)
  val ext : ('a, unit, string) format -> 'a

  (** Programmatic navigation: push to the browser history and re-render. *)
  val navigate : string -> unit

  (** [sync_path path] — record a path change that already happened (e.g. after
      [history.replaceState]) without triggering a navigate side-effect. *)
  val sync_path : string -> unit

  (** Render the currently active page at this router's outlet slot. *)
  val outlet : t -> vnode

  (** Activate this router as the application-level current router (per-request: stored in the render
      context, so concurrent server renders don't share it). *)
  val activate : t -> unit

  (** The currently active router (set via {!activate}). *)
  val current : unit -> t

  (** A per-request copy of [t] for a SERVER render — same routes, but its own path signal + params, so
      concurrent requests to one app never share the current path. The SSR driver activates the clone. *)
  val clone_for_render : t -> t

  (** Look up a named segment from the currently matched route. *)
  val param : string -> string option

  (** [param_or name default] — param with a fallback string. *)
  val param_or : string -> string -> string
end

(** {1 Ambient router helpers} *)

(** [p fmt ...] — build an in-app typed path using the active router's base. *)
val p : ('a, unit, string) format -> 'a

(** [ext fmt ...] — build a raw URL (including external ones) without the active router's base. *)
val ext : ('a, unit, string) format -> 'a

(** [href path params] — build a URL with query params using the active router. *)
val href : string -> (string * string) list -> string

(** Look up a named segment from the currently matched route, using the active router. *)
val param : string -> string option

(** [param_or name default] — param with a fallback, using the active router. *)
val param_or : string -> string -> string

(** Programmatic navigation using the active router. *)
val navigate : string -> unit

(** [sync_path path] — record a path change without a navigate side-effect. *)
val sync_path : string -> unit

(** Render the currently active page at the active router's outlet. *)
val outlet : unit -> vnode

(** {1 Document shell} *)

(** SSR document shell composition: takes a context record (collected by {!Head}, {!Data},
    and the app render) and emits the structural vnodes for a full HTML document. *)
module Doc : sig
  (** The collected SSR context for one render pass. *)
  type ctx = { head : string; data : string; body : string; styles : string; client_js : string }

  (** Emit the [<head>] vnode from the context's pre-rendered head string. *)
  val head : ctx -> vnode

  (** Emit the stylesheet [<link>] tags from the context. *)
  val styles : ctx -> vnode

  (** Emit the fast-render seed [<script>] tag only (no client bundle). *)
  val data : ctx -> vnode

  (** Emit the app body [<div>] (or whatever the document root is). *)
  val outlet : ctx -> vnode

  (** Emit both the fast-render seed and the client bundle [<script>] tags. *)
  val scripts : ctx -> vnode
end

(** {1 Mount} *)

(** An app mounted at a URL base: its root render thunk, its router, and its document shell.
    The code generator emits a list of these for multi-app pages. *)
type mount = {
  base : string;
  root : unit -> (unit -> vnode);
  router : Router.t;
  document : Doc.ctx -> vnode;
}

(** {1 Reconciler} *)

(** Abstract DOM backend for the reconciler. The browser implementation wraps the real DOM
    API; the SSR implementation is inert (rendering uses {!to_html}). Implement this to target
    a custom or test DOM. *)
module type BACKEND = sig
  (** The DOM node type. *)
  type node

  val create_text : string -> node
  val create_element : string -> node
  val get_text : node -> string
  val set_text : node -> string -> unit
  val get_attr : node -> string -> string option
  val set_attr : node -> string -> string -> unit
  val remove_attr : node -> string -> unit
  val set_prop : node -> string -> string -> unit
  val get_prop : node -> string -> string
  val append : node -> node -> unit

  (** [insert_before parent child ref] inserts [child] before [ref] (or appends when [ref] is
      [None]) — the DOM [insertBefore]. Used by keyed reconciliation to move only out-of-position
      nodes instead of re-appending every child. *)
  val insert_before : node -> node -> node option -> unit

  val remove : node -> node -> unit
  val replace : node -> node -> node -> unit
  val parent : node -> node option
  val listen : node -> string -> (unit -> unit) ref -> unit
  val child : node -> int -> node option
  val first_child : node -> node option

  (** The tag name of an element node, LOWERCASED ([Some "div"]); [None] for a non-element such as
      a text or comment node. Used by hydration to detect SSR/CSR drift before adopting a node. *)
  val node_tag : node -> string option

  (** [true] when the node is a text node. Used by hydration to validate a {!text} vnode's adoptee. *)
  val is_text : node -> bool
end

(** The virtual DOM reconciler parameterized over a {!BACKEND}. Diffs the previous vnode
    tree against the new one and applies the minimal set of DOM mutations. *)
module Reconcile : functor (B : BACKEND) -> sig
  (** [mount_root node render] — attach a reactive render loop at [node]. [render] is called
      once; its result is re-evaluated on signal changes and diffed against the previous tree. *)
  val mount_root : B.node -> (unit -> vnode) -> unit

  (** Like {!mount_root}, but returns a disposer. Calling it stops the root render effect (so a
      later signal write never re-renders this tree) and unmounts the whole tree — running every
      mounted component's {!on_cleanup} callbacks and disposing its nested {!watch}/effect
      subscriptions. Idempotent. Use when an embedder owns a sub-tree's lifetime and must tear it
      down deterministically; {!mount_root} is this with the handle dropped. *)
  val mount_root_disposable : B.node -> (unit -> vnode) -> (unit -> unit)
end
