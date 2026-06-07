(** The frozen public contract of the Fur runtime core.

    Abstract types make invalid states unrepresentable; this [.mli] is also a recompile
    firewall — editing the [.ml] body never recompiles dependents. Internals (reaction,
    run_effect, the effect tracker, flatten/escape, take_seed, Head counter, etc.) are hidden. *)

(** {1 Reactivity} *)

(** A reactive value of type ['a]. Reading it inside a {!watch} or {!comp} body automatically
    tracks it as a dependency; writing it re-runs every dependent effect synchronously. *)
type 'a signal

(** [signal ?eq init] — create a signal with initial value [init]. [eq] is the equality
    predicate used to suppress spurious re-runs (default: structural [=]). *)
val signal : ?eq:('a -> 'a -> bool) -> 'a -> 'a signal

(** Read the current signal value WITHOUT registering it as a dependency. Use inside effects
    that need the current value but must not re-run when it changes. *)
val peek : 'a signal -> 'a

(** Read the signal value, registering it as a dependency of the enclosing reactive context
    ({!watch} or {!comp}). Aliased as [!s] in Fur component syntax. *)
val get : 'a signal -> 'a

(** Set the signal to a new value; notifies dependents synchronously. *)
val set : 'a signal -> 'a -> unit

(** [update s f] — set [s] to [f (peek s)]: a read-modify-write that does not track [s]. *)
val update : 'a signal -> ('a -> 'a) -> unit

(** [watch f] runs [f] immediately, tracking every {!get} inside it, then re-runs
    automatically whenever any tracked signal changes. Returns a stop function: call it to
    detach the effect permanently. *)
val watch : (unit -> unit) -> (unit -> unit)

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

(** A fragment: multiple sibling nodes with no wrapper element. *)
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
    as a child of {!h} without explicit conversion. *)
val node : 'a -> vnode

(** [skey v] — coerce a stable key to a string ([int] or [string]). *)
val skey : 'a -> string

(** [each xs f] — [List.map f xs] accepting a mixed-type list via coercion. Cleaner than a
    bare [List.map] in element-construction expressions. *)
val each : 'a list -> ('a -> 'b) -> 'b list

(** Attach a stable reconciler key to an existing vnode (use when [h ~key] is inconvenient,
    e.g. on a component returned by a helper function). *)
val with_key : string -> vnode -> vnode

(** Serialize a vnode tree to HTML markup (SSR). Does NOT prepend a doctype. *)
val to_html : vnode -> string

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

  (** The reactive source list — [(priority, tags)] pairs, one per {!use} call. *)
  val sources : (int * tag list) list signal

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
    synchronous from the seed), and exposes {!Data.refetch} for mutations. *)
module Data : sig
  (** A loaded (or loading) resource of type ['a]. *)
  type 'a t

  (** Per-request seed context: a [key → JSON-string] map filled by the server, read by
      the client on hydration. Mutate via {!put_seed} / {!clear_seed}. *)
  val seed : (string, string) Hashtbl.t

  (** Write a key/value pair into the current request's seed (server-side). *)
  val put_seed : string -> string -> unit

  (** Clear all seed entries (e.g. between requests in a test). *)
  val clear_seed : unit -> unit

  (** The platform/app fetch strategy: a [(key, callback)] function that resolves a
      resource key to a JSON string and calls [callback] with the result. Replaced by
      the app's data layer. *)
  val source : (string -> (string -> unit) -> unit) ref

  (** [resource ~key ?client_only ~fallback ~decode ()] — declare a typed resource.
      [key] identifies it in the seed; [decode] parses the JSON string; [fallback] is the
      value while loading. [~client_only] skips SSR (useful for purely client-local state). *)
  val resource : key:string -> ?client_only:bool -> fallback:'a -> decode:(string -> 'a) -> unit -> 'a t

  (** [string key ?fallback ?client_only ()] — a string resource (no decode needed). *)
  val string : string -> ?fallback:string -> ?client_only:bool -> unit -> string t

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
    via the History API. *)
module Router : sig
  (** A router instance. *)
  type t

  (** A page render function: [unit -> (unit -> vnode)] — called once on mount, returns the
      reactive render thunk. *)
  type page = unit -> (unit -> vnode)

  (** [make ?base ?not_found ()] — create a router. [base] is the URL prefix stripped from
      paths (useful for apps mounted at a sub-path). *)
  val make : ?base:string -> ?not_found:page -> unit -> t

  (** [page ?name pattern render t] — register a page for a URL pattern. *)
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

  (** [href t path params] — build a URL string. *)
  val href : t -> string -> (string * string) list -> string

  (** [path t fmt ...] — build an in-app path using a format string. *)
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

  (** Activate this router as the application-level current router. *)
  val activate : t -> unit

  (** The currently active router (set via {!activate}). *)
  val current : unit -> t

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
  val remove : node -> node -> unit
  val replace : node -> node -> node -> unit
  val parent : node -> node option
  val listen : node -> string -> (unit -> unit) ref -> unit
  val child : node -> int -> node option
  val first_child : node -> node option
end

(** The virtual DOM reconciler parameterized over a {!BACKEND}. Diffs the previous vnode
    tree against the new one and applies the minimal set of DOM mutations. *)
module Reconcile : functor (B : BACKEND) -> sig
  (** [mount_root node render] — attach a reactive render loop at [node]. [render] is called
      once; its result is re-evaluated on signal changes and diffed against the previous tree. *)
  val mount_root : B.node -> (unit -> vnode) -> unit
end
