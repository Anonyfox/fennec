(* Fur — platform-agnostic core: signals + vnode + SSR. The state model is uniform:
   a signal in a component's setup is LOCAL (per-instance); a signal in a shared
   module is GLOBAL. get subscribes, set/update notify. One primitive, scoped by
   where you define it. *)
(* A reaction carries scheduler bookkeeping beyond its [run]/[deps]:
   - [dirty]: already enqueued for the next flush (dedup — a diamond marks it once);
   - [alive]: cleared by [dispose] so a queued-then-disposed reaction is skipped, not run;
   - [epoch]/[runs]: a per-flush execution counter that bounds re-entrancy — a self-writing
     effect re-marks itself but can only re-run [reentry_cap] times within one flush, so it
     terminates instead of stack-overflowing or spinning forever. *)
type reaction =
  { run : unit -> unit; mutable deps : psig list;
    mutable dirty : bool; mutable alive : bool; mutable epoch : int; mutable runs : int }
and 'a signal = { mutable v : 'a; mutable subs : reaction list; eq : 'a -> 'a -> bool }
and psig = P : 'a signal -> psig

let current : reaction option ref = ref None
let mk_reaction run = { run; deps = []; dirty = false; alive = true; epoch = 0; runs = 0 }

(* platform flag: the client entrypoint flips this true; native SSR leaves it false. Defined
   here (above the scheduler) because the flush-timing decision reads it. *)
let is_browser = ref false

(* [eq] decides whether a [set] is a real change (and thus notifies). Defaults to
   structural equality; pass ~eq:(fun _ _ -> false) to always notify, or a custom
   one for values structural-compare can't handle (e.g. closures). *)
let signal ?(eq = ( = )) v = { v; subs = []; eq }
let peek s = s.v
let get s =
  (match !current with
   | Some e -> if not (List.memq e s.subs) then (s.subs <- e :: s.subs; e.deps <- P s :: e.deps)
   | None -> ());
  s.v
let run_effect e =
  List.iter (fun (P s) -> s.subs <- List.filter (fun e' -> e' != e) s.subs) e.deps;
  e.deps <- [];
  let prev = !current in current := Some e;
  Fun.protect ~finally:(fun () -> current := prev) e.run

(* ──── batched effect scheduler ────
   A [set] writes the value SYNCHRONOUSLY (so [get] right after [set] sees the new value), then
   marks each subscriber dirty into a dedup'd FIFO and requests ONE flush — it never re-runs an
   effect inline. So N writes in one turn collapse to a single render pass, and a diamond (two
   signals feeding one effect) re-runs that effect once.

   FLUSH TIMING: inside an open [batch]/[flush_sync] the flush is deferred to the outermost close.
   Otherwise, on the browser it is a microtask (one per turn, via [Platform.schedule]) so a whole
   event handler's writes coalesce; on native (SSR / tests) it runs immediately — keeping the
   long-standing synchronous-after-[set] behavior the unit tests and the one-shot SSR render rely
   on. [Platform.schedule] is itself synchronous on native, so even a test that flips [is_browser]
   stays deterministic. *)
let pending : reaction Queue.t = Queue.create ()
let batch_depth = ref 0
let flush_scheduled = ref false   (* a microtask flush is already queued (browser) *)
let flushing = ref false          (* re-entrancy guard: a flush is in progress *)
let flush_epoch = ref 0
let reentry_cap = 100             (* max re-runs of one reaction within a single flush *)

let mark e =
  if e.alive && not e.dirty then (e.dirty <- true; Queue.add e pending)

let flush () =
  if not !flushing then begin
    flushing := true;
    incr flush_epoch;
    let ep = !flush_epoch in
    Fun.protect ~finally:(fun () -> flushing := false; flush_scheduled := false) (fun () ->
      while not (Queue.is_empty pending) do
        let e = Queue.pop pending in
        e.dirty <- false;
        if e.alive then begin
          if e.epoch <> ep then (e.epoch <- ep; e.runs <- 0);
          e.runs <- e.runs + 1;
          (* a self-writing effect re-marks itself; stop re-running it past the cap so the
             flush terminates (bounded), rather than spinning or overflowing the stack *)
          if e.runs <= reentry_cap then run_effect e
        end
      done)
  end

let request_flush () =
  if !batch_depth > 0 then ()                 (* deferred to the batch close *)
  else if !is_browser then
    (if not !flush_scheduled then (flush_scheduled := true; Platform.schedule flush))
  else flush ()                               (* native: synchronous *)

let set s v = if not (s.eq v s.v) then (s.v <- v; List.iter mark s.subs; request_flush ())
let update s f = set s (f (peek s))

(* Run [f] with writes batched: every [set] inside defers its flush to the outermost close,
   so a transaction of N writes renders once. The flush at close is SYNCHRONOUS (React
   flushSync-style) — when [f] returns, every dependent effect has already re-run and the DOM
   reflects the final state. Nestable (depth-counted). [batch] is the alias for grouping
   without the synchronous-flush emphasis; both share one body. *)
let flush_sync f =
  incr batch_depth;
  Fun.protect ~finally:(fun () -> decr batch_depth; if !batch_depth = 0 then flush ()) f
let batch f = flush_sync f

let dispose e =  (* unmount: unsubscribe from everything so it never re-runs *)
  e.alive <- false; e.dirty <- false;
  List.iter (fun (P s) -> s.subs <- List.filter (fun e' -> e' != e) s.subs) e.deps;
  e.deps <- []

(* IMPORTANT — per-request isolation (the concurrency seam).
   Several pieces below keep PER-RENDER state in MODULE GLOBALS: Head.sources, the
   Data.seed table + Data.source hook, and each Router's `current` signal. On the
   client that's correct (one document, one app, single-threaded). In the one-shot
   SSR binary it's also fine (one render per process). But fennec's REAL server is
   concurrent (Eio fibers), and these globals would race/bleed across simultaneous
   requests. Before this meets the real server, give each request its own context —
   the clean fix is Eio fiber-local storage (Eio.Fiber.with_binding / a per-request
   record threaded by the handler), NOT locks. This changes no public API: resource/
   Head.use/Router stay identical; only WHERE their backing store lives changes.
   Every such global below is tagged `IMPORTANT: per-request state`. *)

(* Effect scope: cleanups registered during a component's setup/first render are
   tied to THAT instance and run on its unmount. The DOM runtime points
   [current_cleanups] at the mounting instance's accumulator (save/restore per
   instance, so nested children scope correctly). Used by Head.use to remove its
   head contribution on unmount, by subscriptions, etc. A no-op container on the
   server (SSR never unmounts). *)
let current_cleanups : (unit -> unit) list ref ref = ref (ref [])
let on_cleanup f = let r = !current_cleanups in r := f :: !r

(* on_mount: a browser-only side effect (à la Vue's onMounted / React useEffect[]).
   Registered during setup, run once AFTER the initial client render adopts the SSR
   DOM. A no-op on the server, so SSR never executes browser-only handlers.

   OWNER SCOPE: each queued callback captures the cleanups ref of the component that
   registered it (= [!current_cleanups] at registration, which [instantiate] has
   pointed at the owning instance). [flush_mounts] rebinds [current_cleanups] to that
   ref while running the callback, so a [watch]/subscription started inside on_mount —
   the documented place to subscribe — registers its disposer on the OWNING component
   and is torn down on its unmount. Without the pairing the disposer would land on
   whatever scope happened to be current at flush time (the root) and leak for the
   page's life; over list/row churn that is unbounded. *)
let mount_queue : ((unit -> unit) list ref * (unit -> unit)) list ref = ref []
let on_mount f = if !is_browser then mount_queue := (!current_cleanups, f) :: !mount_queue
let flush_mounts () =
  let q = List.rev !mount_queue in
  mount_queue := [];
  let saved = !current_cleanups in
  List.iter (fun (owner, f) ->
      current_cleanups := owner;
      Fun.protect ~finally:(fun () -> current_cleanups := saved) f)
    q

(* reactive side-effect (à la Solid createEffect / MobX autorun): runs now, re-runs
   when a signal it read changes, auto-disposed on the owning component's unmount.
   Returns a stop handle. (Named [watch] because [effect] is an OCaml 5 keyword.) *)
let watch f =
  let e = mk_reaction f in
  run_effect e;
  on_cleanup (fun () -> dispose e);
  fun () -> dispose e

(* ──── memo: a cached, derived signal ────
   A memo is a plain {!signal} whose value tracks a pure computation [f] over other signals.
   Because it returns a [signal], reads go through the ordinary {!get}/{!peek} — there is no
   custom getter — so the value must already be fresh when read; the memo refreshes eagerly within
   the effect flush, but each refresh costs at most one [f] per flush.

   One internal tracker reaction runs [f] under dependency tracking and pushes the result into the
   value signal [out] with the [eq] short-circuit (so downstream readers re-run only on a real
   change). A dependency change enqueues the tracker; the flush runs it ONCE — deduped by the
   reaction's dirty bit, so a diamond of deps recomputes [f] once — and BEFORE any dependent reader
   that was enqueued after it (glitch-free). Reading the memo many times between changes is free: a
   bare [get out], no recomputation. The tracker is seeded once at construction and, like {!watch},
   auto-disposed on the owning component's unmount. *)
let memo (type a) ?(eq = (( = ) : a -> a -> bool)) (f : unit -> a) : a signal =
  let out : a signal = { v = Obj.magic (); subs = []; eq } in
  let seeded = ref false in
  let tracker =
    mk_reaction (fun () ->
        let v = f () in
        (* first computation writes [out] directly (no spurious notify, no eq on the junk seed);
           later recomputes go through [set] so observers are marked + the eq guard applies *)
        if not !seeded then (out.v <- v; seeded := true) else set out v)
  in
  run_effect tracker;                          (* seed: compute now, track deps from the start *)
  on_cleanup (fun () -> dispose tracker);
  out

(* ---- ambient current event ----
   Handlers stay [unit -> unit]; these accessors read the event being dispatched via
   the linked Platform (browser reads the live js_of_ocaml event; native returns safe
   defaults). During SSR no handler runs, so event code can't touch the server render. *)
let target_value = Platform.event_value      (* an input/textarea/select's current value *)
let target_checked = Platform.event_checked  (* a checkbox/radio's checked state *)
let key = Platform.event_key                  (* a keyboard event's key, e.g. "Enter" *)
let prevent_default = Platform.event_prevent_default

(* ---- Browser: SSR-safe facade resolved by the linked Platform. Native = no-op/None;
   browser = js_of_ocaml. Safe anywhere; meaningful only in client contexts. *)
module Browser = struct
  let local_get = Platform.local_get
  let local_set = Platform.local_set
  let local_remove = Platform.local_remove
end

type attr = Attr of string * string | Handler of string * (unit -> unit)
type vnode =
  | Text of string
  | Raw of string  (* verbatim markup — server-only escape hatch (templates, head injection) *)
  | Elem of { tag : string; key : string option; attrs : attr list; children : vnode list }
  | Fragment of vnode list
  | Comp of comp
and comp = { cid : string; ckey : string option; setup : unit -> (unit -> vnode) }

let text s = Text s
let raw s = Raw s
let frag l = Fragment l
let h ?key tag attrs children = Elem { tag; key; attrs; children }
let comp ~cid ?key setup = Comp { cid; ckey = key; setup }
let on ev f = Handler (ev, f)
let attr k v = Attr (k, v)
let class_ v = Attr ("class", v)

(* ---- THE sanctioned unsafe primitive ----
   JSX child/key slots accept int | float | string | vnode in the same position —
   ad-hoc polymorphism OCaml can't express in the type system. This is the ONE place
   we use Obj, and it is TOTAL over that contract:
     - is_int  -> a literal int     -> Text (string_of_int …)
     - string  -> a string          -> Text …
     - double  -> a float           -> Text (string_of_float …)
     - else    -> ALREADY a vnode   -> returned as-is
   The ppx only emits [node]/[skey] in child/key position and never wraps a value it
   knows is already a vnode (an Html element, a component .make, or an Iso builder),
   so the else branch is reached only for genuine vnodes. Any other type in a child
   slot is a usage error. *)
let node (x : 'a) : vnode =
  let r = Obj.repr x in
  if Obj.is_int r then Text (string_of_int (Obj.magic x))
  else if Obj.tag r = Obj.string_tag then Text (Obj.magic x)
  else if Obj.tag r = Obj.double_tag then Text (string_of_float (Obj.magic x))
  else (Obj.magic x : vnode)
(* key coercion (same contract, int|string) so key=t.id works as well as key="x" *)
let skey (x : 'a) : string =
  let r = Obj.repr x in
  if Obj.is_int r then string_of_int (Obj.magic x) else (Obj.magic x : string)

(* JS-like list rendering order: each items (fun x -> …)  ==  items.map(x => …) *)
let each l f = List.map f l
let with_key k = function
  | Elem { tag; attrs; children; _ } -> Elem { tag; key = Some k; attrs; children }
  | Comp c -> Comp { c with ckey = Some k }
  | v -> v
(* Flatten fragments AND coalesce adjacent text into one Text node. SSR serializes
   consecutive text (e.g. "iso — " ^ count ^ " todos") into a SINGLE DOM text node,
   so the client must present the same single child or hydration adoption desyncs. *)
let rec flatten l =
  let expanded = List.concat_map (function Fragment xs -> flatten xs | v -> [v]) l in
  let rec coalesce = function
    | Text a :: Text b :: rest -> coalesce (Text (a ^ b) :: rest)
    | x :: rest -> x :: coalesce rest
    | [] -> []
  in
  coalesce expanded

let escape s =
  let b = Buffer.create (String.length s) in
  String.iter (function
    | '<' -> Buffer.add_string b "&lt;" | '>' -> Buffer.add_string b "&gt;"
    | '&' -> Buffer.add_string b "&amp;" | '"' -> Buffer.add_string b "&quot;"
    | c -> Buffer.add_char b c) s;
  Buffer.contents b
let is_void = function "input"|"br"|"img"|"hr"|"meta"|"link" -> true | _ -> false

(* ONE serializer. [style] is a per-element hook returning extra inline-style declarations to merge into
   the element — the seam the email inliner uses to flatten a stylesheet into [style=] attributes. The
   stylesheet decls go FIRST, so an explicit inline [style] (appended after) still wins per the cascade.
   Plain [to_html] passes a constant-[None] hook, so it serializes exactly as before. *)
let merge_style attrs extra =
  match extra with
  | None | Some "" -> attrs
  | Some s ->
    if List.exists (function Attr ("style", _) -> true | _ -> false) attrs then
      List.map (function Attr ("style", e) -> Attr ("style", s ^ ";" ^ e) | a -> a) attrs
    else Attr ("style", s) :: attrs

let html_with (style : tag:string -> attrs:(string * string) list -> string option) v =
  let rec go = function
    | Text s -> escape s
    | Raw s -> s
    | Fragment l -> String.concat "" (List.map go l)
    | Comp c -> go ((c.setup ()) ())   (* SSR: run setup + render once, no reactivity *)
    | Elem { tag; attrs; children; _ } ->
      let plain = List.filter_map (function Attr (k, v) -> Some (k, v) | Handler _ -> None) attrs in
      let attrs = merge_style attrs (style ~tag ~attrs:plain) in
      let a = List.filter_map (function
        | Attr (k,v) -> Some (Printf.sprintf " %s=\"%s\"" k (escape v)) | Handler _ -> None) attrs
        |> String.concat "" in
      if is_void tag then Printf.sprintf "<%s%s/>" tag a
      else Printf.sprintf "<%s%s>%s</%s>" tag a (String.concat "" (List.map go (flatten children))) tag
  in
  go v

let to_html v = html_with (fun ~tag:_ ~attrs:_ -> None) v
let to_html_styled ~style v = html_with style v

(* A full HTML document: the only thing to_html can't express is the doctype. A
   server-only template is just a vnode rooted at <html>; this renders it. *)
let document v = "<!doctype html>" ^ to_html v

(* ---- Head: data-driven, reactive head management (à la Vue's @unhead) ----

   Any component registers a contribution in its SETUP via [Head.use (fun () -> [...])].
   The closure is a reactive effect, so reading a signal inside it makes that head
   entry DYNAMIC. Contributions register in depth-first tree order (a parent's setup
   runs before its children's), and [resolve] keeps the LAST occurrence per key — so
   a deeper/later component overrides a shallower one ("deepest wins").

   Rehydration safety: this is isomorphic code (identical on server + client). SSR
   emits each resolved tag with data-fh="<content-key>"; the client reconciles
   document.head keyed by that same key. Same inputs -> same resolve -> the client's
   first pass is a no-op. Defaults therefore belong in the app tree (e.g. App's
   setup), NOT baked server-only into the template, or the two sides would disagree. *)
module Head = struct
  type tag =
    | Title of string
    | Meta of (string * string) list   (* attribute pairs, e.g. ["name","description"; "content",c] *)
    | Link of (string * string) list
    | Script of (string * string) list * string  (* attrs * inline body ("" if external) *)
    | Json_ld of string                (* raw JSON for <script type="application/ld+json"> *)

  (* typed tag builders — for dynamic batches via [use] (read like markup, stay data) *)
  module Tag = struct
    let title s = Title s
    let meta ~name content = Meta [ ("name", name); ("content", content) ]
    let og property content = Meta [ ("property", property); ("content", content) ]
    let link ~rel ?(attrs = []) href = Link (("rel", rel) :: ("href", href) :: attrs)
    let script ?(attrs = []) ?(body = "") () = Script (attrs, body)
    let json_ld j = Json_ld j
  end

  (* the registry: ordered (source-id, tags) (a later source overrides an earlier) + the slot-id
     allocator. PER-REQUEST: fiber-local on the concurrent native server (via the Platform's render
     context — the SAME one Data uses, so one binding isolates both), a single global on the browser
     (one document). Lazily created on first use; the Obj cast is sound + contained here (only Head
     ever stores/reads this slot, always as a [ctx]). This is what stops one request's <title> from
     leaking into another's on the parallel server. *)
  type ctx = { sources : (int * tag list) list signal; counter : int ref }

  let ctx () : ctx =
    match Platform.slot_get "head" with
    | Some o -> (Obj.obj o : ctx)
    | None ->
      let c = { sources = signal []; counter = ref 0 } in
      Platform.slot_set "head" (Obj.repr c);
      c

  (* the reactive registry for THIS request (server) / document (browser) — the client head reconciler
     subscribes to it. On the browser it is the one stable global signal; on the server, per-request. *)
  let sources () : (int * tag list) list signal = (ctx ()).sources

  (* Register a reactive contribution. Call ONCE per instance, in setup (it allocates
     a stable slot id). The effect recomputes [f] whenever a signal it read changes. *)
  let use (f : unit -> tag list) : unit =
    let { sources; counter } = ctx () in
    let id = !counter in
    incr counter;
    let eff =
      mk_reaction (fun () ->
          let tags = f () in
          let cur = peek sources in
          set sources
            (if List.mem_assoc id cur
             then List.map (fun (i, t) -> if i = id then (i, tags) else (i, t)) cur
             else cur @ [ (id, tags) ]))
    in
    run_effect eff;
    (* on unmount: stop reacting AND drop this slot so its tags disappear *)
    on_cleanup (fun () ->
      dispose eff;
      set sources (List.filter (fun (i, _) -> i <> id) (peek sources)))

  (* one-liner registrants for the common (static) case — Head.title "x" instead of
     Head.use (fun () -> [Head.Tag.title "x"]). Use [use] for dynamic/multi-tag. *)
  let one t = use (fun () -> [ t ])
  let title s = one (Tag.title s)
  let description s = one (Tag.meta ~name:"description" s)
  let meta ~name v = one (Tag.meta ~name v)
  let og property v = one (Tag.og property v)
  let link ~rel ?attrs href = one (Tag.link ~rel ?attrs href)
  let json_ld j = one (Tag.json_ld j)

  (* the content-key that identifies a tag for dedupe + DOM reconciliation *)
  let tag_key = function
    | Title _ -> "title"
    | Meta a -> "meta:" ^ (match List.assoc_opt "name" a with
        | Some n -> n
        | None -> (match List.assoc_opt "property" a with
            | Some p -> p | None -> Digest.to_hex (Digest.string (String.concat "|" (List.map (fun (k,v) -> k ^ "=" ^ v) a)))))
    | Link a -> "link:" ^ Option.value ~default:"" (List.assoc_opt "rel" a) ^ ":" ^ Option.value ~default:"" (List.assoc_opt "href" a)
    | Script (a, b) -> "script:" ^ (match List.assoc_opt "src" a with Some s -> s | None -> Digest.to_hex (Digest.string b))
    | Json_ld j -> "jsonld:" ^ Digest.to_hex (Digest.string j)

  (* flatten all contributions in order, then keep the LAST tag per key *)
  let resolve srcs =
    let all = List.concat_map snd srcs in
    let rec dedupe seen acc = function
      | [] -> acc
      | t :: rest ->
        let k = tag_key t in
        if List.mem k seen then dedupe seen acc rest else dedupe (k :: seen) (t :: acc) rest
    in
    dedupe [] [] (List.rev all)  (* reversed: last occurrence wins, result restored to order *)

  let attrs_str a = String.concat "" (List.map (fun (k, v) -> Printf.sprintf " %s=\"%s\"" k (escape v)) a)

  (* server render: a string of resolved head tags, each marked with its key *)
  let to_ssr () =
    resolve (peek (ctx ()).sources)
    |> List.map (fun t ->
        let k = tag_key t in
        match t with
        | Title s -> Printf.sprintf "<title data-fh=\"%s\">%s</title>" k (escape s)
        | Meta a -> Printf.sprintf "<meta data-fh=\"%s\"%s>" k (attrs_str a)
        | Link a -> Printf.sprintf "<link data-fh=\"%s\"%s>" k (attrs_str a)
        | Script (a, b) -> Printf.sprintf "<script data-fh=\"%s\"%s>%s</script>" k (attrs_str a) b
        | Json_ld j -> Printf.sprintf "<script data-fh=\"%s\" type=\"application/ld+json\">%s</script>" k j)
    |> String.concat ""
end

(* ---- Data: isomorphic, reactive resources (à la SolidJS createResource) ----

   A resource is a SIGNAL of state — never awaited mid-render. The component reads
   it reactively (with a fallback while loading), and the async result's only job is
   to [set] the signal. That single idea dissolves the Eio<->Promise mismatch: each
   platform resolves into a [set], nothing blocks the render.

   The same keyed table serves SSR-embed and client-seed (Meteor fast-render):
   - SERVER: the driver fills [seed] (= the request's data context) by actually
     running fetches in Eio fibers, then serializes it into the page.
   - CLIENT: [seed] is loaded from window.__FUR_DATA__. A resource whose key is
     present resolves SYNCHRONOUSLY (no fetch, no loading flash, hydration matches),
     and the entry is consumed so later/dynamic fetches hit the network for real.

   The [source] hook is the only platform split (the SOURCE functor, as a ref): on
   the server it forks an Eio fiber; on the client it does a real fetch. *)
module Data = struct
  type 'a state = Loading | Ready of 'a | Failed of string
  type 'a t = { st : 'a state signal; key : string; decode : string -> 'a; fallback : 'a }

  (* key -> raw payload string. Client: seeded from __FUR_DATA__, consumed once.
     Server: the per-request data context the driver fills + serializes. *)
  (* IMPORTANT: per-request state — [seed] AND [source] below are the request's data
     context + fetch strategy; both must be fiber-local on the concurrent server (two
     requests fetching different data would otherwise share one table). *)
  (* The per-request seed table + fetch source live in the platform: FIBER-LOCAL on the concurrent
     native server (so simultaneous SSR requests never share a table), a single global on the browser
     / outside an Eio run. [with_context] establishes a fresh per-request context (the SSR driver
     wraps each concurrent render in it). This keeps the resource/value/refetch API identical. *)
  let seed_table () = Platform.seed_table ()
  let with_context f = Platform.with_data_context f
  let put_seed k v = Hashtbl.replace (seed_table ()) k v
  let take_seed k =
    let s = seed_table () in
    match Hashtbl.find_opt s k with
    | None -> None
    | Some v -> if !is_browser then Hashtbl.remove s k; Some v  (* client consumes; server keeps for pass 2 + embed *)
  let clear_seed () = Hashtbl.clear (seed_table ())

  (* platform SOURCE: deliver a key's raw payload to a continuation. Default no-op
     (overridden by the server driver / the client fetch binding via [set_source]). *)
  let set_source f = Platform.set_data_source f

  let resource ~key ?(client_only = false) ~fallback ~decode () =
    let initial, fetch_now =
      match take_seed key with
      | Some json -> (Ready (decode json), false)              (* seed/ctx hit: synchronous *)
      | None -> (Loading, not (client_only && not !is_browser)) (* server skips browser-only data *)
    in
    let st = signal initial in
    if fetch_now then (Platform.data_source ()) key (fun json -> set st (Ready (decode json)));
    { st; key; decode; fallback }

  (* common case: string payload (no decoder). [Data.string "/api/x" ~fallback:"…" ()] *)
  let string key ?(fallback = "") ?(client_only = false) () =
    resource ~key ~client_only ~fallback ~decode:Fun.id ()

  (* TYPED resource over a Sift codec: [Data.model codec "/api/x" ~fallback ()] decodes the seeded /
     fetched JSON with [Sift.decode_json codec] (the relaxed-JSON decode + the model's full validation),
     SSR-seeded + refetchable like {!string} but yielding the typed ['a]. A malformed / invalid payload
     falls back to [fallback] (the resource never crashes the UI on foreign garbage) — the same skip
     posture {!Live.find_c} takes. The codec is the SAME ['a Sift.t] the server's [Sift.encode_json]
     produced, so wire shape = model shape with no second serializer. *)
  let model codec key ?(client_only = false) ~fallback () =
    resource ~key ~client_only ~fallback
      ~decode:(fun json -> match Sift.decode_json codec json with Ok v -> v | Error _ -> fallback) ()

  (* ── CO-LOCATED resources: the SERVER fetcher declared INLINE in the component ─────────────────
     A {!local} / {!model_local} resource carries its own server data-source: a {!Server_only.fn}
     thunk that produces the value. Declaring one at component module-init (server side) REGISTERS that
     fetcher in a process table keyed by the resource's path — the single source of truth the framework
     drains to fuse BOTH halves of the old three-place split: the SSR seed source (consulted between
     render passes) AND the HTTP refetch route (mounted on every endpoint). The app author wires
     nothing — exactly as [Pulse.publish] fuses the live publication + the SSR seed in one call.

     The fetcher is leak-proof by CONSTRUCTION + by STRIPPING:
     - {!Server_only.fn} has no {!Sift}, so its thunk can never be seeded (only the produced string is);
     - on the client (jsoo) the fur ppx STRIPS the thunk body (the same posture as a handler's [load]),
       so no server logic — and no secret inside it — ever reaches the bundle. There [local]/[model_local]
       degrade to a plain seeded + refetchable {!string}/{!model} reading the path. *)
  module Server_only = struct
    (* a server-only fetcher: a thunk with NO Sift, so it cannot cross into a seed/payload. The ppx
       replaces the body with [client_inert] in the client build, so the real body never links into jsoo. *)
    type 'a t = Fetch of (unit -> 'a)

    let fn (f : unit -> 'a) : 'a t = Fetch f
    let run (Fetch f : 'a t) : 'a = f ()
  end

  (* one registered co-located source: how to produce its bytes + whether they are JSON (so the
     auto-mounted route picks the content-type — application/json for {!model_local}, text/plain for the
     string {!local}). [produce] runs the inline {!Server_only.fn} fetcher; the SSR seed source and the
     refetch route both call it. *)
  type local_src = { produce : unit -> string; json : bool }

  (* the process registry: path -> source. Server-only in effect (the producer closes over the
     {!Server_only.fn} thunk); on the client every [local]/[model_local] lowers to a stripped
     [string]/[model], so nothing is registered there. A [Hashtbl] keyed by path is idempotent under a
     re-declared key (last registration wins — module init runs once per process anyway). *)
  let _local_sources : (string, local_src) Hashtbl.t = Hashtbl.create 16

  (* derive the served path from a bare name: ["greeting"] -> ["/api/greeting"]. An explicit [~path]
     (already absolute) is used verbatim, so a resource can sit anywhere. A name already starting with
     ["/"] is taken as the path too (so [local "/api/x"] keeps working like the old stringly key). *)
  let local_path ?path name =
    match path with
    | Some p -> p
    | None -> if String.length name > 0 && name.[0] = '/' then name else "/api/" ^ name

  (* register a co-located source under [path] (server side). Idempotent per path. *)
  let register_local ~path ~json (produce : unit -> string) =
    Hashtbl.replace _local_sources path { produce; json }

  (* the framework drains these at boot ({!Fennec.serve} / the SSR driver): each entry is one
     auto-mounted refetch route AND one SSR seed source, both keyed by the path. *)
  let local_sources () = Hashtbl.fold (fun path src acc -> (path, src) :: acc) _local_sources []

  (* look up a source by path (the SSR driver's in-process source consults this first). *)
  let local_source path = Hashtbl.find_opt _local_sources path

  (* accessors over an opaque {!local_src} (the registry value), for the framework's route mounter *)
  let src_produce (s : local_src) = s.produce ()
  let src_is_json (s : local_src) = s.json

  (* [local name ?path ~fallback fetch] — a STRING resource whose SERVER fetcher [fetch] is declared
     INLINE. The path is [~path] or ["/api/" ^ name]; the resource reads the SSR seed + refetches that
     path, exactly like {!string}. Registering [fetch] is what wires the seed source + the refetch route,
     so server.ml needs no [api_source] arm and no [Paw.get]. On the client the ppx strips [fetch]. *)
  let local name ?path ~(fallback : string) (fetch : string Server_only.t) : string t =
    let path = local_path ?path name in
    register_local ~path ~json:false (fun () -> Server_only.run fetch);
    string path ~fallback ()

  (* [model_local codec name ?path ~fallback fetch] — the TYPED twin of {!local}: the inline server
     [fetch] yields an ['a], encoded to the seed with [Sift.encode_json codec] and decoded back on read
     with [Sift.decode_json codec] — typed end to end, no stringly key, no manual decode. The same codec
     drives the registered SSR seed AND the mounted route, so wire shape = model shape on both surfaces. *)
  let model_local codec name ?path ~(fallback : 'a) (fetch : 'a Server_only.t) : 'a t =
    let path = local_path ?path name in
    register_local ~path ~json:true (fun () -> Sift.encode_json codec (Server_only.run fetch));
    model codec path ~fallback ()

  (* reactive readers (each subscribes via get) *)
  let status r = get r.st
  let value r = match get r.st with Ready v -> v | _ -> r.fallback  (* fallback until ready *)
  let loading r = match get r.st with Loading -> true | _ -> false
  let error r = match get r.st with Failed e -> Some e | _ -> None
  (* an explicit, dynamic refetch always hits the network (bypasses the seed) *)
  let refetch r = set r.st Loading; (Platform.data_source ()) r.key (fun json -> set r.st (Ready (r.decode json)))

  (* serialize the data context to a <script>-safe JS assignment *)
  let js_string s =
    let b = Buffer.create (String.length s + 2) in
    Buffer.add_char b '"';
    String.iter (fun c -> match c with
      | '"' -> Buffer.add_string b "\\\"" | '\\' -> Buffer.add_string b "\\\\"
      | '\n' -> Buffer.add_string b "\\n" | '\r' -> Buffer.add_string b "\\r"
      | '<' -> Buffer.add_string b "\\u003c"  (* never let a value close the <script> *)
      | c -> Buffer.add_char b c) s;
    Buffer.add_char b '"';
    Buffer.contents b
  let to_script () =
    let pairs = Hashtbl.fold (fun k v acc -> (js_string k ^ ":" ^ js_string v) :: acc) (seed_table ()) [] in
    "window.__FUR_DATA__={" ^ String.concat "," pairs ^ "}"
end

(* ---- Matcher: pure path-pattern matching (reused verbatim from fennec) ----
   Stdlib only, identical on server + client. Patterns: "/", "/about",
   "/users/:id" (named param), "/files/*" (greedy tail). *)
module Matcher = struct
  type params = (string * string) list
  let segments (p : string) : string list =
    String.split_on_char '/' p |> List.filter (fun s -> s <> "")
  let match_one ~(pattern : string) (path : string) : params option =
    let ps = segments pattern and xs = segments path in
    let rec go ps xs acc =
      match (ps, xs) with
      | [], [] -> Some (List.rev acc)
      | [ "*" ], rest -> Some (List.rev (("*", String.concat "/" rest) :: acc))
      | pseg :: ptl, xseg :: xtl ->
        if String.length pseg > 0 && pseg.[0] = ':' then
          go ptl xtl ((String.sub pseg 1 (String.length pseg - 1), xseg) :: acc)
        else if pseg = xseg then go ptl xtl acc
        else None
      | _ -> None
    in
    go ps xs []
  let find (routes : (string * 'a) list) (path : string) : ('a * params) option =
    let rec go = function
      | [] -> None
      | (pattern, v) :: rest -> (
        match match_one ~pattern path with Some p -> Some (v, p) | None -> go rest)
    in
    go routes
  let param (params : params) name = List.assoc_opt name params
end

(* ---- Router: base-aware, reactive, isomorphic ----

   An app is mounted at a BASE prefix (""/"/admin"/"/shop"). It declares routes
   RELATIVE to that base ("/products/:id"), so it's location-transparent: the same
   app works at any base. The base is injected once, never baked into patterns.

   - Server: the dispatcher strips the base, sets [current] to the relative path,
     renders the outlet. (One mount table; longest base prefix wins.)
   - Client: [set_path] relativizes window.location; the outlet re-renders on the
     [current] signal; [navigate]/click-interception pushState within scope.
   - [href]/[build] do REVERSE routing (named route + params -> URL), so links are
     derived from the route table, not fragile hardcoded strings. *)
module Router = struct
  open Matcher
  (* a page is just a component (unit -> render). Its route params come from the
     ambient `param` accessor, so pages and components have the SAME shape. *)
  type page = unit -> (unit -> vnode)
  type route = { pattern : string; name : string; page : page }
  type t = { base : string; mutable routes : route list; not_found : page option;
             current : string signal; mutable cur_params : params }

  (* IMPORTANT: per-request state — `current`/`cur_params` (the active path + its
     params) and `active` below are per-render. A module-global router instance is
     fine on the client (one document) but on the concurrent server two requests
     would fight over them; make them fiber-local (resolve the router from the
     request context) before going live. *)
  let make ?(base = "") ?not_found () =
    { base; routes = []; not_found; current = signal "/"; cur_params = [] }

  (* the ACTIVE app for THIS render — lets pages/components reach the router (for p, param, href) without
     importing it, so there's no instance/registration cycle. PER-REQUEST: stored in the fiber-local
     render context (the browser keeps the single global, one document), so concurrent server renders
     never fight over it. The server activates a per-request CLONE (see [clone_for_render]) so the path
     signal is per-request too. The Obj cast is sound + contained here (only Router stores/reads "router"). *)
  let activate (t : t) = Platform.slot_set "router" (Obj.repr t)

  let current () : t =
    match Platform.slot_get "router" with
    | Some o -> (Obj.obj o : t)
    | None -> failwith "Router: no active app (call activate)"

  (* a per-request copy of the app router for a SERVER render: same routes, but its OWN path signal +
     params, so two concurrent requests to the same app never share (and corrupt) the current path. *)
  let clone_for_render (t : t) : t = { t with current = signal "/"; cur_params = [] }
  let page ?name pattern p t =
    t.routes <- t.routes @ [ { pattern; name = (match name with Some n -> n | None -> pattern); page = p } ];
    t

  let relativize base abs =
    if base = "" || base = "/" then abs
    else if abs = base then "/"
    else if String.length abs > String.length base && String.sub abs 0 (String.length base) = base
    then (let r = String.sub abs (String.length base) (String.length abs - String.length base) in if r = "" then "/" else r)
    else abs
  let absolutize base rel =
    if base = "" || base = "/" then rel else if rel = "/" then base else base ^ rel

  let base t = t.base                          (* the app's mount prefix *)
  let current_path t = get t.current          (* reactive: relative path *)
  let set_path t abs = set t.current (relativize t.base abs)

  (* reverse routing: build the RELATIVE path for a named route + params *)
  let build t name (args : (string * string) list) =
    match List.find_opt (fun r -> r.name = name) t.routes with
    | None -> failwith ("router: unknown route " ^ name)
    | Some r ->
      "/" ^ (Matcher.segments r.pattern
             |> List.map (fun seg ->
                 if String.length seg > 0 && seg.[0] = ':' then
                   let k = String.sub seg 1 (String.length seg - 1) in
                   (match List.assoc_opt k args with Some v -> v | None -> failwith ("router: missing param " ^ k))
                 else if seg = "*" then Option.value ~default:"" (List.assoc_opt "*" args)
                 else seg)
             |> String.concat "/")
  (* absolute (base-prefixed) href for a named route *)
  let href t name args = absolutize t.base (build t name args)

  (* Typed, base-aware path building — the Phoenix [~p"/users/#{id}"] flavour, but
     leaning on OCaml's already-typed format strings (the %d/%s IS the type).
     [path] is for IN-APP links: typesafe holes, the base is auto-prefixed, and the
     built path is dev-checked against the route table (a typo fails fast — "knows
     its allowed paths"). [ext] is the OUTER-REACH escape hatch: a raw url with no
     base and no check, for linking to other apps / external sites. *)
  let path t fmt =
    Printf.ksprintf
      (fun rel ->
        (match Matcher.find (List.map (fun r -> (r.pattern, ())) t.routes) rel with
         | Some _ -> ()
         | None -> failwith (Printf.sprintf "router: path %S matches no route (use Router.ext for outer reach)" rel));
        absolutize t.base rel)
      fmt
  let ext fmt = Printf.sprintf fmt

  (* navigation: push history via the linked platform, sync the path, run mounts.
     [sync_path] is the popstate path (already navigated; no push). No runtime hook.
     The path-set is wrapped in [flush_sync] so the outlet re-renders SYNCHRONOUSLY and the new
     page's components mount (queuing their [on_mount]s) BEFORE [flush_mounts] runs them — under
     the batched scheduler a bare [set] would defer the re-render to a microtask, leaving the new
     mounts unflushed. *)
  let sync_path abs = flush_sync (fun () -> set_path (current ()) abs); flush_mounts ()
  let navigate abs = Platform.push_state abs; sync_path abs

  (* the routed outlet: a component that reactively renders the matched page,
     keyed by the relative path so a path change swaps the page instance *)
  let outlet t : vnode =
    comp ~cid:"__router_outlet" (fun () -> fun () ->
      let rel = get t.current in
      match Matcher.find (List.map (fun r -> (r.pattern, r)) t.routes) rel with
      | Some (r, params) -> t.cur_params <- params; comp ~cid:rel r.page
      | None -> t.cur_params <- [ ("*", rel) ];
        (match t.not_found with
         | Some pg -> comp ~cid:("__nf:" ^ rel) pg
         | None -> text ""))

  (* ambient param access for the active app's current route (page reads `param "id"`
     instead of receiving + destructuring a params assoc) *)
  let param name = Matcher.param (current ()).cur_params name
  let param_or name d = match param name with Some v -> v | None -> d
end

(* ---- bare, ambient userland helpers (with -open Iso these need no prefix) ---- *)

(* typed in-app path (base auto-prefixed, route-checked); ext = outer reach *)
let p fmt = Router.path (Router.current ()) fmt
let ext fmt = Router.ext fmt
let href name args = Router.href (Router.current ()) name args
(* current route params of the active app *)
let param = Router.param
let param_or = Router.param_or
(* navigate the active app (client intercepts; pushState + re-render) *)
let navigate = Router.navigate
let sync_path = Router.sync_path
(* the routed outlet of the active app — place (outlet ()) in a layout *)
let outlet () = Router.outlet (Router.current ())

(* int-signal arithmetic sugar: count += 1 / count -= 1 *)
let ( += ) s n = update s (fun x -> x + n)
let ( -= ) s n = update s (fun x -> x - n)

(* ---- Doc: document-shell slots, so a template places (Doc.head ctx) etc. instead
   of threading raw head/data/body/script strings by hand. The SSR driver fills ctx. *)
module Doc = struct
  type ctx = { head : string; data : string; body : string; styles : string; client_js : string }
  let head c = raw c.head                                   (* resolved <head> metadata *)
  let styles c = raw c.styles                               (* collected CSS *)
  let data c = raw (Printf.sprintf "<script>%s</script>" c.data)  (* fast-render seed only *)
  let outlet c = raw c.body                                 (* the SSR'd app body *)
  let scripts c = raw (Printf.sprintf "<script>%s</script><script>%s</script>" c.data c.client_js)
end

(* ---- a mounted app: base + its shell (root) + its router + its document shell.
   The generator emits one [mount list]; the client picks by location, the server by
   request path. The single declarative description of "what runs where". *)
type mount = {
  base : string;
  root : unit -> (unit -> vnode);   (* the app's layout/shell make *)
  router : Router.t;
  document : Doc.ctx -> vnode;       (* the app's chosen template *)
}

(* ---- Reconcile: the create / hydrate / keyed-diff algorithm, parameterized over a
   DOM BACKEND. The browser instantiates it with js_of_ocaml; tests instantiate it
   with an in-memory fake — so the subtlest code (keyed reconciliation, hydration
   adoption + recovery) is unit-testable in milliseconds, and the algorithm itself is
   platform-agnostic. *)
module type BACKEND = sig
  type node
  val create_text : string -> node
  val create_element : string -> node
  val get_text : node -> string
  val set_text : node -> string -> unit
  val get_attr : node -> string -> string option
  val set_attr : node -> string -> string -> unit
  val remove_attr : node -> string -> unit
  val set_prop : node -> string -> string -> unit   (* value/checked as live property *)
  val get_prop : node -> string -> string
  val append : node -> node -> unit                 (* parent child *)
  (* insert [child] before [ref] under [parent]; [ref = None] appends. The DOM insertBefore — the
     minimal-move primitive keyed reconciliation needs to reorder without re-appending every node. *)
  val insert_before : node -> node -> node option -> unit
  val remove : node -> node -> unit                 (* parent child *)
  val replace : node -> node -> node -> unit        (* parent new old *)
  val parent : node -> node option
  val listen : node -> string -> (unit -> unit) ref -> unit  (* attach a handler over the ref *)
  val child : node -> int -> node option            (* nth child (hydration) *)
  val first_child : node -> node option
  (* hydration validation: the kind of an adopted SSR node, so hydrate can detect SSR/CSR drift
     instead of blindly adopting the wrong node. [node_tag] is the element's tag name LOWERCASED
     ([Some "div"]), or [None] for a non-element (text/comment); [is_text] is true for a text node. *)
  val node_tag : node -> string option
  val is_text : node -> bool
end

(* Longest-increasing-subsequence mask for keyed reconciliation (the Vue3/Inferno trick). Given
   [old_idx], where [old_idx.(i)] is the previous position of the i-th new child (or [-1] for a
   freshly-created child that has no previous position), return a boolean array marking the indices
   that form a longest increasing run of the kept items' old positions. Those nodes are already in
   the correct relative order and can stay put; every other node is moved into place. O(n log n).
   [-1] entries are never part of the sequence (new nodes always need inserting). *)
let lis_mask (old_idx : int array) : bool array =
  let n = Array.length old_idx in
  let mask = Array.make n false in
  (* [tails.(len)] = index (into old_idx) of the smallest tail of an increasing subseq of length
     len+1; [prev] = predecessor links to reconstruct the chain. *)
  let tails = Array.make n 0 in
  let prev = Array.make n (-1) in
  let len = ref 0 in
  for i = 0 to n - 1 do
    if old_idx.(i) >= 0 then begin
      (* binary search for the first tail whose value >= old_idx.(i) (strictly-increasing LIS) *)
      let lo = ref 0 and hi = ref !len in
      while !lo < !hi do
        let mid = (!lo + !hi) / 2 in
        if old_idx.(tails.(mid)) < old_idx.(i) then lo := mid + 1 else hi := mid
      done;
      let pos = !lo in
      if pos > 0 then prev.(i) <- tails.(pos - 1);
      tails.(pos) <- i;
      if pos = !len then incr len
    end
  done;
  (* walk back from the last tail of the longest chain *)
  if !len > 0 then begin
    let k = ref tails.(!len - 1) in
    while !k >= 0 do mask.(!k) <- true; k := prev.(!k) done
  end;
  mask

module Reconcile (B : BACKEND) = struct
  type melem = { tag : string; key : string option; node : B.node;
                 mutable attrs : attr list; mutable children : mounted list;
                 handlers : (string, (unit -> unit) ref) Hashtbl.t }
  and mcomp = { mcid : string; mckey : string option; mutable msub : mounted;
                meff : reaction; cleanups : (unit -> unit) list ref }
  and mounted = MText of B.node | MElem of melem | MComp of mcomp

  let rec mnode = function MText n -> n | MElem e -> e.node | MComp mc -> mnode mc.msub
  let ensure_handler handlers node ev f =
    match Hashtbl.find_opt handlers ev with
    | Some r -> r := f
    | None -> let r = ref f in Hashtbl.replace handlers ev r; B.listen node ev r
  let put_attr node handlers = function
    | Attr ("value", v) -> B.set_prop node "value" v
    | Attr ("checked", v) -> B.set_prop node "checked" v
    | Attr (k, v) -> B.set_attr node k v
    | Handler (ev, f) -> ensure_handler handlers node ev f
  let key_of_m = function MElem e -> e.key | MComp mc -> mc.mckey | _ -> None
  let key_of_v = function Elem { key; _ } -> key | Comp { ckey; _ } -> ckey | _ -> None
  let has_key v = key_of_v v <> None

  let rec create = function
    | Text s -> MText (B.create_text s)
    | Comp c -> mount_comp c
    | Fragment _ -> MText (B.create_text "")
    | Elem { tag; key; attrs; children } ->
      let node = B.create_element tag in
      let handlers = Hashtbl.create 4 in
      List.iter (put_attr node handlers) attrs;
      let children = List.map (fun ch -> let m = create ch in B.append node (mnode m); m) (flatten children) in
      MElem { tag; key; node; attrs; children; handlers }

  and mk_effect ~first sub =
    mk_reaction (fun () -> match !sub with `R (render, m) ->
        let v = render () in
        (match m with
         | None -> sub := `R (render, Some (first v))
         | Some old -> (match B.parent (mnode old) with
             | Some p -> sub := `R (render, Some (reconcile ~parent:p old v))
             | None -> sub := `R (render, Some old))))

  and instantiate ~first (c : comp) =
    let cleanups = ref [] in
    let saved = !current_cleanups in
    current_cleanups := cleanups;
    let render = c.setup () in
    let sub = ref (`R (render, None)) in
    let eff = mk_effect ~first sub in
    run_effect eff;
    current_cleanups := saved;
    let m = (match !sub with `R (_, Some m) -> m | _ -> failwith "comp produced nothing") in
    MComp { mcid = c.cid; mckey = c.ckey; msub = m; meff = eff; cleanups }
  and mount_comp c = instantiate ~first:create c

  (* the adopted SSR node [dom] doesn't match what this vnode expects (SSR/CSR drift): build the
     node fresh and swap it in over the wrong one, so a [<div>] vnode never silently adopts a
     [<span>] and patches the wrong node. *)
  and hydrate_recover dom vnode =
    let m = create vnode in
    (match B.parent dom with Some p -> B.replace p (mnode m) dom | None -> ());
    m

  and hydrate dom = function
    | Text _ as v ->
      (* the adoptee must be a text node; a drifted element here would be patched as text *)
      if B.is_text dom then MText dom else hydrate_recover dom v
    | Comp c -> instantiate ~first:(fun v -> hydrate dom v) c
    | Fragment _ -> MText dom
    | (Elem { tag; key; attrs; children }) as v ->
      (* tag must match; otherwise adopt-in-place would corrupt the tree on the first patch *)
      if B.node_tag dom <> Some (String.lowercase_ascii tag) then hydrate_recover dom v
      else begin
        let handlers = Hashtbl.create 4 in
        List.iter (function Handler (ev, f) -> ensure_handler handlers dom ev f | Attr _ -> ()) attrs;
        let children = List.mapi (fun i ch ->
          match B.child dom i with
          | Some d -> hydrate d ch
          | None -> let m = create ch in B.append dom (mnode m); m) (flatten children) in
        MElem { tag; key; node = dom; attrs; children; handlers }
      end

  and unmount = function
    | MComp mc -> List.iter (fun f -> f ()) !(mc.cleanups); dispose mc.meff; unmount mc.msub
    | MElem e -> List.iter unmount e.children
    | MText _ -> ()

  and patch_attrs e new_attrs =
    List.iter (function
      | Attr ("value", v) -> if B.get_prop e.node "value" <> v then B.set_prop e.node "value" v
      | Attr ("checked", v) -> B.set_prop e.node "checked" v
      | Attr (k, v) -> if B.get_attr e.node k <> Some v then B.set_attr e.node k v
      | Handler (ev, f) -> ensure_handler e.handlers e.node ev f) new_attrs;
    List.iter (function
      | Attr (k, _) when not (List.exists (function Attr (k2, _) -> k2 = k | _ -> false) new_attrs) -> B.remove_attr e.node k
      | _ -> ()) e.attrs

  and reconcile ~parent m vnode : mounted =
    match m, vnode with
    | MText t, Text s -> (if B.get_text t <> s then B.set_text t s); m
    | MElem e, Elem { tag; attrs; children; _ } when e.tag = tag ->
      patch_attrs e attrs; e.attrs <- attrs;
      e.children <- reconcile_children ~parent:e.node e.children (flatten children); m
    | MComp mc, Comp c when mc.mcid = c.cid && mc.mckey = c.ckey -> m
    | _ -> unmount m; let m' = create vnode in B.replace parent (mnode m') (mnode m); m'

  and reconcile_children ~parent olds news =
    if List.exists has_key news then keyed ~parent olds news else positional ~parent olds news
  and positional ~parent olds news = match olds, news with
    | o :: os, n :: ns -> let m = reconcile ~parent o n in m :: positional ~parent os ns
    | [], n :: ns -> let m = create n in B.append parent (mnode m); m :: positional ~parent [] ns
    | o :: os, [] -> unmount o; B.remove parent (mnode o); positional ~parent os []
    | [], [] -> []
  and keyed ~parent olds news =
    (* index olds by key -> (mounted, old position) *)
    let map = Hashtbl.create 16 in
    List.iteri (fun i m -> match key_of_m m with Some k -> Hashtbl.replace map k (m, i) | None -> ()) olds;
    let used = Hashtbl.create 16 in
    let news = Array.of_list news in
    let n = Array.length news in
    let result = Array.make n (MText (B.create_text "")) in
    let old_idx = Array.make n (-1) in     (* previous position of each new child, -1 if fresh *)
    Array.iteri (fun i vn ->
        match key_of_v vn with
        | Some k when Hashtbl.mem map k ->
          let (m, oi) = Hashtbl.find map k in
          Hashtbl.replace used k ();
          result.(i) <- reconcile ~parent m vn;
          old_idx.(i) <- oi
        | _ -> result.(i) <- create vn (* fresh or unkeyed: old_idx stays -1 *)) news;
    (* remove the olds that no longer appear *)
    List.iter (fun m -> match key_of_m m with
        | Some k when not (Hashtbl.mem used k) -> unmount m; B.remove parent (mnode m)
        | _ -> ()) olds;
    (* place into final order with MINIMAL moves: nodes whose old positions already form an
       increasing run (the LIS) are in correct relative order and stay put; everything else —
       fresh nodes and out-of-order kept nodes — is inserted before its right neighbour. Right to
       left so the reference node [result.(i+1)] is already in its final position. *)
    let stay = lis_mask old_idx in
    for i = n - 1 downto 0 do
      if not (i >= 0 && old_idx.(i) >= 0 && stay.(i)) then begin
        let ref_node = if i + 1 < n then Some (mnode result.(i + 1)) else None in
        B.insert_before parent (mnode result.(i)) ref_node
      end
    done;
    Array.to_list result

  (* hydrate or render under [container]: first run adopts the SSR root, later runs diff.
     Returns a disposer that stops the root render effect (so it never re-runs on a later
     signal write) AND unmounts the whole tree — running every component's cleanups + the
     nested effect disposals. Idempotent. [mount_root] is this with the handle dropped, so
     its public type is unchanged; an embedder that owns a sub-tree's lifetime calls
     [mount_root_disposable] and disposes on teardown. *)
  let mount_root_disposable container (render : unit -> vnode) =
    let mounted = ref None in
    let eff = mk_reaction (fun () ->
        let vnode = render () in
        match !mounted with
        | None -> (match B.first_child container with
            | Some first -> mounted := Some (hydrate first vnode)
            | None -> let m = create vnode in B.append container (mnode m); mounted := Some m)
        | Some m -> mounted := Some (reconcile ~parent:container m vnode)) in
    run_effect eff;
    fun () ->
      dispose eff;
      (match !mounted with Some m -> unmount m | None -> ());
      mounted := None
  let mount_root container render = ignore (mount_root_disposable container render)
end

(* ──── signals ──── *)

let%test "peek initial" =
  let s = signal 0 in
  peek s = 0

let%test_unit "effect runs once on create" =
  let s = signal 0 in
  let runs = ref 0 in
  let _ = watch (fun () -> incr runs; ignore (get s)) in
  Fennec_hunt_unit.check "effect runs once on create" (!runs = 1)

let%test_unit "effect re-runs on change" =
  let s = signal 0 in
  let runs = ref 0 in
  let _ = watch (fun () -> incr runs; ignore (get s)) in
  set s 1;
  Fennec_hunt_unit.check "effect re-runs on change" (!runs = 2)

let%test_unit "no re-run on equal set" =
  let s = signal 0 in
  let runs = ref 0 in
  let _ = watch (fun () -> incr runs; ignore (get s)) in
  set s 1; set s 1;
  Fennec_hunt_unit.check "no re-run on equal set" (!runs = 2)

let%test_unit "update notifies" =
  let s = signal 0 in
  let runs = ref 0 in
  let _ = watch (fun () -> incr runs; ignore (get s)) in
  set s 1; update s (fun n -> n + 1);
  Fennec_hunt_unit.check "update notifies" (!runs = 3 && peek s = 2)

let%test_unit "disposed effect never re-runs" =
  let s = signal 0 in
  let r2 = ref 0 in
  let stop = watch (fun () -> incr r2; ignore (get s)) in
  let before = !r2 in
  stop (); set s 99;
  Fennec_hunt_unit.check "disposed effect never re-runs" (!r2 = before)

let%test_unit "dynamic deps: tracks a" =
  let a = signal 1 and b = signal 10 and pick = signal true in
  let last = ref 0 in
  let _ = watch (fun () -> last := if get pick then get a else get b) in
  Fennec_hunt_unit.check "dynamic deps: tracks a" (!last = 1)

let%test_unit "switches to b" =
  let a = signal 1 and b = signal 10 and pick = signal true in
  let last = ref 0 in
  let _ = watch (fun () -> last := if get pick then get a else get b) in
  set pick false;
  Fennec_hunt_unit.check "switches to b" (!last = 10)

let%test_unit "no longer tracks a after switch" =
  let a = signal 1 and b = signal 10 and pick = signal true in
  let last = ref 0 in
  let _ = watch (fun () -> last := if get pick then get a else get b) in
  set pick false; set a 5;
  Fennec_hunt_unit.check "no longer tracks a after switch" (!last = 10)

let%test_unit "custom eq (always-notify) re-runs on equal set" =
  let no = signal 0 ~eq:(fun _ _ -> false) in
  let c = ref 0 in
  let _ = watch (fun () -> incr c; ignore (get no)) in
  set no 0;
  Fennec_hunt_unit.check "custom eq (always-notify) re-runs on equal set" (!c = 2)

(* ──── matcher ──── *)

let%test "root" =
  Matcher.match_one ~pattern:"/" "/" = Some []

let%test "exact" =
  Matcher.match_one ~pattern:"/about" "/about" = Some []

let%test "named param" =
  Matcher.match_one ~pattern:"/users/:id" "/users/42" = Some [("id","42")]

let%test "two params" =
  Matcher.match_one ~pattern:"/p/:a/:b" "/p/x/y" = Some [("a","x");("b","y")]

let%test "catch-all" =
  Matcher.match_one ~pattern:"/files/*" "/files/a/b" = Some [("*","a/b")]

let%test "no match (len)" =
  Matcher.match_one ~pattern:"/users/:id" "/users" = None

let%test "no match (lit)" =
  Matcher.match_one ~pattern:"/a" "/b" = None

let%test "trailing slash normalizes" =
  Matcher.match_one ~pattern:"/about" "/about/" = Some []

let%test "find first-match" =
  let table = [("/", `Home); ("/products", `List); ("/products/:id", `Show)] in
  Matcher.find table "/products" = Some (`List, [])

let%test "find param" =
  let table = [("/", `Home); ("/products", `List); ("/products/:id", `Show)] in
  Matcher.find table "/products/7" = Some (`Show, [("id","7")])

let%test "param accessor" =
  Matcher.param [("id","7")] "id" = Some "7"

(* the documented invariant for ANY path: splitting yields only non-empty segments (so leading,
   trailing, and doubled slashes never produce an empty segment that would corrupt matching). *)
let%prop "segments are always non-empty" = fun (path : string) ->
  List.for_all (fun seg -> seg <> "") (Matcher.segments path)

(* ──── head merge ──── *)

let%test_unit "title last wins" =
  let r = Head.resolve [ (0, [Head.Tag.title "A"; Head.Tag.meta ~name:"description" "old"]);
                          (1, [Head.Tag.title "B"; Head.Tag.meta ~name:"description" "new"; Head.Tag.og "og:x" "y"]) ] in
  Fennec_hunt_unit.check "title last wins"
    (List.exists (function Head.Title "B" -> true | _ -> false) r)

let%test_unit "stale title dropped" =
  let r = Head.resolve [ (0, [Head.Tag.title "A"]); (1, [Head.Tag.title "B"]) ] in
  Fennec_hunt_unit.check "stale title dropped"
    (not (List.exists (function Head.Title "A" -> true | _ -> false) r))

let%test_unit "meta deduped by name (last)" =
  let r = Head.resolve [ (0, [Head.Tag.meta ~name:"description" "old"]);
                          (1, [Head.Tag.meta ~name:"description" "new"]) ] in
  Fennec_hunt_unit.check "meta deduped by name (last)"
    (List.exists (function Head.Meta a -> List.assoc_opt "content" a = Some "new" | _ -> false) r)

let%test_unit "non-conflicting kept" =
  let r = Head.resolve [ (0, [Head.Tag.title "A"]);
                          (1, [Head.Tag.og "og:x" "y"]) ] in
  Fennec_hunt_unit.check "non-conflicting kept"
    (List.exists (function Head.Meta a -> List.assoc_opt "property" a = Some "og:x" | _ -> false) r)

let%test_unit "tag_key title" =
  Fennec_hunt_unit.check_eq "tag_key title"
    ~expected:"title" ~got:(Head.tag_key (Head.Tag.title "z"))

let%test_unit "tag_key meta" =
  Fennec_hunt_unit.check_eq "tag_key meta"
    ~expected:"meta:description" ~got:(Head.tag_key (Head.Tag.meta ~name:"description" "z"))

(* ──── SSR (to_html) ──── *)

let%test_unit "escape text" =
  Fennec_hunt_unit.check_eq "escape text"
    ~expected:"&lt;a&amp;&quot;b&gt;" ~got:(to_html (text "<a&\"b>"))

let%test_unit "attr escape" =
  Fennec_hunt_unit.check_eq "attr escape"
    ~expected:{|<div class="x"></div>|} ~got:(to_html (h "div" [attr "class" "x"] []))

let%test_unit "void self-close" =
  Fennec_hunt_unit.check_eq "void self-close"
    ~expected:"<input/>" ~got:(to_html (h "input" [] []))

let%test_unit "handlers omitted in ssr" =
  Fennec_hunt_unit.check_eq "handlers omitted in ssr"
    ~expected:"<button>go</button>" ~got:(to_html (h "button" [on "click" (fun () -> ())] [text "go"]))

let%test_unit "fragment concats" =
  Fennec_hunt_unit.check_eq "fragment concats"
    ~expected:"ab" ~got:(to_html (frag [text "a"; text "b"]))

let%test_unit "adjacent text coalesces" =
  Fennec_hunt_unit.check_eq "adjacent text coalesces"
    ~expected:"<p>xy</p>" ~got:(to_html (h "p" [] [text "x"; text "y"]))

let%test_unit "raw passthrough" =
  Fennec_hunt_unit.check_eq "raw passthrough"
    ~expected:"<x>" ~got:(to_html (raw "<x>"))

let%test_unit "doctype" =
  Fennec_hunt_unit.check_eq "doctype"
    ~expected:"<!doctype" ~got:(String.sub (document (h "html" [] [])) 0 9)

(* ──── data resources ──── *)

let%test_unit "seeded resource is ready value" =
  Data.clear_seed ();
  Data.put_seed "k" "v";
  let hit = Data.string "k" ~fallback:"f" () in
  Fennec_hunt_unit.check_eq "seeded resource is ready value"
    ~expected:"v" ~got:(Data.value hit)

let%test_unit "seeded not loading" =
  Data.clear_seed ();
  Data.put_seed "k" "v";
  let hit = Data.string "k" ~fallback:"f" () in
  Fennec_hunt_unit.check "seeded not loading" (not (Data.loading hit))

let%test_unit "miss shows fallback" =
  Data.clear_seed ();
  Data.set_source (fun _ _ -> ());
  let miss = Data.string "absent" ~fallback:"f" () in
  Fennec_hunt_unit.check_eq "miss shows fallback"
    ~expected:"f" ~got:(Data.value miss)

let%test_unit "miss is loading" =
  Data.clear_seed ();
  Data.set_source (fun _ _ -> ());
  let miss = Data.string "absent" ~fallback:"f" () in
  Fennec_hunt_unit.check "miss is loading" (Data.loading miss)

(* Data.model: a typed resource round-trips the SAME codec through the seed (what Sift.encode_json
   put in on the server, Sift.decode_json reads back on the client). *)
type model_demo = { who : string; count : int }

let model_demo_codec =
  Sift.(
    seal
      (record (fun who count -> { who; count })
      |> field (req "who" (non_empty string)) (fun t -> t.who)
      |> field (req "count" (min_i 0 int)) (fun t -> t.count)))

let model_demo_fallback = { who = "?"; count = 0 }

let%test_unit "Data.model decodes a typed seed via the codec (server encode -> client decode)" =
  Data.clear_seed ();
  (* exactly what the server's [Sift.encode_json codec v] emits into the seed *)
  Data.put_seed "/api/demo" (Sift.encode_json model_demo_codec { who = "Ada"; count = 7 });
  let r = Data.model model_demo_codec "/api/demo" ~fallback:model_demo_fallback () in
  let v = Data.value r in
  Fennec_hunt_unit.check "typed seed round-trips" (v.who = "Ada" && v.count = 7);
  Fennec_hunt_unit.check "typed seed is not loading" (not (Data.loading r))

let%test_unit "Data.model falls back on a malformed / invalid payload (never crashes the UI)" =
  Data.clear_seed ();
  (* count = -1 violates [min_i 0] -> decode errors -> the resource yields the fallback, not an exn *)
  Data.put_seed "/api/demo" {|{"who":"Ada","count":-1}|};
  let r = Data.model model_demo_codec "/api/demo" ~fallback:model_demo_fallback () in
  Fennec_hunt_unit.check "invalid payload -> fallback" (Data.value r = model_demo_fallback)

(* Data.local / Data.model_local: the CO-LOCATED fetcher. Declaring one registers a producer keyed by
   the derived path; the framework drains the registry to fuse the SSR seed + the refetch route. *)
let%test_unit "Data.local_path derives /api/<name>, honors ~path, passes a /-prefixed name through" =
  Fennec_hunt_unit.check "bare name -> /api/<name>" (Data.local_path "greeting" = "/api/greeting");
  Fennec_hunt_unit.check "~path verbatim" (Data.local_path ~path:"/x/y" "greeting" = "/x/y");
  Fennec_hunt_unit.check "/-prefixed name is the path" (Data.local_path "/api/raw" = "/api/raw")

let%test_unit "Data.local registers a producer that runs the inline fetcher (server side)" =
  let calls = ref 0 in
  let r = Data.local "greet_test" ~fallback:"…" (Data.Server_only.fn (fun () -> incr calls; "hi from server")) in
  (* the resource is keyed by the derived path and refetchable like a plain string resource *)
  Fennec_hunt_unit.check "value starts at fallback (no seed/source yet)" (Data.value r = "…");
  (* registration happened under the derived path, and the producer runs the fetcher lazily *)
  (match Data.local_source "/api/greet_test" with
   | Some src ->
     Fennec_hunt_unit.check "producer runs the fetcher" (Data.src_produce src = "hi from server");
     Fennec_hunt_unit.check "fetcher actually invoked" (!calls = 1);
     Fennec_hunt_unit.check "string source is served as text (not json)" (not (Data.src_is_json src))
   | None -> Fennec_hunt_unit.check "producer registered" false)

let%test_unit "Data.model_local encodes the typed value through the codec for the seed/route" =
  let r =
    Data.model_local model_demo_codec "demo_local" ~fallback:model_demo_fallback
      (Data.Server_only.fn (fun () -> { who = "Ada"; count = 7 }))
  in
  Fennec_hunt_unit.check "typed value falls back until seeded" (Data.value r = model_demo_fallback);
  match Data.local_source "/api/demo_local" with
  | Some src ->
    (* the producer emits EXACTLY what Sift.encode_json would — the same bytes the route + seed carry *)
    let expected = Sift.encode_json model_demo_codec { who = "Ada"; count = 7 } in
    Fennec_hunt_unit.check "producer encodes via the codec" (Data.src_produce src = expected);
    Fennec_hunt_unit.check "typed source is served as json" (Data.src_is_json src);
    (* and the typed resource decodes that very payload back through the seed *)
    Data.clear_seed ();
    Data.put_seed "/api/demo_local" (Data.src_produce src);
    let r2 = Data.model_local model_demo_codec "demo_local" ~fallback:model_demo_fallback (Data.Server_only.fn (fun () -> model_demo_fallback)) in
    let v = Data.value r2 in
    Fennec_hunt_unit.check "seed decodes back to the typed value" (v.who = "Ada" && v.count = 7);
    Data.clear_seed ()
  | None -> Fennec_hunt_unit.check "typed producer registered" false

let%test_unit "Data.local_sources lists registered co-located sources for the framework to drain" =
  ignore (Data.local "drain_test" ~fallback:"f" (Data.Server_only.fn (fun () -> "v")));
  let found = List.exists (fun (p, src) -> p = "/api/drain_test" && Data.src_produce src = "v") (Data.local_sources ()) in
  Fennec_hunt_unit.check "registered source appears in local_sources" found

let%test_unit "to_script assigns global" =
  Data.clear_seed (); Data.put_seed "u" "ok";
  let s = Data.to_script () in
  Fennec_hunt_unit.check "to_script assigns global"
    (Fennec_hunt_unit.str_contains s "window.__FUR_DATA__={")

let%test_unit "to_script contains pair" =
  Data.clear_seed (); Data.put_seed "u" "ok";
  let s = Data.to_script () in
  Fennec_hunt_unit.check "to_script contains pair"
    (Fennec_hunt_unit.str_contains s "\"u\":\"ok\"")

let%test "to_script escapes <" =
  Data.js_string "<x>" = "\"\\u003cx>\""

(* ──── router ──── *)

let%test_unit "relativize strips base" =
  Fennec_hunt_unit.check_eq "relativize strips base"
    ~expected:"/products" ~got:(Router.relativize "/shop" "/shop/products")

let%test_unit "relativize base->root" =
  Fennec_hunt_unit.check_eq "relativize base->root"
    ~expected:"/" ~got:(Router.relativize "/shop" "/shop")

let%test_unit "absolutize prefixes" =
  Fennec_hunt_unit.check_eq "absolutize prefixes"
    ~expected:"/shop/products" ~got:(Router.absolutize "/shop" "/products")

let%test_unit "absolutize root->base" =
  Fennec_hunt_unit.check_eq "absolutize root->base"
    ~expected:"/shop" ~got:(Router.absolutize "/shop" "/")

let%test_unit "root base passthrough" =
  Fennec_hunt_unit.check_eq "root base passthrough"
    ~expected:"/x" ~got:(Router.relativize "" "/x")

let%test_unit "reverse build" =
  let dummy _ = fun () -> text "" in
  let t = Router.make ~base:"/shop" ()
          |> Router.page ~name:"product" "/products/:id" dummy
          |> Router.page ~name:"home" "/" dummy in
  Fennec_hunt_unit.check_eq "reverse build"
    ~expected:"/products/7" ~got:(Router.build t "product" [("id","7")])

let%test_unit "href base-prefixed" =
  let dummy _ = fun () -> text "" in
  let t = Router.make ~base:"/shop" ()
          |> Router.page ~name:"product" "/products/:id" dummy
          |> Router.page ~name:"home" "/" dummy in
  Fennec_hunt_unit.check_eq "href base-prefixed"
    ~expected:"/shop/products/7" ~got:(Router.href t "product" [("id","7")])

let%test_unit "typed path" =
  let dummy _ = fun () -> text "" in
  let t = Router.make ~base:"/shop" ()
          |> Router.page ~name:"product" "/products/:id" dummy
          |> Router.page ~name:"home" "/" dummy in
  Router.activate t;
  Fennec_hunt_unit.check_eq "typed path"
    ~expected:"/shop/products/7" ~got:(Router.path t "/products/%d" 7)

let%test_unit "ext raw" =
  Fennec_hunt_unit.check_eq "ext raw"
    ~expected:"/admin/3" ~got:(Router.ext "/admin/%d" 3)

(* ──── reconcile (fake backend) ──── *)

module Fake = struct
  (* [tag = None] models a text node, [Some t] an element — so the fake backend can answer the
     hydration-validation queries ([node_tag]/[is_text]) the way the real DOM does. *)
  type node = { mutable text : string; mutable attrs : (string * string) list;
                mutable kids : node list; mutable par : node option; tag : string option }
  let mk () = { text = ""; attrs = []; kids = []; par = None; tag = None }
  let create_text s = let n = mk () in n.text <- s; n
  let create_element t = { (mk ()) with tag = Some t }
  let get_text n = n.text
  let set_text n s = n.text <- s
  let get_attr n k = List.assoc_opt k n.attrs
  let set_attr n k v = n.attrs <- (k, v) :: List.remove_assoc k n.attrs
  let remove_attr n k = n.attrs <- List.remove_assoc k n.attrs
  let set_prop n k v = set_attr n k v
  let get_prop n k = Option.value ~default:"" (get_attr n k)
  let detach c = match c.par with Some p -> p.kids <- List.filter (fun x -> x != c) p.kids; c.par <- None | None -> ()
  (* test instrumentation: count true MOVES (repositioning a node already under [p]) separately
     from fresh inserts, so a test can assert keyed reconciliation moves only displaced rows.
     [already_under p c] uses physical node equality — NOT [c.par == Some p], which would compare
     freshly-boxed options and never match. *)
  let moves = ref 0
  let reset_moves () = moves := 0
  let already_under p c = match c.par with Some par -> par == p | None -> false
  let append p c = (if already_under p c then incr moves); detach c; p.kids <- p.kids @ [ c ]; c.par <- Some p
  let insert_before p c r =
    if already_under p c then incr moves;
    detach c;
    (match r with
     | None -> p.kids <- p.kids @ [ c ]
     | Some r ->
       p.kids <- List.concat_map (fun x -> if x == r then [ c; r ] else [ x ]) p.kids);
    c.par <- Some p
  let remove _ c = detach c
  let replace p nw od = detach nw;
    p.kids <- List.concat_map (fun x -> if x == od then [ nw ] else [ x ]) p.kids;
    nw.par <- Some p; od.par <- None
  let parent n = n.par
  let listen _ _ _ = ()
  let child n i = List.nth_opt n.kids i
  let first_child n = match n.kids with x :: _ -> Some x | [] -> None
  let node_tag n = Option.map String.lowercase_ascii n.tag
  let is_text n = n.tag = None
end

module D = Reconcile (Fake)

let texts_ ul = String.concat "," (List.map (fun li -> match li.Fake.kids with t :: _ -> t.Fake.text | [] -> "") ul.Fake.kids)

let%test_unit "keyed initial" =
  let model = signal [ 1; 2; 3 ] in
  let render () = h "ul" [] (each (get model) (fun i -> h ~key:(string_of_int i) "li" [] [ text (string_of_int i) ])) in
  let root = Fake.create_element "" in
  let _ = D.mount_root root render in
  let ul () = List.hd root.Fake.kids in
  Fennec_hunt_unit.check_eq "keyed initial" ~expected:"1,2,3" ~got:(texts_ (ul ()))

let%test_unit "keyed reorder" =
  let model = signal [ 1; 2; 3 ] in
  let render () = h "ul" [] (each (get model) (fun i -> h ~key:(string_of_int i) "li" [] [ text (string_of_int i) ])) in
  let root = Fake.create_element "" in
  let _ = D.mount_root root render in
  set model [ 3; 1; 2 ];
  Fennec_hunt_unit.check_eq "keyed reorder" ~expected:"3,1,2" ~got:(texts_ (List.hd root.Fake.kids))

let%test_unit "keyed remove" =
  let model = signal [ 1; 2; 3 ] in
  let render () = h "ul" [] (each (get model) (fun i -> h ~key:(string_of_int i) "li" [] [ text (string_of_int i) ])) in
  let root = Fake.create_element "" in
  let _ = D.mount_root root render in
  set model [ 3; 1; 2 ]; set model [ 3; 2 ];
  Fennec_hunt_unit.check_eq "keyed remove" ~expected:"3,2" ~got:(texts_ (List.hd root.Fake.kids))

let%test_unit "keyed add" =
  let model = signal [ 1; 2; 3 ] in
  let render () = h "ul" [] (each (get model) (fun i -> h ~key:(string_of_int i) "li" [] [ text (string_of_int i) ])) in
  let root = Fake.create_element "" in
  let _ = D.mount_root root render in
  set model [ 3; 1; 2 ]; set model [ 3; 2 ]; set model [ 3; 2; 4 ];
  Fennec_hunt_unit.check_eq "keyed add" ~expected:"3,2,4" ~got:(texts_ (List.hd root.Fake.kids))

let%test "no orphans after diff" =
  let model = signal [ 1; 2; 3 ] in
  let render () = h "ul" [] (each (get model) (fun i -> h ~key:(string_of_int i) "li" [] [ text (string_of_int i) ])) in
  let root = Fake.create_element "" in
  let _ = D.mount_root root render in
  set model [ 3; 1; 2 ]; set model [ 3; 2 ]; set model [ 3; 2; 4 ];
  List.length (List.hd root.Fake.kids).Fake.kids = 3

(* ──── keyed MINIMAL moves (the append-storm fix) ──── *)

(* a keyed list renderer + a model signal, with a helper that re-renders and reports how many
   already-attached nodes were repositioned. Pre-fix [keyed] re-appended EVERY child on any update
   (N moves to shuffle one row); now only displaced nodes move. *)
let keyed_move_count initial next =
  let model = signal initial in
  let render () = h "ul" [] (each (get model) (fun i -> h ~key:(string_of_int i) "li" [] [ text (string_of_int i) ])) in
  let root = Fake.create_element "" in
  let _ = D.mount_root root render in
  Fake.reset_moves ();
  set model next;                         (* native: synchronous reconcile *)
  let order = texts_ (List.hd root.Fake.kids) in
  (!Fake.moves, order)

(* rotate one element to the front of 3: only that element moves (the other two are the LIS). *)
let%test "keyed reorder [1;2;3]->[3;1;2] performs exactly 1 move" =
  let (moves, order) = keyed_move_count [ 1; 2; 3 ] [ 3; 1; 2 ] in
  moves = 1 && order = "3,1,2"

(* move one element out of a run of 5: displacement is 1, not 5. *)
let%test "keyed reorder [1;2;3;4;5]->[1;2;5;3;4] performs exactly 1 move" =
  let (moves, order) = keyed_move_count [ 1; 2; 3; 4; 5 ] [ 1; 2; 5; 3; 4 ] in
  moves = 1 && order = "1,2,5,3,4"

(* re-rendering the SAME order must move nothing (no append-storm on a no-op update). *)
let%test "keyed same order performs 0 moves" =
  let (moves, order) = keyed_move_count [ 1; 2; 3; 4 ] [ 1; 2; 3; 4 ] in
  moves = 0 && order = "1,2,3,4"

(* a full reversal of 4 has an LIS of length 1, so the minimal move count is n-1 = 3 — and the
   result is still correct. (This is the worst case; it must not exceed n-1.) *)
let%test "keyed reversal [1;2;3;4]->[4;3;2;1] performs n-1 moves, correctly" =
  let (moves, order) = keyed_move_count [ 1; 2; 3; 4 ] [ 4; 3; 2; 1 ] in
  moves = 3 && order = "4,3,2,1"

(* a swap of the two ends keeps the middle in place: [1;2;3;4;5]->[5;2;3;4;1]. The LIS is the
   middle run 2,3,4, so only the two ends move (2 moves), not 5. *)
let%test "keyed end-swap moves only the two ends" =
  let (moves, order) = keyed_move_count [ 1; 2; 3; 4; 5 ] [ 5; 2; 3; 4; 1 ] in
  moves = 2 && order = "5,2,3,4,1"

(* a FRESH node inserted in the middle is placed correctly (insert_before with a ref node), and
   the surrounding kept nodes don't move — the original code only ever appended, so mid-insert is
   the path most prone to mis-positioning. *)
let%test "keyed mid-insert positions the new node without moving the kept ones" =
  let (moves, order) = keyed_move_count [ 1; 2; 3 ] [ 1; 9; 2; 3 ] in
  moves = 0 && order = "1,9,2,3"

(* combined: prepend a new node AND rotate — [1;2;3]->[3;0;1;2]: 0 is fresh, 3 moves to front,
   1 and 2 stay (the LIS). Order correct, only the rotated node counts as a move. *)
let%test "keyed insert + reorder together" =
  let (moves, order) = keyed_move_count [ 1; 2; 3 ] [ 3; 0; 1; 2 ] in
  moves = 1 && order = "3,0,1,2"

let%test_unit "text patched in place" =
  let t_ = signal "a" in
  let render2 () = h "p" [ attr "data-x" (get t_) ] [ text (get t_) ] in
  let r2 = Fake.create_element "" in
  let _ = D.mount_root r2 render2 in
  set t_ "b";
  let ptext () = match (List.hd r2.Fake.kids).Fake.kids with x :: _ -> x.Fake.text | [] -> "" in
  Fennec_hunt_unit.check_eq "text patched in place" ~expected:"b" ~got:(ptext ())

let%test_unit "attr patched in place" =
  let t_ = signal "a" in
  let render2 () = h "p" [ attr "data-x" (get t_) ] [ text (get t_) ] in
  let r2 = Fake.create_element "" in
  let _ = D.mount_root r2 render2 in
  set t_ "b";
  Fennec_hunt_unit.check_eq "attr patched in place"
    ~expected:"b" ~got:(Option.value ~default:"" (List.assoc_opt "data-x" (List.hd r2.Fake.kids).Fake.attrs))

(* on_mount OWNER SCOPE (the leak regression): a [watch] started inside on_mount — the documented
   place to subscribe — must register its disposer on the OWNING component, so it is torn down on
   that component's unmount, not leaked for the page's life. We mount a comp whose on_mount watches
   a shared signal, flush mounts, then unmount the root: the signal's subscriber count must return to
   baseline. Pre-fix, flush_mounts ran the callback under the root cleanups, so the watch's disposer
   landed on the root and the subscription survived the comp's unmount. *)
let%test "on_mount watch is scoped to its component and disposes on unmount" =
  let saved_browser = !is_browser in
  is_browser := true;
  let s = signal 0 in
  let base = List.length s.subs in
  let comp_v = comp ~cid:"mounter" (fun () ->
      on_mount (fun () -> ignore (watch (fun () -> ignore (get s))));
      fun () -> text "x") in
  let root = Fake.create_element "" in
  let dispose_root = D.mount_root_disposable root (fun () -> comp_v) in
  flush_mounts ();
  let after_mount = List.length s.subs in
  dispose_root ();
  let after_unmount = List.length s.subs in
  is_browser := saved_browser;
  after_mount = base + 1 && after_unmount = base

(* …and no accumulation across many mount/unmount cycles (the unbounded-over-row-churn shape): each
   cycle's watch must come and go, so the subscriber count is flat, not climbing by one per cycle. *)
let%test "on_mount watch does not accumulate across N mount/unmount cycles" =
  let saved_browser = !is_browser in
  is_browser := true;
  let s = signal 0 in
  let base = List.length s.subs in
  let cycle () =
    let comp_v = comp ~cid:"mounter" (fun () ->
        on_mount (fun () -> ignore (watch (fun () -> ignore (get s))));
        fun () -> text "x") in
    let root = Fake.create_element "" in
    let dispose_root = D.mount_root_disposable root (fun () -> comp_v) in
    flush_mounts ();
    dispose_root ()
  in
  for _ = 1 to 50 do cycle () done;
  is_browser := saved_browser;
  List.length s.subs = base

(* ──── batching / re-entrancy / flush_sync ──── *)

(* the value write is synchronous even though the effect flush is batched: [get]/[peek] right
   after [set] must observe the new value (only the re-render is deferred). *)
let%test "get right after set sees the new value (write is synchronous)" =
  let s = signal 0 in
  set s 7;
  peek s = 7 && (let seen = ref (-1) in flush_sync (fun () -> set s 9; seen := peek s); !seen = 9)

(* K writes in ONE batch (= one handler turn) re-render a subscribing component exactly ONCE,
   not K times. We count renders via the reconcile render thunk over the Fake backend. *)
let%test "K writes in one batch render the subscriber once" =
  let n = signal 0 in
  let renders = ref 0 in
  let render () = incr renders; h "p" [] [ text (string_of_int (get n)) ] in
  let root = Fake.create_element "" in
  let _ = D.mount_root root render in
  let after_mount = !renders in
  flush_sync (fun () -> for i = 1 to 20 do set n i done);
  let one_batch = !renders - after_mount in
  one_batch = 1 && (match (List.hd root.Fake.kids).Fake.kids with t :: _ -> t.Fake.text = "20" | [] -> false)

(* diamond de-dup: two signals both feed one component; setting BOTH in a batch re-renders the
   component ONCE (the shared reaction is marked once, not once per path). *)
let%test "diamond: two signals into one component, both set in a batch, render once" =
  let a = signal 0 and b = signal 0 in
  let renders = ref 0 in
  let render () = incr renders; h "p" [] [ text (string_of_int (get a + get b)) ] in
  let root = Fake.create_element "" in
  let _ = D.mount_root root render in
  let after_mount = !renders in
  flush_sync (fun () -> set a 1; set b 1);
  !renders - after_mount = 1
  && (match (List.hd root.Fake.kids).Fake.kids with t :: _ -> t.Fake.text = "2" | [] -> false)

(* re-entrancy guard: an effect that SETs a signal it GETs must terminate (bounded), not recurse
   into a stack overflow or spin forever. After it settles, the signal advanced by the cap and the
   effect ran a bounded number of times. *)
let%test "self-writing effect terminates (bounded, no overflow)" =
  let c = signal 0 in
  let runs = ref 0 in
  let _ = watch (fun () -> incr runs; let v = get c in if v < 1_000_000 then set c (v + 1)) in
  (* it must have stopped on its own; the cap bounds both the run count and the value *)
  !runs <= reentry_cap + 1 && peek c <= reentry_cap + 1 && peek c > 0

(* flush_sync forces a synchronous flush: after it returns, a pending re-render has ALREADY run,
   even though a bare batched write would otherwise defer. Here the effect's side effect (a ref)
   is observable immediately after flush_sync, with no extra flush. *)
let%test "flush_sync forces a synchronous flush" =
  let s = signal 0 in
  let mirror = ref 0 in
  let _ = watch (fun () -> mirror := get s) in
  flush_sync (fun () -> set s 42);
  !mirror = 42

(* ──── memo ──── *)

(* a memo caches between reads (f runs at most once per change) and recomputes once per batch even
   when several deps change together; downstream observers see the fresh value. *)
let%test "memo caches between reads and recomputes once per batch" =
  let a = signal 2 and b = signal 3 in
  let calls = ref 0 in
  let m = memo (fun () -> incr calls; get a + get b) in
  let seed_calls = !calls in                       (* 1: seeded at construction *)
  let r1 = peek m and r2 = peek m and r3 = peek m in
  let cached = !calls in                           (* still seed_calls — reads don't recompute *)
  flush_sync (fun () -> set a 10; set b 20);        (* two deps in one batch → ONE recompute *)
  let after_batch_calls = !calls in
  seed_calls = 1 && r1 = 5 && r2 = 5 && r3 = 5 && cached = 1
  && peek m = 30 && after_batch_calls = 2

(* a memo is glitch-free with batching: an effect that reads the memo sees the post-batch value
   once, not an intermediate value per dep write. *)
let%test "memo is glitch-free: observer sees one consistent update per batch" =
  let a = signal 1 and b = signal 1 in
  let m = memo (fun () -> get a + get b) in
  let observed = ref [] in
  let _ = watch (fun () -> observed := get m :: !observed) in   (* initial run records 2 *)
  flush_sync (fun () -> set a 5; set b 5);                       (* one update to 10 *)
  (* the observer ran once at creation (2) and once for the batched memo change (10) — never on a
     half-applied intermediate like 6 *)
  !observed = [ 10; 2 ]

(* ──── hydration tag-validation (SSR/CSR drift recovery) ──── *)

(* helper: a Fake DOM element with the given tag, parented under a fresh container, as if SSR
   emitted it — the adoptee hydrate will be handed *)
let drifted_container child = let c = Fake.create_element "root" in Fake.append c child; c

(* a <div> vnode hydrating against an SSR <span> must NOT adopt the span and then patch the wrong
   node — it creates the div fresh and swaps it in. Pre-fix, hydrate adopted childNodes[i] with no
   tag check, so the live node would be a <span> wearing div attributes. *)
let%test "hydrate recovers from element tag drift (div vnode vs span dom)" =
  let container = drifted_container (Fake.create_element "span") in
  let _ = D.mount_root container (fun () -> h "div" [ attr "id" "x" ] [ text "hi" ]) in
  (match container.Fake.kids with
   | [ only ] -> only.Fake.tag = Some "div" && List.assoc_opt "id" only.Fake.attrs = Some "x"
   | _ -> false)

(* a text vnode hydrating against an SSR element must recover to a real text node (not adopt the
   element and call set_text on it). *)
let%test "hydrate recovers from text-vs-element drift" =
  let container = drifted_container (Fake.create_element "p") in
  let _ = D.mount_root container (fun () -> text "plain") in
  (match container.Fake.kids with
   | [ only ] -> Fake.is_text only && only.Fake.text = "plain"
   | _ -> false)

(* an element vnode hydrating against an SSR text node likewise recovers to a real element. *)
let%test "hydrate recovers from element-vs-text drift" =
  let container = drifted_container (Fake.create_text "stray") in
  let _ = D.mount_root container (fun () -> h "section" [] [ text "ok" ]) in
  (match container.Fake.kids with
   | [ only ] -> only.Fake.tag = Some "section"
   | _ -> false)

(* a correct outer shell with a DRIFTED nested child: the parent is adopted in place, only the
   wrong child is recovered locally — drift recovery is surgical, not a whole-subtree teardown. *)
let%test "hydrate recovers a drifted nested child while keeping the matching parent" =
  let inner_wrong = Fake.create_element "b" in
  let outer = Fake.create_element "ul" in
  Fake.append outer inner_wrong;
  let container = drifted_container outer in
  let adopted_outer = List.hd container.Fake.kids in
  let _ = D.mount_root container (fun () -> h "ul" [] [ h "li" [] [ text "1" ] ]) in
  let outer' = List.hd container.Fake.kids in
  (* same parent node object adopted (identity preserved), but its child is now an <li>, not <b> *)
  outer' == adopted_outer && outer'.Fake.tag = Some "ul"
  && (match outer'.Fake.kids with [ li ] -> li.Fake.tag = Some "li" | _ -> false)

(* a MATCHING shell still adopts in place (no needless re-create): the same node object is reused,
   its text patched — the drift guard must not regress the happy path. *)
let%test "hydrate still adopts a matching node in place" =
  let p = Fake.create_element "p" in
  Fake.append p (Fake.create_text "old");
  let container = drifted_container p in
  let adopted = List.hd container.Fake.kids in
  let _ = D.mount_root container (fun () -> h "p" [] [ text "old" ]) in
  let p' = List.hd container.Fake.kids in
  p' == adopted && p'.Fake.tag = Some "p"

(* per-request Head isolation: each render context (a [Platform.with_data_context] binding — fiber-local
   on the concurrent server, the reset fallback outside Eio) gets its OWN Head registry, so one render's
   <title>/meta can never leak into the next. Unit-level proof of the server fix; the http suite proves
   it end-to-end (a no-title /hello after a titled /). *)
let%test "Head registry is per-context — tags do not leak across renders" =
  let sub h n =
    let nh = String.length h and nn = String.length n in
    let rec go i = i + nn <= nh && (String.sub h i nn = n || go (i + 1)) in
    nn = 0 || go 0
  in
  let h1 = Platform.with_data_context (fun () -> Head.title "Page A"; Head.to_ssr ()) in
  let h2 = Platform.with_data_context (fun () -> Head.title "Page B"; Head.to_ssr ()) in
  let h3 = Platform.with_data_context (fun () -> Head.to_ssr ()) in
  sub h1 "Page A"
  && (not (sub h1 "Page B"))
  && sub h2 "Page B"
  && (not (sub h2 "Page A"))  (* B's render must not see A's title *)
  && (not (sub h3 "Page A"))
  && (not (sub h3 "Page B"))  (* a no-title render is empty — the exact /hello leak, guarded *)

(* per-request Router isolation: the SSR driver activates a per-request CLONE of the app router (its own
   path signal), so two concurrent requests to one app never share the current path (a wrong-route
   hazard). A later render must start fresh, not inherit the prior render's path. *)
let%test "Router active app + path are per-context — a fresh render does not inherit the prior route" =
  let r = Router.make ~base:"" () in
  let _ =
    Platform.with_data_context (fun () ->
        Router.activate (Router.clone_for_render r);
        Router.set_path (Router.current ()) "/was-here")
  in
  let fresh =
    Platform.with_data_context (fun () ->
        Router.activate (Router.clone_for_render r);
        Router.current_path (Router.current ()))
  in
  fresh = "/"
