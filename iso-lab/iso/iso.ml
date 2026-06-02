(* Platform-agnostic core: signals + vnode + SSR. The state model is uniform:
   a signal in a component's setup is LOCAL (per-instance); a signal in a shared
   module is GLOBAL. get subscribes, set/update notify. One primitive, scoped by
   where you define it. *)
type reaction = { run : unit -> unit; mutable deps : psig list }
and 'a signal = { mutable v : 'a; mutable subs : reaction list }
and psig = P : 'a signal -> psig

let current : reaction option ref = ref None
let signal v = { v; subs = [] }
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
let set s v = if compare v s.v <> 0 then (s.v <- v; List.iter run_effect (List.rev s.subs))
let update s f = set s (f (peek s))
let dispose e =  (* unmount: unsubscribe from everything so it never re-runs *)
  List.iter (fun (P s) -> s.subs <- List.filter (fun e' -> e' != e) s.subs) e.deps;
  e.deps <- []

(* platform flag: the client entrypoint flips this true; native SSR leaves it false *)
let is_browser = ref false

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

(* on_mount: a browser-only side effect (à la Vue's onMounted / React useEffect[]).
   Registered during setup, run once AFTER the initial client render adopts the SSR
   DOM. A no-op on the server, so SSR never executes browser-only handlers. *)
let mount_queue : (unit -> unit) list ref = ref []
let on_mount f = if !is_browser then mount_queue := f :: !mount_queue
let flush_mounts () =
  let q = List.rev !mount_queue in
  mount_queue := [];
  List.iter (fun f -> f ()) q

(* Effect scope: cleanups registered during a component's setup/first render are
   tied to THAT instance and run on its unmount. The DOM runtime points
   [current_cleanups] at the mounting instance's accumulator (save/restore per
   instance, so nested children scope correctly). Used by Head.use to remove its
   head contribution on unmount, by subscriptions, etc. A no-op container on the
   server (SSR never unmounts). *)
let current_cleanups : (unit -> unit) list ref ref = ref (ref [])
let on_cleanup f = let r = !current_cleanups in r := f :: !r

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
let node (x : 'a) : vnode =
  let r = Obj.repr x in
  if Obj.is_int r then Text (string_of_int (Obj.magic x))
  else if Obj.tag r = Obj.string_tag then Text (Obj.magic x)
  else if Obj.tag r = Obj.double_tag then Text (string_of_float (Obj.magic x))
  else (Obj.magic x : vnode)
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
let rec to_html = function
  | Text s -> escape s
  | Raw s -> s
  | Fragment l -> String.concat "" (List.map to_html l)
  | Comp c -> to_html ((c.setup ()) ())   (* SSR: run setup + render once, no reactivity *)
  | Elem { tag; attrs; children; _ } ->
    let a = List.filter_map (function
      | Attr (k,v) -> Some (Printf.sprintf " %s=\"%s\"" k (escape v)) | Handler _ -> None) attrs
      |> String.concat "" in
    if is_void tag then Printf.sprintf "<%s%s/>" tag a
    else Printf.sprintf "<%s%s>%s</%s>" tag a (String.concat "" (List.map to_html (flatten children))) tag

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
   emits each resolved tag with data-ih="<content-key>"; the client reconciles
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

  (* the registry: ordered (source-id, tags); a later source overrides an earlier *)
  (* IMPORTANT: per-request state — must become fiber-local on the concurrent server *)
  let sources : (int * tag list) list signal = signal []
  let counter = ref 0

  (* Register a reactive contribution. Call ONCE per instance, in setup (it allocates
     a stable slot id). The effect recomputes [f] whenever a signal it read changes. *)
  let use (f : unit -> tag list) : unit =
    let id = !counter in
    incr counter;
    let eff =
      { run = (fun () ->
          let tags = f () in
          let cur = peek sources in
          set sources
            (if List.mem_assoc id cur
             then List.map (fun (i, t) -> if i = id then (i, tags) else (i, t)) cur
             else cur @ [ (id, tags) ]));
        deps = [] }
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
    resolve (peek sources)
    |> List.map (fun t ->
        let k = tag_key t in
        match t with
        | Title s -> Printf.sprintf "<title data-ih=\"%s\">%s</title>" k (escape s)
        | Meta a -> Printf.sprintf "<meta data-ih=\"%s\"%s>" k (attrs_str a)
        | Link a -> Printf.sprintf "<link data-ih=\"%s\"%s>" k (attrs_str a)
        | Script (a, b) -> Printf.sprintf "<script data-ih=\"%s\"%s>%s</script>" k (attrs_str a) b
        | Json_ld j -> Printf.sprintf "<script data-ih=\"%s\" type=\"application/ld+json\">%s</script>" k j)
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
   - CLIENT: [seed] is loaded from window.__ISO_DATA__. A resource whose key is
     present resolves SYNCHRONOUSLY (no fetch, no loading flash, hydration matches),
     and the entry is consumed so later/dynamic fetches hit the network for real.

   The [source] hook is the only platform split (the SOURCE functor, as a ref): on
   the server it forks an Eio fiber; on the client it does a real fetch. *)
module Data = struct
  type 'a state = Loading | Ready of 'a | Failed of string
  type 'a t = { st : 'a state signal; key : string; decode : string -> 'a; fallback : 'a }

  (* key -> raw payload string. Client: seeded from __ISO_DATA__, consumed once.
     Server: the per-request data context the driver fills + serializes. *)
  (* IMPORTANT: per-request state — [seed] AND [source] below are the request's data
     context + fetch strategy; both must be fiber-local on the concurrent server (two
     requests fetching different data would otherwise share one table). *)
  let seed : (string, string) Hashtbl.t = Hashtbl.create 16
  let put_seed k v = Hashtbl.replace seed k v
  let take_seed k =
    match Hashtbl.find_opt seed k with
    | None -> None
    | Some v -> if !is_browser then Hashtbl.remove seed k; Some v  (* client consumes; server keeps for pass 2 + embed *)
  let clear_seed () = Hashtbl.clear seed

  (* platform SOURCE: deliver a key's raw payload to a continuation. Default no-op
     (overridden by the server driver / the client fetch binding). *)
  let source : (string -> (string -> unit) -> unit) ref = ref (fun _ _ -> ())

  let resource ~key ?(client_only = false) ~fallback ~decode () =
    let initial, fetch_now =
      match take_seed key with
      | Some json -> (Ready (decode json), false)              (* seed/ctx hit: synchronous *)
      | None -> (Loading, not (client_only && not !is_browser)) (* server skips browser-only data *)
    in
    let st = signal initial in
    if fetch_now then !source key (fun json -> set st (Ready (decode json)));
    { st; key; decode; fallback }

  (* common case: string payload (no decoder). [Data.string "/api/x" ~fallback:"…" ()] *)
  let string key ?(fallback = "") ?(client_only = false) () =
    resource ~key ~client_only ~fallback ~decode:Fun.id ()

  (* reactive readers (each subscribes via get) *)
  let status r = get r.st
  let value r = match get r.st with Ready v -> v | _ -> r.fallback  (* fallback until ready *)
  let loading r = match get r.st with Loading -> true | _ -> false
  let error r = match get r.st with Failed e -> Some e | _ -> None
  (* an explicit, dynamic refetch always hits the network (bypasses the seed) *)
  let refetch r = set r.st Loading; !source r.key (fun json -> set r.st (Ready (r.decode json)))

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
    let pairs = Hashtbl.fold (fun k v acc -> (js_string k ^ ":" ^ js_string v) :: acc) seed [] in
    "window.__ISO_DATA__={" ^ String.concat "," pairs ^ "}"
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

  (* the ACTIVE app for this render — lets pages/components reach the router (for p,
     param, href) without importing it, so there's no instance/registration cycle *)
  let active : t option ref = ref None
  let activate t = active := Some t
  let current () = match !active with Some t -> t | None -> failwith "Router: no active app (call activate)"
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

  (* navigation hook: client overrides with pushState + set_path; server no-op *)
  let nav_hook : (string -> unit) ref = ref (fun _ -> ())
  let navigate abs = !nav_hook abs

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
  let outlet c = raw c.body                                 (* the SSR'd app body *)
  let scripts c = raw (Printf.sprintf "<script>%s</script><script>%s</script>" c.data c.client_js)
end
