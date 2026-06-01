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

  (* ergonomic constructors so call sites read like markup but stay typed data *)
  let title s = Title s
  let meta ~name content = Meta [ ("name", name); ("content", content) ]
  let og property content = Meta [ ("property", property); ("content", content) ]
  let link ~rel ?(attrs = []) href = Link (("rel", rel) :: ("href", href) :: attrs)
  let script ?(attrs = []) ?(body = "") () = Script (attrs, body)
  let json_ld j = Json_ld j

  (* the registry: ordered (source-id, tags); a later source overrides an earlier *)
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
    run_effect eff

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
