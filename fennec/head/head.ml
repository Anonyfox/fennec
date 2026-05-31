(* Document <head> metadata — the PURE core. No React, no IO: just the tag model,
   the dedup+merge (inside-out / last-wins), HTML escaping, and rendering the
   merged set to an HTML string. Compiles to BOTH targets (native SSR + Melange
   CSR), so the SAME merge runs on the server and the client — guaranteeing the
   server-emitted head equals the client-computed head (no hydration flash).

   Precedence: React renders parent -> child depth-first, and a <Head> registers
   during its render, so children register AFTER parents. Therefore "last write
   wins per dedup key" == "innermost/deepest wins" — exactly the inside-out
   behavior we want, with no explicit depth tracking. *)

type tag =
  | Title of string
  | Charset of string
  | Meta_name of string * string (* <meta name=.. content=..> e.g. description *)
  | Meta_property of string * string (* <meta property=.. content=..> e.g. og:* *)
  | Canonical of string (* <link rel="canonical" href=..> *)
  | Link of (string * string) list (* arbitrary <link>, by attrs *)
  | Meta of (string * string) list (* arbitrary <meta>, by attrs *)

(* The dedup key. SINGLE-valued tags (title, charset, canonical, a given
   meta-name/property) collapse to one. Repeatable tags (alternate links, etc.)
   get a key from their identifying attrs so distinct ones coexist but exact dups
   collapse. *)
let key (t : tag) : string =
  match t with
  | Title _ -> "title"
  | Charset _ -> "charset"
  | Canonical _ -> "link:canonical"
  | Meta_name (n, _) -> "meta:name:" ^ n
  | Meta_property (p, _) -> "meta:property:" ^ p
  | Link attrs ->
    let rel = try List.assoc "rel" attrs with Not_found -> "" in
    let href = try List.assoc "href" attrs with Not_found -> "" in
    if rel = "" && href = "" then
      (* no identity: key on the whole attr set so identical links dedup *)
      "link:" ^ String.concat ";" (List.map (fun (k, v) -> k ^ "=" ^ v) attrs)
    else "link:" ^ rel ^ ":" ^ href
  | Meta attrs ->
    let name =
      try List.assoc "name" attrs
      with Not_found -> ( try List.assoc "property" attrs with Not_found -> "")
    in
    if name = "" then "meta:" ^ String.concat ";" (List.map (fun (k, v) -> k ^ "=" ^ v) attrs)
    else "meta:" ^ name

(* Merge a list of tags, last-write-wins per key, output in order of each key's
   FIRST appearance (stable, so title stays where it was first declared) but with
   the LAST value. This is deterministic -> SSR and CSR agree. *)
let merge (tags : tag list) : tag list =
  (* first pass: record order of first appearance per key *)
  let order = Hashtbl.create 16 in
  let seq = ref 0 in
  List.iter
    (fun t ->
      let k = key t in
      if not (Hashtbl.mem order k) then (
        Hashtbl.add order k !seq;
        incr seq))
    tags;
  (* second pass: last value per key *)
  let last = Hashtbl.create 16 in
  List.iter (fun t -> Hashtbl.replace last (key t) t) tags;
  (* emit in first-appearance order *)
  Hashtbl.fold (fun k t acc -> (Hashtbl.find order k, t) :: acc) last []
  |> List.sort (fun (a, _) (b, _) -> compare a b)
  |> List.map snd

(* ---- HTML escaping ---- *)

(* escape a value for an HTML double-quoted attribute *)
let attr_escape (s : string) : string =
  let b = Buffer.create (String.length s + 8) in
  String.iter
    (fun c ->
      match c with
      | '"' -> Buffer.add_string b "&quot;"
      | '&' -> Buffer.add_string b "&amp;"
      | '<' -> Buffer.add_string b "&lt;"
      | '>' -> Buffer.add_string b "&gt;"
      | '\'' -> Buffer.add_string b "&#x27;"
      | c -> Buffer.add_char b c)
    s;
  Buffer.contents b

(* escape text content (e.g. inside <title>) *)
let text_escape (s : string) : string =
  let b = Buffer.create (String.length s + 8) in
  String.iter
    (fun c ->
      match c with
      | '&' -> Buffer.add_string b "&amp;"
      | '<' -> Buffer.add_string b "&lt;"
      | '>' -> Buffer.add_string b "&gt;"
      | c -> Buffer.add_char b c)
    s;
  Buffer.contents b

let attrs_to_html (attrs : (string * string) list) : string =
  String.concat ""
    (List.map (fun (k, v) -> Printf.sprintf " %s=\"%s\"" (attr_escape k) (attr_escape v)) attrs)

(* render ONE tag to its HTML element string *)
let tag_to_html (t : tag) : string =
  match t with
  | Title s -> Printf.sprintf "<title>%s</title>" (text_escape s)
  | Charset c -> Printf.sprintf "<meta charset=\"%s\"/>" (attr_escape c)
  | Meta_name (n, c) ->
    Printf.sprintf "<meta name=\"%s\" content=\"%s\"/>" (attr_escape n) (attr_escape c)
  | Meta_property (p, c) ->
    Printf.sprintf "<meta property=\"%s\" content=\"%s\"/>" (attr_escape p) (attr_escape c)
  | Canonical href -> Printf.sprintf "<link rel=\"canonical\" href=\"%s\"/>" (attr_escape href)
  | Link attrs -> Printf.sprintf "<link%s/>" (attrs_to_html attrs)
  | Meta attrs -> Printf.sprintf "<meta%s/>" (attrs_to_html attrs)

(* render the MERGED head to an HTML string (for native SSR injection) *)
let to_html (tags : tag list) : string =
  String.concat "" (List.map tag_to_html (merge tags))

(* Build a tag list from the <Head> component's typed props + an escape-hatch
   [extra] list of raw tags. Shared by both targets so the prop->tag mapping is
   identical on server and client. *)
let of_props ?title ?description ?canonical ?(extra = []) () : tag list =
  (match title with Some t -> [ Title t ] | None -> [])
  @ (match description with Some d -> [ Meta_name ("description", d) ] | None -> [])
  @ (match canonical with Some c -> [ Canonical c ] | None -> [])
  @ extra

(* ---- accessors the CSR runtime uses to apply tags to the live document ---- *)

(* the merged title, if any (CSR sets document.title from this) *)
let title_of (tags : tag list) : string option =
  List.fold_left (fun acc t -> match t with Title s -> Some s | _ -> acc) None tags
