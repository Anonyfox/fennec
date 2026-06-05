(* A Host-header pattern, parsed into a total 3-way shape so request-time matching never re-parses,
   and so overlapping patterns order deterministically. A pattern is exact ("acme.com"), a single
   leading-wildcard suffix ("*.acme.com" — one or more leading labels), or the catch-all "*".
   {!of_string} classifies once and rejects every malformed form; {!matches} is then a total match
   over the variant; {!specificity} ranks them like a router (exact > longer suffix > shorter
   suffix > catch-all) so {!Host_router} resolves overlaps by precision, not declaration order. *)

type t =
  | Exact of string (* a full normalized host: "acme.com" *)
  | Suffix of string (* the dot-prefixed tail of "*.x": ".acme.com" — matches >=1 leading label *)
  | Any (* "*": matches every host; at most one may exist in a router *)

(* lowercase + trim + drop a single trailing FQDN "." (shared by host + pattern normalization) *)
let fold s =
  let s = String.trim s in
  let s = if String.length s > 1 && s.[String.length s - 1] = '.' then String.sub s 0 (String.length s - 1) else s in
  String.lowercase_ascii s

(* a request Host: like {!fold}, but also strips a ":port" suffix *)
let normalize (host : string) : string =
  let h = match String.index_opt host ':' with Some i -> String.sub host 0 i | None -> host in
  fold h

(* drop a single trailing FQDN "." (only when something remains before it) *)
let drop_dot s = if String.length s > 1 && s.[String.length s - 1] = '.' then String.sub s 0 (String.length s - 1) else s

let of_string (raw : string) : (t, string) result =
  (* classify on the trimmed/lowercased string FIRST, then drop a trailing dot per-case — so a bare
     "*." is not silently folded into the catch-all "*" *)
  let s = String.lowercase_ascii (String.trim raw) in
  let bad msg = Error (Printf.sprintf "invalid host pattern %S: %s" raw msg) in
  if s = "" then bad "empty"
  else if s = "*" then Ok Any
  else if String.contains s ' ' then bad "contains whitespace"
  else if String.length s >= 2 && s.[0] = '*' && s.[1] = '.' then
    (* wildcard suffix "*.rest" — the only legal wildcard form *)
    let rest = drop_dot (String.sub s 1 (String.length s - 1)) (* ".rest" *) in
    if rest = "." || String.length rest < 2 || String.contains rest '*' then bad "'*.' needs a domain after it, e.g. \"*.example.com\""
    else Ok (Suffix rest)
  else if String.contains s '*' then bad "'*' is only allowed alone (\"*\") or as a single leading label (\"*.example.com\")"
  else Ok (Exact (drop_dot s))

let to_string = function Exact h -> h | Suffix suf -> "*" ^ suf | Any -> "*"

let matches (t : t) ~(host : string) : bool =
  let h = normalize host in
  match t with
  | Any -> true
  | Exact e -> h = e
  | Suffix suf ->
    let sl = String.length suf and hl = String.length h in
    hl > sl && String.sub h (hl - sl) sl = suf

(* higher = more specific: any Exact outranks every Suffix; among suffixes the longer (more
   labels) wins; Any is the floor. A host name caps well under the Exact constant. *)
let specificity = function Exact _ -> 1_000_000 | Suffix suf -> String.length suf | Any -> 0
