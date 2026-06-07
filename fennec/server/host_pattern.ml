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

(* ──── of_string ──── *)

let of_string (raw : string) : (t, string) result =
  let s = String.lowercase_ascii (String.trim raw) in
  let bad msg = Error (Printf.sprintf "invalid host pattern %S: %s" raw msg) in
  if s = "" then bad "empty"
  else if s = "*" then Ok Any
  else if String.contains s ' ' then bad "contains whitespace"
  else if String.length s >= 2 && s.[0] = '*' && s.[1] = '.' then
    let rest = drop_dot (String.sub s 1 (String.length s - 1)) in
    if rest = "." || String.length rest < 2 || String.contains rest '*' then bad "'*.' needs a domain after it, e.g. \"*.example.com\""
    else Ok (Suffix rest)
  else if String.contains s '*' then bad "'*' is only allowed alone (\"*\") or as a single leading label (\"*.example.com\")"
  else Ok (Exact (drop_dot s))

let%test "exact"                      = of_string "acme.com" = Ok (Exact "acme.com")
let%test "exact lowercases"           = of_string "ACME.com" = Ok (Exact "acme.com")
let%test "exact strips trailing dot"  = of_string "acme.com." = Ok (Exact "acme.com")
let%test "suffix"                     = of_string "*.acme.com" = Ok (Suffix ".acme.com")
let%test "catch-all"                  = of_string "*" = Ok Any
let%test "empty → error"              = Result.is_error (of_string "")
let%test "whitespace-only → error"    = Result.is_error (of_string "   ")
let%test "internal space → error"     = Result.is_error (of_string "ac me.com")
let%test "'*.' alone → error"         = Result.is_error (of_string "*.")
let%test "mid '*' → error"            = Result.is_error (of_string "a*b")
let%test "'*foo' (not '*.') → error"  = Result.is_error (of_string "*foo")
let%test "double wildcard → error"    = Result.is_error (of_string "*.*.com")

(* ──── to_string ──── *)

let to_string = function Exact h -> h | Suffix suf -> "*" ^ suf | Any -> "*"

(* ──── matches ──── *)

let matches (t : t) ~(host : string) : bool =
  let h = normalize host in
  match t with
  | Any -> true
  | Exact e -> h = e
  | Suffix suf ->
    let sl = String.length suf and hl = String.length h in
    hl > sl && String.sub h (hl - sl) sl = suf

let%test "exact matches"              = matches (Result.get_ok (of_string "acme.com")) ~host:"acme.com"
let%test "exact matches w/ :port"     = matches (Result.get_ok (of_string "acme.com")) ~host:"acme.com:8020"
let%test "exact case-insensitive"     = matches (Result.get_ok (of_string "acme.com")) ~host:"ACME.COM"
let%test "exact rejects other"        = not (matches (Result.get_ok (of_string "acme.com")) ~host:"other.com")
let%test "suffix matches subdomain"   = matches (Result.get_ok (of_string "*.acme.com")) ~host:"api.acme.com"
let%test "suffix matches deep"        = matches (Result.get_ok (of_string "*.acme.com")) ~host:"a.b.acme.com"
let%test "suffix needs ≥1 label"      = not (matches (Result.get_ok (of_string "*.acme.com")) ~host:"acme.com")
let%test "suffix rejects other base"  = not (matches (Result.get_ok (of_string "*.acme.com")) ~host:"api.other.com")
let%test "any matches anything"       = matches Any ~host:"whatever.com"
let%test "any matches empty host"     = matches Any ~host:""
let%test "exact rejects empty host"   = not (matches (Result.get_ok (of_string "acme.com")) ~host:"")

(* ──── specificity ──── *)

(* higher = more specific: any Exact outranks every Suffix; among suffixes the longer (more
   labels) wins; Any is the floor. A host name caps well under the Exact constant. *)
let specificity = function Exact _ -> 1_000_000 | Suffix suf -> String.length suf | Any -> 0

let%test "exact > suffix"             = specificity (Result.get_ok (of_string "acme.com")) > specificity (Result.get_ok (of_string "*.acme.com"))
let%test "longer suffix > shorter"    = specificity (Result.get_ok (of_string "*.api.acme.com")) > specificity (Result.get_ok (of_string "*.acme.com"))
let%test "suffix > any"               = specificity (Result.get_ok (of_string "*.acme.com")) > specificity (Result.get_ok (of_string "*"))
let%test "exact > any"                = specificity (Result.get_ok (of_string "acme.com")) > specificity (Result.get_ok (of_string "*"))

(* ──── properties ──── *)

(* normalization is a fixpoint: re-normalizing a host never changes it (and never raises on
   arbitrary input — host headers are attacker-controlled). *)
let%prop "normalize is idempotent" = fun (h : string) -> normalize (normalize h) = normalize h

(* parsing a pattern and rendering it back round-trips exactly. Generates valid patterns from
   lowercase dotted labels (a suffix needs ≥2 labels so its remainder clears the 2-char floor). *)
let%prop "of_string then to_string round-trips a valid pattern" =
  let open Fennec_hunt_prop in
  let label = Gen.(string_size ~gen:(map (fun n -> Char.chr (Char.code 'a' + n)) (int_range 0 25)) (int_range 1 5)) in
  let dotted ~min = Gen.(map (String.concat ".") (list_size (int_range min 3) label)) in
  let pat = Gen.(oneof [ pure Any;
                         map (fun h -> Exact h) (dotted ~min:1);
                         map (fun h -> Suffix ("." ^ h)) (dotted ~min:2) ]) in
  forall ~print:to_string pat (fun p -> of_string (to_string p) = Ok p)
