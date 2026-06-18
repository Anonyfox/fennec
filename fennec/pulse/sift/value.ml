(* value.ml — Sift's NEUTRAL data model: the format-agnostic interchange between a typed value and
   any wire format (BSON, JSON; later XML, CSV). The serde "data model" — one ADT every format reads
   into and writes out of, so a shape is interpreted ONCE and reused across formats.

   RICH by design: a LOSSLESS superset of every value a document store needs, so it can fully replace
   a format-specific tree (it carries dates, ids, decimals, … as their own neutral constructors, not
   as stringly-typed approximations). The basic types (Null/Bool/Int/Int64/Float/String/Bytes/List/
   Assoc) cover JSON and the common case; the semantic constructors below (Date/Id/Decimal/Timestamp/
   Regex/Symbol/Code/Min/Max) carry the richness BSON has and JSON lacks. [Assoc] is ORDERED (key
   order preserved). Pure: NO Bson, NO platform deps — js_of_ocaml-safe, shared verbatim by server and
   browser. The BSON interop is the separate {!Value_bson} adapter; this module never mentions Bson.

   Efficiency: this is NOT the hot path (the zero-copy BSON byte codecs are shape-directed over raw
   bytes and never materialise a Value); the common case stays a native [int] / plain string, and only
   the rare rich value pays for its constructor. *)

type t =
  | Null
  | Bool of bool
  | Int of int (* a 32-bit-range integer (native int) *)
  | Int64 of int64 (* a full 64-bit integer *)
  | Float of float
  | String of string
  | Bytes of string (* opaque binary (generic subtype) *)
  | List of t list
  | Assoc of (string * t) list (* an ordered object/document *)
  (* ---- rich semantic scalars — the lossless superset (what JSON lacks and BSON/stores need) ---- *)
  | Date of int64 (* milliseconds since the Unix epoch *)
  | Id of string (* an object id — 24-char hex *)
  | Decimal of string (* a high-precision decimal, canonical string form *)
  | Timestamp of int * int (* a logical clock: (seconds, ordinal) *)
  | Regex of string * string (* (pattern, options) *)
  | Symbol of string
  | Code of string (* code/function source *)
  | Min (* sort-order sentinels (below / above every other value) *)
  | Max

(* ---- structural equality / ordering (monomorphic — not OCaml's polymorphic runtime ops) ---------
   Literal/structural: an [Assoc] compares key-by-key IN ORDER. Semantic record equality (field-by-
   field, order-insensitive) is the shape-level {!Sift.equal}; this is the plain data equality. *)

let rec equal (a : t) (b : t) : bool =
  match (a, b) with
  | Null, Null -> true
  | Bool x, Bool y -> Bool.equal x y
  | Int x, Int y -> Int.equal x y
  | Int64 x, Int64 y -> Int64.equal x y
  | Float x, Float y -> Float.equal x y
  | String x, String y -> String.equal x y
  | Bytes x, Bytes y -> String.equal x y
  | List xs, List ys -> ( try List.for_all2 equal xs ys with Invalid_argument _ -> false)
  | Assoc xs, Assoc ys -> equal_assoc xs ys
  | Date x, Date y -> Int64.equal x y
  | Id x, Id y -> String.equal x y
  | Decimal x, Decimal y -> String.equal x y
  | Timestamp (a1, b1), Timestamp (a2, b2) -> a1 = a2 && b1 = b2
  | Regex (p1, o1), Regex (p2, o2) -> String.equal p1 p2 && String.equal o1 o2
  | Symbol x, Symbol y -> String.equal x y
  | Code x, Code y -> String.equal x y
  | Min, Min | Max, Max -> true
  | _ -> false

and equal_assoc xs ys =
  match (xs, ys) with
  | [], [] -> true
  | (k1, v1) :: xr, (k2, v2) :: yr -> String.equal k1 k2 && equal v1 v2 && equal_assoc xr yr
  | _ -> false

let rank = function
  | Min -> 0
  | Null -> 1
  | Bool _ -> 2
  | Int _ -> 3
  | Int64 _ -> 4
  | Float _ -> 5
  | Decimal _ -> 6
  | Date _ -> 7
  | Timestamp _ -> 8
  | String _ -> 9
  | Symbol _ -> 10
  | Code _ -> 11
  | Id _ -> 12
  | Bytes _ -> 13
  | Regex _ -> 14
  | List _ -> 15
  | Assoc _ -> 16
  | Max -> 17

let rec compare (a : t) (b : t) : int =
  match (a, b) with
  | Null, Null -> 0
  | Bool x, Bool y -> Bool.compare x y
  | Int x, Int y -> Int.compare x y
  | Int64 x, Int64 y -> Int64.compare x y
  | Float x, Float y -> Float.compare x y
  | String x, String y -> String.compare x y
  | Bytes x, Bytes y -> String.compare x y
  | List xs, List ys -> compare_list xs ys
  | Assoc xs, Assoc ys -> compare_assoc xs ys
  | Date x, Date y -> Int64.compare x y
  | Id x, Id y -> String.compare x y
  | Decimal x, Decimal y -> String.compare x y
  | Timestamp (a1, b1), Timestamp (a2, b2) -> let c = Int.compare a1 a2 in if c <> 0 then c else Int.compare b1 b2
  | Regex (p1, o1), Regex (p2, o2) -> let c = String.compare p1 p2 in if c <> 0 then c else String.compare o1 o2
  | Symbol x, Symbol y -> String.compare x y
  | Code x, Code y -> String.compare x y
  | Min, Min | Max, Max -> 0
  | _ -> Int.compare (rank a) (rank b)

and compare_list xs ys =
  match (xs, ys) with
  | [], [] -> 0
  | [], _ -> -1
  | _, [] -> 1
  | x :: xr, y :: yr -> let c = compare x y in if c <> 0 then c else compare_list xr yr

and compare_assoc xs ys =
  match (xs, ys) with
  | [], [] -> 0
  | [], _ -> -1
  | _, [] -> 1
  | (k1, v1) :: xr, (k2, v2) :: yr ->
      let c = String.compare k1 k2 in
      if c <> 0 then c
      else let c = compare v1 v2 in if c <> 0 then c else compare_assoc xr yr

(* ---- lookup + a readable debug rendering (not a serializer — that is a Format) ------------------ *)

(* field lookup in an [Assoc] (first match), [None] for a non-object or absent key *)
let get (key : string) (v : t) : t option = match v with Assoc kvs -> List.assoc_opt key kvs | _ -> None

let rec pp (fmt : Format.formatter) (v : t) : unit =
  match v with
  | Null -> Format.pp_print_string fmt "null"
  | Bool b -> Format.pp_print_bool fmt b
  | Int n -> Format.pp_print_int fmt n
  | Int64 n -> Format.fprintf fmt "%Ld" n
  | Float f -> Format.fprintf fmt "%g" f
  | String s -> Format.fprintf fmt "%S" s
  | Bytes b -> Format.fprintf fmt "bytes(%d)" (String.length b)
  | Date d -> Format.fprintf fmt "date(%Ld)" d
  | Id s -> Format.fprintf fmt "id(%s)" s
  | Decimal s -> Format.fprintf fmt "decimal(%s)" s
  | Timestamp (t, i) -> Format.fprintf fmt "ts(%d,%d)" t i
  | Regex (p, o) -> Format.fprintf fmt "/%s/%s" p o
  | Symbol s -> Format.fprintf fmt "symbol(%s)" s
  | Code s -> Format.fprintf fmt "code(%s)" s
  | Min -> Format.pp_print_string fmt "min"
  | Max -> Format.pp_print_string fmt "max"
  | List xs -> Format.fprintf fmt "[%a]" (Format.pp_print_list ~pp_sep:(fun fmt () -> Format.pp_print_string fmt ", ") pp) xs
  | Assoc kvs ->
      Format.fprintf fmt "{%a}"
        (Format.pp_print_list ~pp_sep:(fun fmt () -> Format.pp_print_string fmt ", ") (fun fmt (k, v) -> Format.fprintf fmt "%S: %a" k pp v))
        kvs

let to_string (v : t) : string = Format.asprintf "%a" pp v
