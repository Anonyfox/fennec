(* value.ml — Sift's NEUTRAL data model: the format-agnostic interchange between a typed value and
   any wire format (BSON, JSON; later XML, CSV). This is the serde "data model" — a small, total ADT
   every format reads into and writes out of, so one shape is interpreted ONCE and reused across
   formats instead of each format re-walking the GADT.

   Deliberately LEAN (eight constructors): richness lives at the SHAPE level, not here. A date is an
   [int64] leaf, an id a string leaf, an objectid a string — the shape reconstructs the precise wire
   type on encode. The data model keeps only the common denominator BSON, JSON, XML and CSV all share,
   so a new format never has to grow it. [Bytes] is opaque binary; [Assoc] is an ORDERED object
   (key order preserved — BSON documents are ordered, and JSON round-trips cleaner for it).

   Pure: NO Bson, NO platform deps — js_of_ocaml-safe, shared verbatim by server and browser. The
   BSON interop is the separate {!Value_bson} adapter (it is what moves out when Sift goes
   dependency-free); this module never mentions Bson. *)

type t =
  | Null
  | Bool of bool
  | Int of int (* 63-bit native; int64/date precision is a shape-level concern, not a data-model one *)
  | Float of float
  | String of string
  | Bytes of string (* opaque binary — a BSON Binary payload, a blob *)
  | List of t list
  | Assoc of (string * t) list (* an ordered object/document; key order is preserved *)

(* ---- structural equality / ordering (monomorphic — not OCaml's polymorphic runtime ops) ---------
   Literal/structural: an [Assoc] compares key-by-key IN ORDER (the model is ordered), so it is the
   shape-level {!Sift.equal} that decides semantic record equality field-by-field; this is the plain
   data equality forms/tests/diff build on. Floats use the total {!Float} order (nan = nan). *)

let rec equal (a : t) (b : t) : bool =
  match (a, b) with
  | Null, Null -> true
  | Bool x, Bool y -> Bool.equal x y
  | Int x, Int y -> Int.equal x y
  | Float x, Float y -> Float.equal x y
  | String x, String y -> String.equal x y
  | Bytes x, Bytes y -> String.equal x y
  | List xs, List ys -> ( try List.for_all2 equal xs ys with Invalid_argument _ -> false)
  | Assoc xs, Assoc ys -> equal_assoc xs ys
  | _ -> false

and equal_assoc xs ys =
  match (xs, ys) with
  | [], [] -> true
  | (k1, v1) :: xr, (k2, v2) :: yr -> String.equal k1 k2 && equal v1 v2 && equal_assoc xr yr
  | _ -> false

let rank = function
  | Null -> 0
  | Bool _ -> 1
  | Int _ -> 2
  | Float _ -> 3
  | String _ -> 4
  | Bytes _ -> 5
  | List _ -> 6
  | Assoc _ -> 7

let rec compare (a : t) (b : t) : int =
  match (a, b) with
  | Null, Null -> 0
  | Bool x, Bool y -> Bool.compare x y
  | Int x, Int y -> Int.compare x y
  | Float x, Float y -> Float.compare x y
  | String x, String y -> String.compare x y
  | Bytes x, Bytes y -> String.compare x y
  | List xs, List ys -> compare_list xs ys
  | Assoc xs, Assoc ys -> compare_assoc xs ys
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

(* ---- lookup + a readable debug rendering (JSON-ish; not a serializer — that is a Format) -------- *)

(* field lookup in an [Assoc] (first match), [None] for a non-object or absent key *)
let get (key : string) (v : t) : t option = match v with Assoc kvs -> List.assoc_opt key kvs | _ -> None

let rec pp (fmt : Format.formatter) (v : t) : unit =
  match v with
  | Null -> Format.pp_print_string fmt "null"
  | Bool b -> Format.pp_print_bool fmt b
  | Int n -> Format.pp_print_int fmt n
  | Float f -> Format.fprintf fmt "%g" f
  | String s -> Format.fprintf fmt "%S" s
  | Bytes b -> Format.fprintf fmt "bytes(%d)" (String.length b)
  | List xs ->
      Format.fprintf fmt "[%a]"
        (Format.pp_print_list ~pp_sep:(fun fmt () -> Format.pp_print_string fmt ", ") pp)
        xs
  | Assoc kvs ->
      Format.fprintf fmt "{%a}"
        (Format.pp_print_list
           ~pp_sep:(fun fmt () -> Format.pp_print_string fmt ", ")
           (fun fmt (k, v) -> Format.fprintf fmt "%S: %a" k pp v))
        kvs

let to_string (v : t) : string = Format.asprintf "%a" pp v
