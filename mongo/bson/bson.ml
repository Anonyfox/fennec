(* A friendly BSON value type — the canonical in-memory value shared by the query engine,
   Minimongo, and (later) the native driver. This module is deliberately dependency-free (no
   Yojson, no Unix, no Eio) so the SAME source compiles to native OCaml and, via js_of_ocaml, to
   JavaScript. An extended-JSON wire codec can be added as a separate, native-only module; this
   module stays pure and provides only a debug rendering ({!to_string}). *)

type t =
  | Null
  | Bool of bool
  | Int of int
  | Int64 of int64
  | Float of float
  | String of string
  | Document of (string * t) list
  | Array of t list
  | Object_id of string (* 24-char hex *)
  | Date of int64 (* milliseconds since the Unix epoch *)
  | Timestamp of { t : int; i : int } (* BSON timestamp: seconds + ordinal *)
  | Binary of { subtype : string; base64 : string } (* subtype is 2-hex *)
  | Regex of { pattern : string; options : string }
  | Decimal128 of string (* decimal kept as its canonical string form *)
  | Code of string (* JavaScript code without scope *)
  | Code_with_scope of string * (string * t) list
  | Symbol of string
  | Min_key
  | Max_key

(* ---- construction helpers ------------------------------------------------ *)

let doc fields = Document fields
let str s = String s
let int n = Int n
let int64 n = Int64 n
let float f = Float f
let bool b = Bool b
let array xs = Array xs
let null = Null
let date ms = Date ms
let oid s = Object_id s

let is_hex_char c =
  (c >= '0' && c <= '9') || (c >= 'a' && c <= 'f') || (c >= 'A' && c <= 'F')

let object_id_of_string s =
  if String.length s = 24 && String.for_all is_hex_char s then Some (Object_id s)
  else None

(* a top-level field key that names a Mongo operator ($-prefixed) *)
let is_operator_key k = String.length k > 0 && k.[0] = '$'

(* ---- access -------------------------------------------------------------- *)

let fields = function Document kvs -> kvs | _ -> []
let get t key = match t with Document kvs -> List.assoc_opt key kvs | _ -> None
let get_string t key = match get t key with Some (String s) -> Some s | _ -> None
let get_bool t key = match get t key with Some (Bool b) -> Some b | _ -> None
let get_list t key = match get t key with Some (Array xs) -> Some xs | _ -> None

let get_int t key =
  match get t key with
  | Some (Int n) -> Some n
  | Some (Int64 n) -> Some (Int64.to_int n)
  | _ -> None

let get_float t key =
  match get t key with
  | Some (Float f) -> Some f
  | Some (Int n) -> Some (float_of_int n)
  | Some (Int64 n) -> Some (Int64.to_float n)
  | _ -> None

(* numeric projection: the value as a float when it is any numeric (or [Date]), else [None]. The
   single source of truth for "is this a number" across the engine. *)
let as_float = function
  | Int n -> Some (float_of_int n)
  | Int64 n -> Some (Int64.to_float n)
  | Float f -> Some f
  | Date d -> Some (Int64.to_float d)
  | _ -> None

(* ---- equality & ordering ------------------------------------------------- *)

(* Value equality. Numeric types ([Int]/[Int64]/[Float]) compare by numeric value, so [Int 1],
   [Int64 1L] and [Float 1.0] are equal (as in MongoDB). [Document] equality is order-SENSITIVE on
   fields (also as in MongoDB). [Float nan] equals nothing, including itself. *)
let rec equal a b =
  match (a, b) with
  | (Int _ | Int64 _ | Float _), (Int _ | Int64 _ | Float _) -> (
      match (as_float a, as_float b) with Some x, Some y -> x = y | _ -> false)
  | Document x, Document y ->
      List.length x = List.length y
      && List.for_all2 (fun (k1, v1) (k2, v2) -> k1 = k2 && equal v1 v2) x y
  | Array x, Array y -> ( try List.for_all2 equal x y with Invalid_argument _ -> false)
  | _ -> a = b

(* BSON type precedence for cross-type ordering (an INTENTIONAL order, never the arbitrary
   constructor-declaration order of [Stdlib.compare]). Mirrors MongoDB's canonical ordering. *)
let type_rank = function
  | Min_key -> 0
  | Null -> 1
  | Int _ | Int64 _ | Float _ -> 2
  | String _ | Symbol _ -> 3
  | Document _ -> 4
  | Array _ -> 5
  | Binary _ -> 6
  | Object_id _ -> 7
  | Bool _ -> 8
  | Date _ -> 9
  | Timestamp _ -> 10
  | Regex _ -> 11
  | Code _ | Code_with_scope _ -> 12
  | Decimal128 _ -> 13
  | Max_key -> 14

(* A TOTAL order over all values, for sorting. Cross-type comparisons use {!type_rank}; numbers
   compare by value with [nan] sorted lowest; the rare composite types fall back to a structural
   compare WITHIN their own rank (so order never depends on constructor declaration order). *)
let rec compare a b =
  let ra = type_rank a and rb = type_rank b in
  if ra <> rb then Stdlib.compare ra rb
  else
    match (a, b) with
    | (Int _ | Int64 _ | Float _), (Int _ | Int64 _ | Float _) -> (
        match (as_float a, as_float b) with
        | Some x, Some y ->
            let nx = Float.is_nan x and ny = Float.is_nan y in
            if nx && ny then 0
            else if nx then -1
            else if ny then 1
            else Stdlib.compare x y
        | _ -> 0)
    | (String x | Symbol x), (String y | Symbol y) -> String.compare x y
    | Bool x, Bool y -> Stdlib.compare x y
    | Object_id x, Object_id y -> String.compare x y
    | Date x, Date y -> Int64.compare x y
    | Array x, Array y -> compare_list x y
    | _ -> Stdlib.compare a b

and compare_list a b =
  match (a, b) with
  | [], [] -> 0
  | [], _ -> -1
  | _, [] -> 1
  | x :: xs, y :: ys ->
      let c = compare x y in
      if c <> 0 then c else compare_list xs ys

(* ---- debug rendering (NOT extended-JSON) --------------------------------- *)

let rec to_string = function
  | Null -> "null"
  | Bool b -> string_of_bool b
  | Int n -> string_of_int n
  | Int64 n -> Int64.to_string n ^ "L"
  | Float f -> Printf.sprintf "%g" f
  | String s -> "\"" ^ s ^ "\""
  | Object_id s -> "ObjectId(" ^ s ^ ")"
  | Date d -> "Date(" ^ Int64.to_string d ^ ")"
  | Timestamp { t; i } -> Printf.sprintf "Timestamp(%d,%d)" t i
  | Binary { subtype; base64 } -> Printf.sprintf "Binary(%s,%s)" subtype base64
  | Regex { pattern; options } -> Printf.sprintf "/%s/%s" pattern options
  | Decimal128 s -> s
  | Code s -> "Code(" ^ s ^ ")"
  | Code_with_scope (s, _) -> "Code(" ^ s ^ ", <scope>)"
  | Symbol s -> s
  | Min_key -> "MinKey"
  | Max_key -> "MaxKey"
  | Document kvs ->
      "{" ^ String.concat ", " (List.map (fun (k, v) -> k ^ ": " ^ to_string v) kvs) ^ "}"
  | Array xs -> "[" ^ String.concat ", " (List.map to_string xs) ^ "]"

let pp fmt t = Format.pp_print_string fmt (to_string t)
