(* Selector matching — the pure heart of minimongo. Supports the comparison/membership/array/
   element operators ($eq, $ne, $exists, $in, $nin, $gt/$gte/$lt/$lte, $all, $size, $elemMatch,
   $not, $type, $mod) plus top-level $and/$or/$nor over dotted paths. Operators that would require
   a regex engine ($regex) or arbitrary code ($where) are intentionally omitted so the module stays
   Stdlib-only and JavaScript-safe.

   Two cross-cutting rules match MongoDB/minimongo: (1) equality and comparison use {!Bson.equal}
   and a SAME-TYPE comparison ([bson_cmp]) — a number query never matches a string (type
   bracketing); (2) a scalar predicate against an ARRAY field matches if the array as a whole OR
   any element matches (so [{tags:"a"}] matches [{tags:["a","b"]}]). *)

open Bson

(* SAME-TYPE comparison for range operators (type bracketing): numbers/dates compare numerically,
   strings and bools naturally; [None] across incomparable types so a range query is type-scoped.
   (Total cross-type ordering, for sorting, lives in {!Bson.compare}.) *)
let bson_cmp a b =
  match (Bson.as_float a, Bson.as_float b) with
  | Some x, Some y -> Some (Stdlib.compare x y)
  | _ -> (
      match (a, b) with
      | Bson.String s, Bson.String t -> Some (String.compare s t)
      | Bson.Bool x, Bson.Bool y -> Some (Stdlib.compare x y)
      | _ -> None)

(* dotted-path field access: "a.b.c" walks nested documents *)
let rec get_path (d : Bson.t) (path : string) : Bson.t option =
  match String.index_opt path '.' with
  | None -> Bson.get d path
  | Some i -> (
      let head = String.sub path 0 i in
      let rest = String.sub path (i + 1) (String.length path - i - 1) in
      match Bson.get d head with Some sub -> get_path sub rest | None -> None)

let type_name : Bson.t -> string = function
  | Null -> "null"
  | Bool _ -> "bool"
  | Int _ -> "int"
  | Int64 _ -> "long"
  | Float _ -> "double"
  | String _ -> "string"
  | Document _ -> "object"
  | Array _ -> "array"
  | Object_id _ -> "objectId"
  | Date _ -> "date"
  | Timestamp _ -> "timestamp"
  | Binary _ -> "binData"
  | Regex _ -> "regex"
  | Decimal128 _ -> "decimal"
  | Code _ | Code_with_scope _ -> "javascript"
  | Symbol _ -> "symbol"
  | Min_key -> "minKey"
  | Max_key -> "maxKey"

(* MongoDB's "number" umbrella plus the exact type aliases *)
let type_matches (name : string) (fv : Bson.t) : bool =
  match name with
  | "number" -> ( match fv with Int _ | Int64 _ | Float _ | Decimal128 _ -> true | _ -> false)
  | _ -> type_name fv = name

(* MongoDB existence truthiness: false / 0 / null mean "must not exist" *)
let truthy : Bson.t -> bool = function
  | Bool false | Null | Int 0 -> false
  | Float f when f = 0. -> false
  | _ -> true

let cmp_op op c =
  match op with "$gt" -> c > 0 | "$gte" -> c >= 0 | "$lt" -> c < 0 | _ -> c <= 0

(* operators that act on the array value AS A WHOLE rather than per element *)
let is_array_op = function "$size" | "$all" | "$elemMatch" -> true | _ -> false

(* one operator against one CONCRETE value (no array fan-out) *)
let rec op1 op (v : Bson.t) (fv : Bson.t) : bool =
  match op with
  | "$eq" -> Bson.equal fv v
  | "$gt" | "$gte" | "$lt" | "$lte" -> (
      match bson_cmp fv v with Some c -> cmp_op op c | None -> false)
  | "$type" -> (
      match v with
      | String t -> type_matches t fv
      | Array ts ->
          List.exists (function Bson.String t -> type_matches t fv | _ -> false) ts
      | _ -> false)
  | "$mod" -> (
      match (v, Bson.as_float fv) with
      | Array [ d; r ], Some x -> (
          match (Bson.as_float d, Bson.as_float r) with
          | Some dv, Some rv when dv <> 0. && Float.is_finite x ->
              Float.rem (Float.trunc x) dv = rv
          | _ -> false)
      | _ -> false)
  | "$all" -> (
      match (fv, v) with
      | Array elems, Array wanted ->
          List.for_all (fun w -> List.exists (fun e -> Bson.equal e w) elems) wanted
      | _ -> false)
  | "$size" -> (
      match (fv, Bson.as_float v) with
      | Array elems, Some n -> float_of_int (List.length elems) = n
      | _ -> false)
  | "$elemMatch" -> (
      match fv with Array elems -> List.exists (fun e -> matches_cond (Some e) v) elems | _ -> false)
  | _ -> true (* unknown operator: never wrongly hide a document *)

(* a single-value operator fanned over a field value: array fields match if the array as a whole
   OR any element matches (except the array-targeting operators, which take the array directly) *)
and field_pos op v field_val =
  match field_val with
  | Some (Array elems as arr) when not (is_array_op op) ->
      op1 op v arr || List.exists (op1 op v) elems
  | Some fv -> op1 op v fv
  | None -> false

and matches_op (field_val : Bson.t option) op (v : Bson.t) : bool =
  match op with
  | "$eq" | "$gt" | "$gte" | "$lt" | "$lte" | "$type" | "$mod" -> field_pos op v field_val
  | "$ne" -> not (field_pos "$eq" v field_val)
  | "$in" -> (
      match v with Array xs -> List.exists (fun x -> field_pos "$eq" x field_val) xs | _ -> false)
  | "$nin" -> (
      match v with
      | Array xs -> not (List.exists (fun x -> field_pos "$eq" x field_val) xs)
      | _ -> true)
  | "$exists" -> (field_val <> None) = truthy v
  | "$not" -> not (matches_cond field_val v)
  | "$all" | "$size" | "$elemMatch" -> (
      match field_val with Some fv -> op1 op v fv | None -> false)
  | _ -> true (* unknown operator: never wrongly hide a document *)

and matches_cond (field_val : Bson.t option) (cond : Bson.t) : bool =
  match cond with
  | Document kvs when List.exists (fun (k, _) -> Bson.is_operator_key k) kvs ->
      List.for_all (fun (op, v) -> matches_op field_val op v) kvs
  | _ -> (
      match field_val with
      | Some (Array elems as arr) ->
          Bson.equal arr cond || List.exists (fun e -> Bson.equal e cond) elems
      | Some fv -> Bson.equal fv cond
      | None -> false)

(* Does [d] satisfy [selector]? Top-level $and/$or/$nor, dotted paths, implicit AND over the
   remaining field conditions. *)
and doc_matches (selector : Bson.t) (d : Bson.t) : bool =
  match selector with
  | Document kvs ->
      List.for_all
        (fun (k, cond) ->
          match k with
          | "$and" -> (
              match cond with Array xs -> List.for_all (fun s -> doc_matches s d) xs | _ -> true)
          | "$or" -> (
              match cond with Array xs -> List.exists (fun s -> doc_matches s d) xs | _ -> true)
          | "$nor" -> (
              match cond with
              | Array xs -> not (List.exists (fun s -> doc_matches s d) xs)
              | _ -> true)
          | _ when Bson.is_operator_key k -> true
          | _ -> matches_cond (get_path d k) cond)
        kvs
  | _ -> true
