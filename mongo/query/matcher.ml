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
  | Bool false | Null | Int 0 | Int64 0L -> false
  | Float f when f = 0. -> false
  | _ -> true

let cmp_op op c =
  match op with "$gt" -> c > 0 | "$gte" -> c >= 0 | "$lt" -> c < 0 | _ -> c <= 0

(* operators that act on the array value AS A WHOLE rather than per element *)
let is_array_op = function "$size" | "$all" | "$elemMatch" -> true | _ -> false

(* integer view of a field value for bitwise tests *)
let int_value = function Int n -> Some n | Int64 n -> Some (Int64.to_int n) | _ -> None

(* a bitmask from a $bits* operand: an int mask, or an array of bit positions. Bit positions are
   capped at 0..30 so the masking is identical on native (63-bit int) and js_of_ocaml (32-bit int);
   higher positions are not supported. *)
let bit_mask = function
  | Int n -> Some n
  | Int64 n -> Some (Int64.to_int n)
  | Array positions ->
      Some
        (List.fold_left
           (fun m p -> match p with Int b when b >= 0 && b < 31 -> m lor (1 lsl b) | _ -> m)
           0 positions)
  | _ -> None

(* $regex via the pure [Re] library (PCRE-ish), compiled once per (options,pattern) and cached.
   BOUNDED: a long-running server matching many distinct operands would otherwise grow this without
   limit. When the cache fills we [reset] it wholesale (cheaper than per-entry LRU bookkeeping, and
   correctness-neutral — a missed entry just recompiles); the working set of hot patterns refills
   immediately. *)
let re_cache : (string, Re.re) Hashtbl.t = Hashtbl.create 16
let re_cache_cap = 4096

let compile_re pattern opts =
  let key = opts ^ "\x00" ^ pattern in
  match Hashtbl.find_opt re_cache key with
  | Some r -> Some r
  | None -> (
      try
        let flags =
          String.fold_left
            (fun acc c ->
              match c with
              | 'i' -> `CASELESS :: acc
              | 'm' -> `MULTILINE :: acc
              | 's' -> `DOTALL :: acc
              | _ -> acc)
            [] opts
        in
        let r = Re.compile (Re.Pcre.re ~flags pattern) in
        if Hashtbl.length re_cache >= re_cache_cap then Hashtbl.reset re_cache;
        Hashtbl.replace re_cache key r;
        Some r
      with _ -> None)

let regex_matches pattern opts s =
  match compile_re pattern opts with Some r -> Re.execp r s | None -> false

(* test a field value against a $regex operand (a string pattern or a [Regex]); array-aware *)
let regex_field field_val pat opts =
  let pattern, opts =
    match pat with
    | String s -> (s, opts)
    | Regex r -> (r.pattern, r.options ^ opts)
    | _ -> ("", opts)
  in
  let test = function String s -> regex_matches pattern opts s | _ -> false in
  match field_val with
  | Some (Array elems) -> List.exists test elems
  | Some fv -> test fv
  | None -> false

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
  | "$bitsAllSet" | "$bitsAllClear" | "$bitsAnySet" | "$bitsAnyClear" -> (
      match (int_value fv, bit_mask v) with
      | Some f, Some m -> (
          match op with
          | "$bitsAllSet" -> f land m = m
          | "$bitsAllClear" -> f land m = 0
          | "$bitsAnySet" -> f land m <> 0
          | _ -> f land m <> m (* $bitsAnyClear *))
      | _ -> false)
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
  | "$eq" | "$gt" | "$gte" | "$lt" | "$lte" | "$type" | "$mod" | "$bitsAllSet" | "$bitsAllClear"
  | "$bitsAnySet" | "$bitsAnyClear" ->
      field_pos op v field_val
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
  | "$geoWithin" -> ( match field_val with Some fv -> Geo.within fv v | None -> false)
  | "$geoIntersects" -> ( match field_val with Some fv -> Geo.intersects fv v | None -> false)
  | "$near" -> ( match field_val with Some fv -> Geo.near ~force_sphere:false fv v | None -> false)
  | "$nearSphere" -> ( match field_val with Some fv -> Geo.near ~force_sphere:true fv v | None -> false)
  | _ -> true (* unknown operator: never wrongly hide a document *)

and matches_cond (field_val : Bson.t option) (cond : Bson.t) : bool =
  match cond with
  | Document kvs when List.exists (fun (k, _) -> Bson.is_operator_key k) kvs ->
      let opts = match List.assoc_opt "$options" kvs with Some (String s) -> s | _ -> "" in
      List.for_all
        (fun (op, v) ->
          match op with
          | "$options" -> true (* consumed by $regex *)
          | "$regex" -> regex_field field_val v opts
          | _ -> matches_op field_val op v)
        kvs
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
