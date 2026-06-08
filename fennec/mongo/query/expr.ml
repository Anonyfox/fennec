(* Aggregation expression evaluator — evaluates a MongoDB aggregation expression against a document
   to a Bson value. Pure. Field paths ([$a.b]), system variables ([$$ROOT]/[$$CURRENT] and the
   [$$<var>] bound by $map/$filter), literals, and the common operator expressions (arithmetic,
   comparison, boolean, conditional, string, array, type) plus the $group accumulators. *)

open Bson

let starts_with2 s = String.length s >= 2 && s.[0] = '$' && s.[1] = '$'

(* aggregation truthiness: false / 0 / null / missing are falsy *)
let truthy = function Bool false | Null | Int 0 -> false | Float f when f = 0. -> false | _ -> true

let all_int xs = List.for_all (function Int _ -> true | _ -> false) xs
let nums xs = List.filter_map Bson.as_float xs
let numify ints f = if ints && Float.is_integer f then Int (int_of_float f) else Float f

let to_str = function
  | String s -> s
  | Int n -> string_of_int n
  | Int64 n -> Int64.to_string n
  | Float f -> Printf.sprintf "%g" f
  | Bool b -> string_of_bool b
  | Null -> ""
  | v -> Bson.to_string v

(* split a string on a (possibly multi-char) separator *)
let split_str sep s =
  if sep = "" then [ s ]
  else
    let sl = String.length sep and n = String.length s in
    let rec go start i acc =
      if i + sl > n then List.rev (String.sub s start (n - start) :: acc)
      else if String.sub s i sl = sep then go (i + sl) (i + sl) (String.sub s start (i - start) :: acc)
      else go start (i + 1) acc
    in
    go 0 0 []

let rec eval ?(vars = []) (expr : Bson.t) (doc : Bson.t) : Bson.t =
  match expr with
  | String s when starts_with2 s -> (
      (* $$ROOT / $$CURRENT / $$<var>[.path] *)
      let rest = String.sub s 2 (String.length s - 2) in
      let name, path =
        match String.index_opt rest '.' with
        | Some i -> (String.sub rest 0 i, Some (String.sub rest (i + 1) (String.length rest - i - 1)))
        | None -> (rest, None)
      in
      let base = match name with "ROOT" | "CURRENT" -> doc | _ -> ( match List.assoc_opt name vars with Some v -> v | None -> Null) in
      match path with None -> base | Some p -> ( match Matcher.get_path base p with Some v -> v | None -> Null))
  | String s when String.length s > 0 && s.[0] = '$' -> (
      let p = String.sub s 1 (String.length s - 1) in
      match Matcher.get_path doc p with Some v -> v | None -> Null)
  | Document [ (op, arg) ] when Bson.is_operator_key op -> eval_op ~vars op arg doc
  | Document kvs -> Document (List.map (fun (k, v) -> (k, eval ~vars v doc)) kvs)
  | Array xs -> Array (List.map (fun x -> eval ~vars x doc) xs)
  | lit -> lit

and eval_list ~vars arg doc =
  match arg with Array xs -> List.map (fun x -> eval ~vars x doc) xs | x -> [ eval ~vars x doc ]

and eval_op ~vars op arg doc : Bson.t =
  let ev x = eval ~vars x doc in
  let args () = eval_list ~vars arg doc in
  let cmp2 f = match args () with [ a; b ] -> Bool (f (Bson.compare a b)) | _ -> Bool false in
  match op with
  | "$literal" -> arg
  (* arithmetic *)
  | "$add" -> let vs = args () in numify (all_int vs) (List.fold_left ( +. ) 0. (nums vs))
  | "$multiply" -> let vs = args () in numify (all_int vs) (List.fold_left ( *. ) 1. (nums vs))
  | "$subtract" -> ( match args () with [ a; b ] -> numify (all_int [ a; b ]) (num a -. num b) | _ -> Null)
  | "$divide" -> ( match args () with [ a; b ] when num b <> 0. -> Float (num a /. num b) | _ -> Null)
  | "$mod" -> ( match args () with [ a; b ] when num b <> 0. -> numify (all_int [ a; b ]) (Float.rem (num a) (num b)) | _ -> Null)
  | "$abs" -> ( match args () with [ a ] -> numify (all_int [ a ]) (Float.abs (num a)) | _ -> Null)
  | "$ceil" -> ( match args () with [ a ] -> Int (int_of_float (Float.ceil (num a))) | _ -> Null)
  | "$floor" -> ( match args () with [ a ] -> Int (int_of_float (Float.floor (num a))) | _ -> Null)
  | "$round" -> ( match args () with a :: _ -> Int (int_of_float (Float.round (num a))) | _ -> Null)
  (* comparison *)
  | "$eq" -> ( match args () with [ a; b ] -> Bool (Bson.equal a b) | _ -> Bool false)
  | "$ne" -> ( match args () with [ a; b ] -> Bool (not (Bson.equal a b)) | _ -> Bool false)
  | "$gt" -> cmp2 (fun c -> c > 0)
  | "$gte" -> cmp2 (fun c -> c >= 0)
  | "$lt" -> cmp2 (fun c -> c < 0)
  | "$lte" -> cmp2 (fun c -> c <= 0)
  | "$cmp" -> ( match args () with [ a; b ] -> Int (Bson.compare a b) | _ -> Null)
  (* boolean *)
  | "$and" -> Bool (List.for_all truthy (args ()))
  | "$or" -> Bool (List.exists truthy (args ()))
  | "$not" -> ( match args () with [ a ] -> Bool (not (truthy a)) | _ -> Bool true)
  (* conditional *)
  | "$cond" -> (
      match arg with
      | Document kvs ->
          if truthy (ev (try List.assoc "if" kvs with Not_found -> Null)) then ev (List.assoc "then" kvs)
          else ev (List.assoc "else" kvs)
      | Array [ c; t; e ] -> if truthy (ev c) then ev t else ev e
      | _ -> Null)
  | "$ifNull" -> ( match args () with a :: fallback :: _ -> ( match a with Null -> fallback | v -> v) | _ -> Null)
  | "$switch" -> (
      match arg with
      | Document kvs ->
          let branches = match List.assoc_opt "branches" kvs with Some (Array bs) -> bs | _ -> [] in
          let rec go = function
            | Document b :: tl ->
                if truthy (ev (try List.assoc "case" b with Not_found -> Null)) then ev (List.assoc "then" b)
                else go tl
            | _ -> ( match List.assoc_opt "default" kvs with Some d -> ev d | None -> Null)
          in
          go branches
      | _ -> Null)
  (* string *)
  | "$concat" -> String (String.concat "" (List.map to_str (args ())))
  | "$toLower" -> ( match args () with a :: _ -> String (String.lowercase_ascii (to_str a)) | _ -> String "")
  | "$toUpper" -> ( match args () with a :: _ -> String (String.uppercase_ascii (to_str a)) | _ -> String "")
  | "$strLenCp" -> ( match args () with a :: _ -> Int (String.length (to_str a)) | _ -> Int 0)
  | "$split" -> ( match args () with [ a; b ] -> Array (List.map (fun s -> String s) (split_str (to_str b) (to_str a))) | _ -> Null)
  | "$substr" | "$substrBytes" -> (
      match args () with
      | [ s; start; len ] -> (
          let str = to_str s and i = int_of_float (num start) and l = int_of_float (num len) in
          let n = String.length str in
          let i = max 0 (min i n) in
          let l = max 0 (min l (n - i)) in
          String (String.sub str i l))
      | _ -> String "")
  (* array *)
  | "$size" -> ( match args () with [ Array xs ] -> Int (List.length xs) | _ -> Int 0)
  | "$isArray" -> ( match args () with [ Array _ ] -> Bool true | _ -> Bool false)
  | "$arrayElemAt" -> (
      match args () with
      | [ Array xs; idx ] ->
          let i = int_of_float (num idx) in
          let i = if i < 0 then List.length xs + i else i in
          ( match List.nth_opt xs i with Some v -> v | None -> Null)
      | _ -> Null)
  | "$concatArrays" -> Array (List.concat_map (function Array xs -> xs | _ -> []) (args ()))
  | "$reverseArray" -> ( match args () with [ Array xs ] -> Array (List.rev xs) | _ -> Null)
  | "$in" -> ( match args () with [ v; Array xs ] -> Bool (List.exists (Bson.equal v) xs) | _ -> Bool false)
  | "$filter" -> (
      match arg with
      | Document kvs -> (
          let input = ev (try List.assoc "input" kvs with Not_found -> Null) in
          let as_ = match List.assoc_opt "as" kvs with Some (String s) -> s | _ -> "this" in
          let cond = try List.assoc "cond" kvs with Not_found -> Bool true in
          match input with
          | Array xs -> Array (List.filter (fun e -> truthy (eval ~vars:((as_, e) :: vars) cond doc)) xs)
          | _ -> Null)
      | _ -> Null)
  | "$map" -> (
      match arg with
      | Document kvs -> (
          let input = ev (try List.assoc "input" kvs with Not_found -> Null) in
          let as_ = match List.assoc_opt "as" kvs with Some (String s) -> s | _ -> "this" in
          let body = try List.assoc "in" kvs with Not_found -> Null in
          match input with
          | Array xs -> Array (List.map (fun e -> eval ~vars:((as_, e) :: vars) body doc) xs)
          | _ -> Null)
      | _ -> Null)
  (* type / conversion *)
  | "$type" -> ( match args () with [ a ] -> String (Matcher.type_name a) | _ -> String "missing")
  | "$toString" -> ( match args () with a :: _ -> String (to_str a) | _ -> Null)
  | "$toInt" -> ( match args () with a :: _ -> Int (int_of_float (num a)) | _ -> Null)
  | "$toDouble" -> ( match args () with a :: _ -> Float (num a) | _ -> Null)
  | "$toBool" -> ( match args () with a :: _ -> Bool (truthy a) | _ -> Null)
  | _ -> Null

and num v = match Bson.as_float v with Some x -> x | None -> 0.

(* dedup preserving order, by value equality *)
let dedup xs =
  List.rev (List.fold_left (fun acc x -> if List.exists (Bson.equal x) acc then acc else x :: acc) [] xs)

(* a $group accumulator over the documents of one group *)
let accumulate (acc_expr : Bson.t) (docs : Bson.t list) : Bson.t =
  let evals e = List.map (eval e) docs in
  match acc_expr with
  | Document [ ("$sum", e) ] ->
      let vs = evals e in
      numify (all_int vs) (List.fold_left ( +. ) 0. (nums vs))
  | Document [ ("$avg", e) ] -> (
      let ns = nums (evals e) in
      match ns with [] -> Null | _ -> Float (List.fold_left ( +. ) 0. ns /. float_of_int (List.length ns)))
  | Document [ ("$min", e) ] -> ( match evals e with [] -> Null | xs -> List.fold_left (fun a b -> if Bson.compare b a < 0 then b else a) (List.hd xs) xs)
  | Document [ ("$max", e) ] -> ( match evals e with [] -> Null | xs -> List.fold_left (fun a b -> if Bson.compare b a > 0 then b else a) (List.hd xs) xs)
  | Document [ ("$first", e) ] -> ( match docs with d :: _ -> eval e d | [] -> Null)
  | Document [ ("$last", e) ] -> ( match List.rev docs with d :: _ -> eval e d | [] -> Null)
  | Document [ ("$push", e) ] -> Array (evals e)
  | Document [ ("$addToSet", e) ] -> Array (dedup (evals e))
  | Document [ ("$count", _) ] -> Int (List.length docs)
  | other -> ( match docs with d :: _ -> eval other d | [] -> Null)
