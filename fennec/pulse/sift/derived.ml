(* derived.ml — free, shape-derived operations (the repr-style axis E): equal / compare / hash /
   default. Each is composed structurally from the shape, MONOMORPHIC — no reliance on OCaml's
   polymorphic runtime compare/hash (which is slow, samples large structures, and raises on functions).
   A record's [equal] is the AND of its fields' equals, etc. The hook for later customization (ignore a
   field, custom float semantics) without changing call sites. *)

open Shape

(* ---- structural equality (a record: all fields equal; a variant: same case + equal bodies) ------ *)

let rec equal : type a. a shape -> a -> a -> bool =
 fun shape x y ->
  match shape with
  | TString -> String.equal x y
  | TInt -> Int.equal x y
  | TFloat _ -> Float.equal x y
  | TBool -> Bool.equal x y
  | TDate -> Int64.equal x y
  | TId -> String.equal x y
  | TBson -> Bson.equal x y
  | TUnit -> true
  | TList el -> ( try List.for_all2 (equal el) x y with Invalid_argument _ -> false)
  | TOption el -> ( match (x, y) with None, None -> true | Some a, Some b -> equal el a b | _ -> false)
  | TMap el -> List.compare_lengths x y = 0 && List.for_all2 (fun (k1, v1) (k2, v2) -> String.equal k1 k2 && equal el v1 v2) x y
  | TCheck (_, _, _, inner) -> equal inner x y
  | TNorm (_, inner) -> equal inner x y
  | TConv (inj, _, inner) -> equal inner (inj x) (inj y)
  | TCoerce inner -> equal inner x y
  | TLazy l -> equal (Lazy.force l) x y
  | TObj o -> List.for_all (fun (Bound_field f) -> equal f.shape (f.get x) (f.get y)) o.members
  | TVariant { cases; _ } -> equal_variant cases x y

and equal_variant : type r. r case list -> r -> r -> bool =
 fun cases x y ->
  match cases with
  | [] -> false
  | Case c :: rest -> (
      match c.project x with
      | Some a -> ( match c.project y with Some b -> List.for_all (fun (Bound_field f) -> equal f.shape (f.get a) (f.get b)) c.body.members | None -> false)
      | None -> equal_variant rest x y)

(* ---- total ordering (lexicographic over fields / list elements; variants by case order) --------- *)

let rec compare : type a. a shape -> a -> a -> int =
 fun shape x y ->
  match shape with
  | TString -> String.compare x y
  | TInt -> Int.compare x y
  | TFloat _ -> Float.compare x y
  | TBool -> Bool.compare x y
  | TDate -> Int64.compare x y
  | TId -> String.compare x y
  | TBson -> Stdlib.compare x y
  | TUnit -> 0
  | TList el -> compare_list el x y
  | TOption el -> ( match (x, y) with None, None -> 0 | None, Some _ -> -1 | Some _, None -> 1 | Some a, Some b -> compare el a b)
  | TMap el -> compare_map el x y
  | TCheck (_, _, _, inner) -> compare inner x y
  | TNorm (_, inner) -> compare inner x y
  | TConv (inj, _, inner) -> compare inner (inj x) (inj y)
  | TCoerce inner -> compare inner x y
  | TLazy l -> compare (Lazy.force l) x y
  | TObj o -> compare_members o.members x y
  | TVariant { cases; _ } -> compare_variant cases 0 x y

and compare_list : type a. a shape -> a list -> a list -> int =
 fun el xs ys ->
  match (xs, ys) with [], [] -> 0 | [], _ -> -1 | _, [] -> 1 | x :: xr, y :: yr -> let c = compare el x y in if c <> 0 then c else compare_list el xr yr

and compare_map : type a. a shape -> (string * a) list -> (string * a) list -> int =
 fun el xs ys ->
  match (xs, ys) with
  | [], [] -> 0
  | [], _ -> -1
  | _, [] -> 1
  | (k1, v1) :: xr, (k2, v2) :: yr -> let c = String.compare k1 k2 in if c <> 0 then c else let c = compare el v1 v2 in if c <> 0 then c else compare_map el xr yr

and compare_members : type r. r bound_field list -> r -> r -> int =
 fun members x y ->
  match members with
  | [] -> 0
  | Bound_field f :: rest -> let c = compare f.shape (f.get x) (f.get y) in if c <> 0 then c else compare_members rest x y

and compare_variant : type r. r case list -> int -> r -> r -> int =
 fun cases idx x y ->
  match cases with
  | [] -> 0
  | Case c :: rest -> (
      match (c.project x, c.project y) with
      | Some a, Some b -> compare_members c.body.members a b
      | Some _, None -> -1 (* x is this (earlier) case, y is a later one *)
      | None, Some _ -> 1
      | None, None -> compare_variant rest (idx + 1) x y)

(* ---- structural hash (monomorphic; combine = the classic 31x mix) -------------------------------- *)

let combine a b = (a * 31) + b

let rec hash : type a. a shape -> a -> int =
 fun shape v ->
  match shape with
  | TString -> Hashtbl.hash v
  | TInt -> v
  | TFloat _ -> Hashtbl.hash v
  | TBool -> if v then 1 else 0
  | TDate -> Hashtbl.hash v
  | TId -> Hashtbl.hash v
  | TBson -> Hashtbl.hash v
  | TUnit -> 0
  | TList el -> List.fold_left (fun acc x -> combine acc (hash el x)) 7 v
  | TOption el -> ( match v with None -> 0 | Some x -> combine 1 (hash el x))
  | TMap el -> List.fold_left (fun acc (k, x) -> combine (combine acc (Hashtbl.hash k)) (hash el x)) 11 v
  | TCheck (_, _, _, inner) -> hash inner v
  | TNorm (_, inner) -> hash inner v
  | TConv (inj, _, inner) -> hash inner (inj v)
  | TCoerce inner -> hash inner v
  | TLazy l -> hash (Lazy.force l) v
  | TObj o -> List.fold_left (fun acc (Bound_field f) -> combine acc (hash f.shape (f.get v))) 17 o.members
  | TVariant { cases; _ } -> hash_variant cases 1 v

and hash_variant : type r. r case list -> int -> r -> int =
 fun cases tag v ->
  match cases with
  | [] -> 0
  | Case c :: rest -> ( match c.project v with Some a -> List.fold_left (fun acc (Bound_field f) -> combine acc (hash f.shape (f.get a))) tag c.body.members | None -> hash_variant rest (tag + 1) v)

(* ---- a sensible default/zero value from the shape (form initial values, fixtures) --------------- *)

(* leaves zero out; containers are empty; a record is built from its fields' defaults (via the same
   constructor decode threads — so required fields get their leaf default); a variant defaults to its
   first case. Terminates on recursive shapes ([fix]) because the recursion goes through a list/option,
   which default to []/None WITHOUT defaulting an element. *)
let rec default : type a. a shape -> a =
 fun shape ->
  match shape with
  | TString -> ""
  | TInt -> 0
  | TFloat _ -> 0.0
  | TBool -> false
  | TDate -> 0L
  | TId -> ""
  | TBson -> Bson.Null
  | TUnit -> ()
  | TList _ -> []
  | TOption _ -> None
  | TMap _ -> []
  | TCheck (_, _, _, inner) -> default inner
  | TNorm (f, inner) -> f (default inner)
  | TConv (_, proj, inner) -> ( match proj (default inner) with Ok b -> b | Error m -> invalid_arg ("Sift.default: conversion rejects the default value: " ^ m))
  | TCoerce inner -> default inner
  | TLazy l -> default (Lazy.force l)
  | TObj o -> ( match o.decode_src { read_field = (fun f -> Ok (default f.item)) } with Ok v -> v | Error _ -> invalid_arg "Sift.default: record")
  | TVariant { cases = []; _ } -> invalid_arg "Sift.default: variant has no cases"
  | TVariant { cases = Case c :: _; _ } -> ( match c.body.decode_src { read_field = (fun f -> Ok (default f.item)) } with Ok a -> c.inject a | Error _ -> invalid_arg "Sift.default: variant")

(* ---- structural diff: the minimal change from [old] to [new] as Mongo-style $set/$unset ----------

   The reactive-sync primitive: for two values of a DOCUMENT-shaped codec (record / map / variant) it
   reports exactly which fields changed (with their new encoded value) and which became absent. Maps
   straight onto a Mongo update modifier ($set/$unset) AND a DDP [changed]/[cleared] message — compute
   the minimal wire update from old→new without re-sending the unchanged fields. Field equality uses the
   shape's {!equal}; absence uses the field's [omit] (an optional gone to None, an opt_list to []). *)

type delta = { set : (string * Bson.t) list; unset : string list }

let no_change = { set = []; unset = [] }

let rec diff : type a. a shape -> a -> a -> delta =
 fun shape old new_ ->
  match shape with
  | TObj o -> diff_members o.members old new_
  | TMap el -> diff_map el old new_
  | TVariant { tag; cases } -> diff_variant shape tag cases old new_
  | TCheck (_, _, _, inner) -> diff inner old new_
  | TNorm (_, inner) -> diff inner old new_
  | TConv (inj, _, inner) -> diff inner (inj old) (inj new_)
  | TCoerce inner -> diff inner old new_
  | TLazy l -> diff (Lazy.force l) old new_
  | _ -> invalid_arg "Sift.diff: needs a document-shaped codec (record / map / variant)"

and diff_members : type r. r bound_field list -> r -> r -> delta =
 fun members old new_ ->
  let rec go set unset = function
    | [] -> { set = List.rev set; unset = List.rev unset }
    | Bound_field f :: rest ->
        let ov = f.get old and nv = f.get new_ in
        if equal f.shape ov nv then go set unset rest
        else if f.omit nv then go set (f.name :: unset) rest
        else go ((f.name, Engine.write f.shape nv) :: set) unset rest
  in
  go [] [] members

and diff_map : type a. a shape -> (string * a) list -> (string * a) list -> delta =
 fun el old new_ ->
  let set = List.filter_map (fun (k, nv) -> match List.assoc_opt k old with Some ov when equal el ov nv -> None | _ -> Some (k, Engine.write el nv)) new_ in
  let unset = List.filter_map (fun (k, _) -> if List.mem_assoc k new_ then None else Some k) old in
  { set; unset }

and diff_variant : type r. r shape -> string -> r case list -> r -> r -> delta =
 fun shape _tag cases old new_ ->
  let rec go = function
    | [] -> no_change
    | Case c :: rest -> (
        match c.project old with
        | Some a -> ( match c.project new_ with Some b -> diff_members c.body.members a b (* same case: tag unchanged, diff the body *) | None -> full_replace shape old new_)
        | None -> go rest)
  in
  go cases

(* the case changed: $set the whole new document (tag + new body fields), $unset the old fields that the
   new case no longer has. Only this rare path materialises old/new via {!Engine.write}. *)
and full_replace : type a. a shape -> a -> a -> delta =
 fun shape old new_ ->
  let fields v = match Engine.write shape v with Bson.Document kvs -> kvs | _ -> [] in
  let new_fields = fields new_ in
  let new_names = List.map fst new_fields in
  { set = new_fields; unset = List.filter_map (fun (n, _) -> if List.mem n new_names then None else Some n) (fields old) }

(* ---- arbitrary: a random value conforming to the shape (property testing) ----------------------

   A structural generator from the shape. Refinements are satisfied by REJECTION sampling (regenerate
   until the predicate holds, bounded) — so simple ones (length/enum/range) are met; a hard [pattern] or
   a record-level cross-field invariant may not be (honest: best-effort). [fuel] bounds recursion depth
   (a [fix] shape generates shallow trees because the recursion runs through a list/option that empties).
   Pure given the {!Random.State.t} (jsoo-safe), so a seeded state reproduces. *)

let hexchars = "0123456789abcdef"
let rand_string st = String.init (Random.State.int st 11) (fun _ -> Char.chr (Char.code 'a' + Random.State.int st 26))
let rand_hex24 st = String.init 24 (fun _ -> hexchars.[Random.State.int st 16])

let rec gen : type a. a shape -> Random.State.t -> int -> a =
 fun shape st fuel ->
  match shape with
  | TString -> rand_string st
  | TInt -> Random.State.int st 2001 - 1000
  | TFloat _ -> (Random.State.float st 2000.) -. 1000.
  | TBool -> Random.State.bool st
  | TDate -> Random.State.int64 st 1_700_000_000_000L
  | TId -> rand_hex24 st
  | TBson -> Bson.Int (Random.State.int st 1000)
  | TUnit -> ()
  | TList el -> let n = if fuel <= 0 then 0 else Random.State.int st (min 6 (fuel + 1)) in List.init n (fun _ -> gen el st (fuel - 1))
  | TOption el -> if fuel > 0 && Random.State.bool st then Some (gen el st (fuel - 1)) else None
  | TMap el -> let n = if fuel <= 0 then 0 else Random.State.int st 4 in List.init n (fun i -> (Printf.sprintf "k%d" i, gen el st (fuel - 1)))
  | TCheck (p, _, _, inner) -> gen_reject p inner st fuel 20
  | TNorm (f, inner) -> f (gen inner st fuel)
  | TConv (_, proj, inner) -> gen_conv proj inner st fuel 20
  | TCoerce inner -> gen inner st fuel
  | TLazy l -> gen (Lazy.force l) st (fuel - 1)
  | TObj o -> ( match o.decode_src { read_field = (fun f -> Ok (gen f.item st (fuel - 1))) } with Ok v -> v | Error _ -> invalid_arg "Sift.arbitrary: record")
  | TVariant { cases; _ } ->
      let arr = Array.of_list cases in
      let (Case c) = arr.(Random.State.int st (Array.length arr)) in
      ( match c.body.decode_src { read_field = (fun f -> Ok (gen f.item st (fuel - 1))) } with Ok a -> c.inject a | Error _ -> invalid_arg "Sift.arbitrary: variant")

and gen_reject : type a. (a -> bool) -> a shape -> Random.State.t -> int -> int -> a =
 fun p inner st fuel tries -> let v = gen inner st fuel in if tries <= 0 || p (Engine.normalize inner v) then v else gen_reject p inner st fuel (tries - 1)

and gen_conv : type a b. (a -> (b, string) result) -> a shape -> Random.State.t -> int -> int -> b =
 fun proj inner st fuel tries -> match proj (gen inner st fuel) with Ok b -> b | Error _ when tries > 0 -> gen_conv proj inner st fuel (tries - 1) | Error m -> invalid_arg ("Sift.arbitrary: conversion rejects generated values: " ^ m)

let arbitrary : type a. a shape -> Random.State.t -> a = fun shape st -> gen shape st 4
