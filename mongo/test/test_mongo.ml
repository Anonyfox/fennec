(* Tests for the fennec-mongo pure trio — BSON values, the query engine, and Minimongo — using
   fennec's inline test tooling. [let%test] for examples, edge cases, and regressions; [let%prop]
   for the laws (sort orders + permutes, $set-then-read, insert-then-find, diff/merge identities).
   Many cases here are regressions for bugs an audit surfaced (array-aware matching, numeric
   equality, $inc on Int64/non-numeric, $mod, $elemMatch operators, $pull by value, upsert seeds,
   re-entrant-observer safety, insertion-order/clear). The libraries stay dependency-free; the
   tooling lives here. *)

module C = Minimongo
open Query

let d = Bson.doc
let i = Bson.int

(* ── Bson: access + new DX ── *)
let%test "get reads a top-level field" = Bson.get (d [ ("a", i 1) ]) "a" = Some (Bson.Int 1)
let%test "get of a missing field is None" = Bson.get (d [ ("a", i 1) ]) "b" = None
let%test "get of a non-document is None" = Bson.get (Bson.str "x") "a" = None
let%test "fields returns the field list" = Bson.fields (d [ ("a", i 1) ]) = [ ("a", Bson.Int 1) ]
let%test "typed accessors" =
  let r = d [ ("s", Bson.str "x"); ("n", i 3); ("f", Bson.float 1.5); ("b", Bson.bool true); ("l", Bson.array [ i 1 ]) ] in
  Bson.get_string r "s" = Some "x"
  && Bson.get_int r "n" = Some 3
  && Bson.get_float r "f" = Some 1.5
  && Bson.get_bool r "b" = Some true
  && Bson.get_list r "l" = Some [ Bson.Int 1 ]
let%test "object_id_of_string validates 24 hex" =
  Bson.object_id_of_string (String.make 24 'a') = Some (Bson.Object_id (String.make 24 'a'))
  && Bson.object_id_of_string "too-short" = None
  && Bson.object_id_of_string (String.make 24 'z') = None
let%test "to_string renders a document" = Bson.to_string (d [ ("a", i 1) ]) = "{a: 1}"

(* ── Bson: equality & ordering ── *)
let%test "equal across numeric types" =
  Bson.equal (Bson.int 1) (Bson.float 1.0) && Bson.equal (Bson.int 1) (Bson.int64 1L)
let%test "document equality is order-sensitive" =
  not (Bson.equal (d [ ("a", i 1); ("b", i 2) ]) (d [ ("b", i 2); ("a", i 1) ]))
let%test "nan is not equal to itself" = not (Bson.equal (Bson.float nan) (Bson.float nan))
let%test "compare orders by BSON type precedence (number before string)" =
  Bson.compare (i 1) (Bson.str "a") < 0 && Bson.compare (Bson.str "a") (i 1) > 0

(* ── Id ── *)
let%test "object_id is 24 hex chars" =
  let s = Id.object_id () in
  String.length s = 24 && String.for_all (fun c -> String.contains "0123456789abcdef" c) s
let%prop "random_id has the requested length" = fun (n : int) ->
  let n = n land 63 in
  String.length (Id.random_id ~n ()) = n
let%prop "random_id uses only unmistakable characters" = fun (n : int) ->
  let n = n land 31 in
  String.for_all
    (fun c -> String.contains "23456789ABCDEFGHJKLMNPQRSTWXYZabcdefghijkmnopqrstuvwxyz" c)
    (Id.random_id ~n ())

(* ── Matcher ── *)
let%test "empty selector matches any document" = Matcher.doc_matches (d []) (d [ ("a", i 1) ])
let%test "equality selector matches" = Matcher.doc_matches (d [ ("a", i 1) ]) (d [ ("a", i 1) ])
let%test "equality selector rejects a mismatch" =
  not (Matcher.doc_matches (d [ ("a", i 2) ]) (d [ ("a", i 1) ]))
let%test "equality matches across numeric types" =
  Matcher.doc_matches (d [ ("n", i 1) ]) (d [ ("n", Bson.int64 1L) ])
  && Matcher.doc_matches (d [ ("n", Bson.float 1.0) ]) (d [ ("n", i 1) ])
let%test "$gt operator" = Matcher.doc_matches (d [ ("n", d [ ("$gt", i 5) ]) ]) (d [ ("n", i 7) ])
let%test "range query is type-scoped (number query never matches a string)" =
  not (Matcher.doc_matches (d [ ("n", d [ ("$gt", i 5) ]) ]) (d [ ("n", Bson.str "9") ]))
let%test "$or operator" =
  Matcher.doc_matches
    (d [ ("$or", Bson.Array [ d [ ("a", i 1) ]; d [ ("b", i 2) ] ]) ])
    (d [ ("b", i 2) ])
let%test "get_path walks nested documents" =
  Matcher.get_path (d [ ("a", d [ ("b", i 5) ]) ]) "a.b" = Some (Bson.Int 5)
let%test "scalar selector matches an array field element" =
  Matcher.doc_matches (d [ ("tags", Bson.str "a") ]) (d [ ("tags", Bson.array [ Bson.str "a"; Bson.str "b" ]) ])
let%test "$in is array-aware" =
  Matcher.doc_matches
    (d [ ("tags", d [ ("$in", Bson.array [ Bson.str "x" ]) ]) ])
    (d [ ("tags", Bson.array [ Bson.str "x"; Bson.str "y" ]) ])
let%test "$elemMatch with an operator predicate" =
  Matcher.doc_matches (d [ ("arr", d [ ("$elemMatch", d [ ("$gt", i 5) ]) ]) ]) (d [ ("arr", Bson.array [ i 1; i 7 ]) ])
let%test "$type understands the number umbrella and long" =
  Matcher.doc_matches (d [ ("n", d [ ("$type", Bson.str "number") ]) ]) (d [ ("n", i 5) ])
  && Matcher.doc_matches (d [ ("n", d [ ("$type", Bson.str "long") ]) ]) (d [ ("n", Bson.int64 5L) ])
let%test "$exists false matches a missing field" =
  Matcher.doc_matches (d [ ("a", d [ ("$exists", Bson.bool false) ]) ]) (d [ ("b", i 1) ])
  && not (Matcher.doc_matches (d [ ("a", d [ ("$exists", Bson.bool false) ]) ]) (d [ ("a", i 1) ]))
let%test "$mod uses the numeric value" =
  Matcher.doc_matches (d [ ("n", d [ ("$mod", Bson.array [ i 3; i 1 ]) ]) ]) (d [ ("n", i 7) ])
  && not (Matcher.doc_matches (d [ ("n", d [ ("$mod", Bson.array [ i 3; i 1 ]) ]) ]) (d [ ("n", i 8) ]))
let%prop "a document matches its own equality selector" = fun (n : int) ->
  Matcher.doc_matches (d [ ("v", i n) ]) (d [ ("v", i n) ])

(* ── Modifier ── *)
let%prop "$set then read returns the value" = fun (n : int) ->
  let r = Modifier.apply (d []) (d [ ("$set", d [ ("x", i n) ]) ]) in
  Bson.get r "x" = Some (Bson.Int n)
let%prop "$inc from absent yields the increment" = fun (n : int) ->
  let r = Modifier.apply (d []) (d [ ("$inc", d [ ("c", i n) ]) ]) in
  Bson.get r "c" = Some (Bson.Int n)
let%test "$inc preserves Int64 and adds numerically" =
  match Bson.get (Modifier.apply (d [ ("x", Bson.int64 10L) ]) (d [ ("$inc", d [ ("x", i 5) ]) ])) "x" with
  | Some (Bson.Int64 n) -> n = 15L
  | _ -> false
let%test "$inc leaves a non-numeric field unchanged (no clobber)" =
  Bson.get (Modifier.apply (d [ ("x", Bson.str "hi") ]) (d [ ("$inc", d [ ("x", i 5) ]) ])) "x"
  = Some (Bson.String "hi")
let%test "$mul of a missing field yields zero" =
  Bson.get (Modifier.apply (d []) (d [ ("$mul", d [ ("x", i 5) ]) ])) "x" = Some (Bson.Int 0)
let%test "$unset removes a field" =
  Bson.get (Modifier.apply (d [ ("a", i 1) ]) (d [ ("$unset", d [ ("a", i 1) ]) ])) "a" = None
let%test "$pull removes array elements equal to a document" =
  let r = Modifier.apply (d [ ("a", Bson.array [ d [ ("x", i 1) ]; d [ ("x", i 2) ] ]) ]) (d [ ("$pull", d [ ("a", d [ ("x", i 1) ]) ]) ]) in
  Bson.get r "a" = Some (Bson.array [ d [ ("x", i 2) ] ])
let%test "$pull removes array elements matching an operator predicate" =
  let r = Modifier.apply (d [ ("a", Bson.array [ i 1; i 5; i 10 ]) ]) (d [ ("$pull", d [ ("a", d [ ("$gt", i 4) ]) ]) ]) in
  Bson.get r "a" = Some (Bson.array [ i 1 ])
let%test "a non-operator modifier replaces, preserving _id" =
  let r = Modifier.apply (d [ ("_id", Bson.str "k"); ("a", i 1) ]) (d [ ("b", i 2) ]) in
  Bson.get r "_id" = Some (Bson.String "k")
  && Bson.get r "b" = Some (Bson.Int 2)
  && Bson.get r "a" = None
let%test "$bit or/and/xor" =
  let bit cur op n =
    Bson.get (Modifier.apply (d [ ("f", i cur) ]) (d [ ("$bit", d [ ("f", d [ (op, i n) ]) ]) ])) "f"
  in
  bit 1 "or" 4 = Some (Bson.Int 5) && bit 7 "and" 1 = Some (Bson.Int 1) && bit 5 "xor" 1 = Some (Bson.Int 4)
let%test "$push with $each + $sort + $slice" =
  let r =
    Modifier.apply (d [ ("a", Bson.array []) ])
      (d [ ("$push", d [ ("a", d [ ("$each", Bson.array [ i 3; i 1; i 2 ]); ("$sort", i 1); ("$slice", i 2) ]) ]) ])
  in
  Bson.get r "a" = Some (Bson.array [ i 1; i 2 ])
let%test "$push with $position inserts at an index" =
  let r =
    Modifier.apply (d [ ("a", Bson.array [ i 1; i 2 ]) ])
      (d [ ("$push", d [ ("a", d [ ("$each", Bson.array [ i 9 ]); ("$position", i 0) ]) ]) ])
  in
  Bson.get r "a" = Some (Bson.array [ i 9; i 1; i 2 ])

(* ── Projection ── *)
let%test "include keeps the listed fields and _id" =
  let p = Projection.of_fields (d [ ("a", i 1) ]) in
  Projection.apply p (d [ ("_id", Bson.str "x"); ("a", i 1); ("b", i 2) ])
  = d [ ("_id", Bson.str "x"); ("a", i 1) ]
let%test "exclude drops the listed fields" =
  let p = Projection.of_fields (d [ ("b", i 0) ]) in
  Projection.apply p (d [ ("a", i 1); ("b", i 2) ]) = d [ ("a", i 1) ]
let%test "nested projection keeps only the dotted path" =
  let p = Projection.of_fields (d [ ("a.b", i 1) ]) in
  Projection.apply p (d [ ("_id", Bson.str "x"); ("a", d [ ("b", i 1); ("c", i 2) ]); ("e", i 3) ])
  = d [ ("_id", Bson.str "x"); ("a", d [ ("b", i 1) ]) ]
let%test "nested exclusion drops only the dotted path" =
  let p = Projection.of_fields (d [ ("a.b", i 0) ]) in
  Projection.apply p (d [ ("a", d [ ("b", i 1); ("c", i 2) ]) ]) = d [ ("a", d [ ("c", i 2) ]) ]
let%test "$slice limits an array projection" =
  let p = Projection.of_fields (d [ ("arr", d [ ("$slice", i 2) ]) ]) in
  Projection.apply p (d [ ("arr", Bson.array [ i 1; i 2; i 3 ]) ]) = d [ ("arr", Bson.array [ i 1; i 2 ]) ]
let%test "$elemMatch projects the first matching element" =
  let p = Projection.of_fields (d [ ("arr", d [ ("$elemMatch", d [ ("x", d [ ("$gt", i 1) ]) ]) ]) ]) in
  Projection.apply p (d [ ("arr", Bson.array [ d [ ("x", i 1) ]; d [ ("x", i 2) ] ]) ])
  = d [ ("arr", Bson.array [ d [ ("x", i 2) ] ]) ]

(* ── Bitwise query operators ── *)
let%test "$bitsAllSet by bit positions" =
  Matcher.doc_matches (d [ ("n", d [ ("$bitsAllSet", Bson.array [ i 1; i 2 ]) ]) ]) (d [ ("n", i 6) ])
let%test "$bitsAnyClear by bit positions" =
  Matcher.doc_matches (d [ ("n", d [ ("$bitsAnyClear", Bson.array [ i 0; i 1 ]) ]) ]) (d [ ("n", i 6) ])
let%test "$bitsAllSet by mask" =
  Matcher.doc_matches (d [ ("n", d [ ("$bitsAllSet", i 6) ]) ]) (d [ ("n", i 6) ])
  && not (Matcher.doc_matches (d [ ("n", d [ ("$bitsAllSet", i 8) ]) ]) (d [ ("n", i 6) ]))

(* ── $regex ── *)
let%test "$regex matches a pattern (anchored)" =
  Matcher.doc_matches (d [ ("name", d [ ("$regex", Bson.str "^foo") ]) ]) (d [ ("name", Bson.str "foobar") ])
  && not (Matcher.doc_matches (d [ ("name", d [ ("$regex", Bson.str "^foo") ]) ]) (d [ ("name", Bson.str "barfoo") ]))
let%test "$regex honors the case-insensitive option" =
  Matcher.doc_matches
    (d [ ("name", d [ ("$regex", Bson.str "FOO"); ("$options", Bson.str "i") ]) ])
    (d [ ("name", Bson.str "foobar") ])

(* ── Geospatial ── *)
let pt x y = Bson.array [ Bson.float x; Bson.float y ]
let geojson_pt x y = d [ ("type", Bson.str "Point"); ("coordinates", pt x y) ]
let square =
  d [ ("type", Bson.str "Polygon");
      ("coordinates", Bson.array [ Bson.array [ pt 0. 0.; pt 0. 1.; pt 1. 1.; pt 1. 0.; pt 0. 0. ] ]) ]

let%test "$geoWithin $box" =
  let q = d [ ("loc", d [ ("$geoWithin", d [ ("$box", Bson.array [ pt 0. 0.; pt 2. 2. ]) ]) ]) ] in
  Matcher.doc_matches q (d [ ("loc", pt 1. 1.) ]) && not (Matcher.doc_matches q (d [ ("loc", pt 3. 3.) ]))
let%test "$geoWithin polygon (point in polygon)" =
  let q = d [ ("loc", d [ ("$geoWithin", d [ ("$geometry", square) ]) ]) ] in
  Matcher.doc_matches q (d [ ("loc", pt 0.5 0.5) ]) && not (Matcher.doc_matches q (d [ ("loc", pt 2. 2.) ]))
let%test "$geoWithin $centerSphere (angular radius)" =
  let q r = d [ ("loc", d [ ("$geoWithin", d [ ("$centerSphere", Bson.array [ pt 0. 0.; Bson.float r ]) ]) ]) ] in
  Matcher.doc_matches (q 0.02) (d [ ("loc", pt 1. 0.) ])
  && not (Matcher.doc_matches (q 0.001) (d [ ("loc", pt 1. 0.) ]))
let%test "$near distance filter (metres)" =
  let near maxd =
    d [ ("loc", d [ ("$near", d [ ("$geometry", geojson_pt 0. 0.); ("$maxDistance", Bson.float maxd) ]) ]) ]
  in
  (* a point at lng 1 is ~111 km from the origin on the equator *)
  Matcher.doc_matches (near 200000.) (d [ ("loc", pt 1. 0.) ])
  && not (Matcher.doc_matches (near 1000.) (d [ ("loc", pt 1. 0.) ]))
let%test "$geoIntersects (polygon contains a GeoJSON point)" =
  let q = d [ ("loc", d [ ("$geoIntersects", d [ ("$geometry", square) ]) ]) ] in
  Matcher.doc_matches q (d [ ("loc", geojson_pt 0.5 0.5) ])

(* ── Sorter ── *)
let vals docs =
  List.filter_map (fun x -> match Bson.get x "v" with Some (Bson.Int n) -> Some n | _ -> None) docs

let%test "cross-type sort follows BSON type precedence (number before string)" =
  let docs = [ d [ ("v", Bson.str "a") ]; d [ ("v", i 1) ] ] in
  match Sorter.sort (d [ ("v", i 1) ]) docs with
  | first :: _ -> Bson.get first "v" = Some (Bson.Int 1)
  | [] -> false
let%prop "sort preserves length (it is a permutation)" = fun (xs : int list) ->
  let docs = List.map (fun n -> d [ ("v", i n) ]) xs in
  List.length (Sorter.sort (d [ ("v", i 1) ]) docs) = List.length docs
let%prop "ascending sort yields ordered values" = fun (xs : int list) ->
  let docs = List.map (fun n -> d [ ("v", i n) ]) xs in
  let out = vals (Sorter.sort (d [ ("v", i 1) ]) docs) in
  out = List.sort compare out
let%prop "descending sort yields reverse-ordered values" = fun (xs : int list) ->
  let docs = List.map (fun n -> d [ ("v", i n) ]) xs in
  let out = vals (Sorter.sort (d [ ("v", i (-1)) ]) docs) in
  out = List.rev (List.sort compare out)

(* ── Diff ── *)
let%test "transition classifies membership" =
  Diff.transition ~was:false ~now:true = Diff.Entered
  && Diff.transition ~was:true ~now:false = Diff.Left
(* a WELL-FORMED document (unique top-level keys); duplicate keys aren't valid BSON. *)
let doc_of_keys keys =
  d
    (List.fold_left
       (fun acc n ->
         let k = string_of_int n in
         if List.mem_assoc k acc then acc else acc @ [ (k, i n) ])
       [] keys)
let%prop "diff of a document against itself is empty" = fun (keys : int list) ->
  let doc = doc_of_keys keys in
  Diff.diff_fields ~old_doc:doc ~new_doc:doc = ([], [])
let%prop "merge with no update is the identity" = fun (keys : int list) ->
  let doc = doc_of_keys keys in
  Diff.merge_doc doc ~updated:[] ~removed:[] = doc

(* ── Minimongo ── *)
let%prop "insert then find finds the document" = fun (n : int) ->
  let c = C.create () in
  let _ = C.insert c (d [ ("v", i n) ]) in
  C.count (C.find c ~selector:(d [ ("v", i n) ]) ()) = 1
let%prop "insert then remove leaves the collection empty" = fun (n : int) ->
  let c = C.create () in
  let _ = C.insert c (d [ ("v", i n) ]) in
  C.remove c (d [ ("v", i n) ]) = 1 && C.count (C.find c ()) = 0
let%test "insertion order is preserved across many inserts" =
  let c = C.create () in
  List.iter (fun n -> ignore (C.insert c (d [ ("v", i n) ]))) [ 0; 1; 2; 3; 4 ];
  vals (C.fetch (C.find c ())) = [ 0; 1; 2; 3; 4 ]
let%test "remove of an empty selector clears the collection" =
  let c = C.create () in
  List.iter (fun n -> ignore (C.insert c (d [ ("v", i n) ]))) [ 1; 2; 3 ];
  C.remove c (d []) = 3 && C.count (C.find c ()) = 0
let%test "remove_id removes exactly that one document, keeps the rest and their order" =
  let c = C.create () in
  let a = C.insert c (d [ ("v", i 1) ]) in
  let _ = C.insert c (d [ ("v", i 2) ]) in
  let _ = C.insert c (d [ ("v", i 3) ]) in
  C.remove_id c a (* present → true *)
  && (not (C.remove_id c a)) (* already gone → false *)
  && vals (C.fetch (C.find c ())) = [ 2; 3 ]
let%test "find_one and is_empty" =
  let c = C.create () in
  C.is_empty (C.find c ())
  &&
  (let _ = C.insert c (d [ ("v", i 1) ]) in
   (not (C.is_empty (C.find c ())))
   && match C.find_one c ~selector:(d [ ("v", i 1) ]) () with Some _ -> true | None -> false)
let%test "update $set mutates the stored document" =
  let c = C.create () in
  let id = C.insert c (d [ ("v", i 1) ]) in
  let _ = C.update c (d [ ("_id", Bson.str id) ]) (d [ ("$set", d [ ("v", i 2) ]) ]) in
  (match C.find_one c ~selector:(d [ ("_id", Bson.str id) ]) () with
   | Some doc -> Bson.get doc "v" = Some (Bson.Int 2)
   | None -> false)
let%test "upsert seeds embedded-document equality fields" =
  let c = C.create () in
  let _ = C.update c ~upsert:true (d [ ("profile", d [ ("name", Bson.str "x") ]) ]) (d [ ("$set", d [ ("ok", i 1) ]) ]) in
  (match C.find_one c ~selector:(d [ ("profile", d [ ("name", Bson.str "x") ]) ]) () with
   | Some doc -> Bson.get doc "ok" = Some (Bson.Int 1)
   | None -> false)
let%test "observe_changes fires added on insert" =
  let c = C.create () in
  let added = ref 0 in
  let h = C.observe_changes (C.find c ()) ~added:(fun _ _ -> incr added) () in
  let _ = C.insert c (d [ ("v", i 1) ]) in
  h.C.stop ();
  !added = 1
let%test "re-entrant removal during a notification is failsafe (no exception)" =
  let c = C.create () in
  let a = C.insert c (d [ ("k", i 1) ]) in
  let _b = C.insert c (d [ ("k", i 1) ]) in
  (* when a is removed, the observer cascades a removal of a sibling that is still in the outer
     remove's snapshot — the old code raised Not_found here; total lookups make it safe. *)
  let h =
    C.watch c (fun ch ->
        if ch.C.op = C.Remove && ch.C.id = a then ignore (C.remove c (d [ ("k", i 1) ])))
  in
  let ok = try ignore (C.remove c (d [ ("k", i 1) ])); true with _ -> false in
  h.C.stop ();
  ok && C.count (C.find c ()) = 0

(* ── Aggregation: expressions ── *)
let%test "Expr arithmetic over field paths" =
  Expr.eval (d [ ("$add", Bson.array [ i 1; i 2; Bson.str "$x" ]) ]) (d [ ("x", i 3) ]) = Bson.Int 6
let%test "Expr $cond" =
  Expr.eval
    (d [ ("$cond", Bson.array [ d [ ("$gt", Bson.array [ Bson.str "$x"; i 1 ]) ]; Bson.str "big"; Bson.str "small" ]) ])
    (d [ ("x", i 5) ])
  = Bson.String "big"
let%test "Expr $map binds $$this" =
  Expr.eval
    (d [ ("$map", d [ ("input", Bson.str "$xs"); ("in", d [ ("$multiply", Bson.array [ Bson.str "$$this"; i 2 ]) ]) ]) ])
    (d [ ("xs", Bson.array [ i 1; i 2; i 3 ]) ])
  = Bson.array [ i 2; i 4; i 6 ]

(* ── Aggregation: pipeline ── *)
let sales =
  [ d [ ("_id", i 1); ("cat", Bson.str "a"); ("qty", i 2) ];
    d [ ("_id", i 2); ("cat", Bson.str "a"); ("qty", i 3) ];
    d [ ("_id", i 3); ("cat", Bson.str "b"); ("qty", i 5) ] ]

let%test "$match + $project" =
  Aggregate.run
    [ d [ ("$match", d [ ("cat", Bson.str "a") ]) ]; d [ ("$project", d [ ("qty", i 1); ("_id", i 0) ]) ] ]
    sales
  = [ d [ ("qty", i 2) ]; d [ ("qty", i 3) ] ]
let%test "$group with $sum" =
  Aggregate.run [ d [ ("$group", d [ ("_id", Bson.str "$cat"); ("total", d [ ("$sum", Bson.str "$qty") ]) ]) ] ] sales
  = [ d [ ("_id", Bson.str "a"); ("total", i 5) ]; d [ ("_id", Bson.str "b"); ("total", i 5) ] ]
let%test "$sort + $limit" =
  match Aggregate.run [ d [ ("$sort", d [ ("qty", i (-1)) ]) ]; d [ ("$limit", i 1) ] ] sales with
  | [ top ] -> Bson.get top "qty" = Some (Bson.Int 5)
  | _ -> false
let%test "$addFields computes a field" =
  match Aggregate.run [ d [ ("$addFields", d [ ("doubled", d [ ("$multiply", Bson.array [ Bson.str "$qty"; i 2 ]) ]) ]) ] ] [ List.hd sales ] with
  | [ x ] -> Bson.get x "doubled" = Some (Bson.Int 4)
  | _ -> false
let%test "$count" = Aggregate.run [ d [ ("$count", Bson.str "n") ] ] sales = [ d [ ("n", i 3) ] ]
let%test "$unwind deconstructs an array" =
  let docs = [ d [ ("_id", i 1); ("tags", Bson.array [ Bson.str "x"; Bson.str "y" ]) ] ] in
  Aggregate.run [ d [ ("$unwind", Bson.str "$tags") ] ] docs
  = [ d [ ("_id", i 1); ("tags", Bson.str "x") ]; d [ ("_id", i 1); ("tags", Bson.str "y") ] ]
let%test "$lookup joins a foreign collection" =
  let orders = [ d [ ("_id", i 1); ("cust", i 7) ] ] in
  let customers = [ d [ ("_id", i 7); ("name", Bson.str "Ada") ] ] in
  let lookup = function "customers" -> customers | _ -> [] in
  match
    Aggregate.run ~lookup
      [ d [ ("$lookup", d [ ("from", Bson.str "customers"); ("localField", Bson.str "cust"); ("foreignField", Bson.str "_id"); ("as", Bson.str "c") ]) ] ]
      orders
  with
  | [ o ] -> Bson.get o "c" = Some (Bson.array [ d [ ("_id", i 7); ("name", Bson.str "Ada") ] ])
  | _ -> false
let%test "Minimongo.aggregate over a collection" =
  let c = C.create () in
  List.iter (fun doc -> ignore (C.insert c doc)) sales;
  C.aggregate c [ d [ ("$group", d [ ("_id", Bson.str "$cat"); ("n", d [ ("$sum", i 1) ]) ]) ] ]
  = [ d [ ("_id", Bson.str "a"); ("n", i 2) ]; d [ ("_id", Bson.str "b"); ("n", i 1) ] ]

(* ── Aggregation: robustness regressions (audit findings) ── *)
let%test "$group on a NaN _id does not crash (groups NaN together)" =
  let docs = [ d [ ("x", Bson.float nan) ]; d [ ("x", Bson.float nan) ] ] in
  match Aggregate.run [ d [ ("$group", d [ ("_id", Bson.str "$x"); ("n", d [ ("$sum", i 1) ]) ]) ] ] docs with
  | [ g ] -> Bson.get g "n" = Some (Bson.Int 2)
  | _ -> false
let%test "$arrayElemAt out-of-range is Null, never raises" =
  Expr.eval (d [ ("$arrayElemAt", Bson.array [ Bson.array [ i 1; i 2 ]; i (-100) ]) ]) (d []) = Bson.Null
  && Expr.eval (d [ ("$arrayElemAt", Bson.array [ Bson.array [ i 1; i 2 ]; i 9 ]) ]) (d []) = Bson.Null
let%test "$cond with a missing branch is Null, never raises" =
  Expr.eval (d [ ("$cond", d [ ("if", Bson.bool true); ("then", i 5) ]) ]) (d []) = Bson.Int 5
  && Expr.eval (d [ ("$cond", d [ ("if", Bson.bool false); ("then", i 5) ]) ]) (d []) = Bson.Null
let%test "$toInt of a non-finite number is Null" =
  Expr.eval (d [ ("$toInt", Bson.array [ Bson.float infinity ]) ]) (d []) = Bson.Null
let%test "$unwind preserveNullAndEmptyArrays keeps empty/missing" =
  let docs = [ d [ ("_id", i 1); ("tags", Bson.array []) ]; d [ ("_id", i 2) ] ] in
  List.length
    (Aggregate.run
       [ d [ ("$unwind", d [ ("path", Bson.str "$tags"); ("preserveNullAndEmptyArrays", Bson.bool true) ]) ] ]
       docs)
  = 2

(* ── windowed observe: sort+limit/skip windows are maintained LIVE ── *)
let%test "windowed observe_changes: a sorted+limited window is maintained live (enter/displace/promote, no leak)" =
  let c = C.create () in
  let add id s = ignore (C.insert c (d [ ("_id", Bson.str id); ("score", i s) ])) in
  add "a" 30; add "b" 20; add "k" 10;
  let live = Hashtbl.create 8 in
  let events = ref 0 in
  let h =
    C.observe_changes (C.find c ~sort:(d [ ("score", i (-1)) ]) ~limit:3 ())
      ~added:(fun id _ -> Hashtbl.replace live id (); incr events)
      ~removed:(fun id -> Hashtbl.remove live id; incr events)
      ()
  in
  let init_ok = Hashtbl.length live = 3 in
  (* a higher-ranked doc enters → the boundary doc ("k") is displaced; the window stays at 3 *)
  add "d" 40;
  let displaced = Hashtbl.mem live "d" && (not (Hashtbl.mem live "k")) && Hashtbl.length live = 3 in
  (* a doc below the window → membership unchanged *)
  add "e" 5;
  let no_leak = Hashtbl.length live = 3 && not (Hashtbl.mem live "e") in
  (* the top doc drops out by score → it leaves, "k" promotes back in *)
  ignore (C.update c (d [ ("_id", Bson.str "a") ]) (d [ ("$set", d [ ("score", i 1) ]) ]));
  let promoted = (not (Hashtbl.mem live "a")) && Hashtbl.mem live "k" && Hashtbl.length live = 3 in
  (* removing a window doc promotes the next-best ("e" beats a=1) *)
  ignore (C.remove_id c "d");
  let refilled = (not (Hashtbl.mem live "d")) && Hashtbl.mem live "e" && Hashtbl.length live = 3 in
  h.C.stop ();
  init_ok && displaced && no_leak && promoted && refilled && !events > 0

let%test "windowed observe_changes: a skip window shifts when a doc enters ahead of it" =
  let c = C.create () in
  let add id s = ignore (C.insert c (d [ ("_id", Bson.str id); ("v", i s) ])) in
  add "one" 1; add "two" 2; add "three" 3;
  let live = Hashtbl.create 8 in
  let h =
    C.observe_changes (C.find c ~sort:(d [ ("v", i 1) ]) ~skip:1 ~limit:2 ())
      ~added:(fun id _ -> Hashtbl.replace live id ())
      ~removed:(fun id -> Hashtbl.remove live id)
      ()
  in
  (* initial window (skip the lowest): two, three *)
  let init_ok = Hashtbl.mem live "two" && Hashtbl.mem live "three" && Hashtbl.length live = 2 in
  (* a new lowest doc shifts the window: skip "zero" → window = one, two ("three" leaves, "one" enters) *)
  add "zero" 0;
  let shifted = Hashtbl.mem live "one" && Hashtbl.mem live "two" && (not (Hashtbl.mem live "three")) && Hashtbl.length live = 2 in
  h.C.stop ();
  init_ok && shifted

let%test "windowed observe_changes: a PREFIX doc's removal / selector-exit shifts a skip window" =
  (* the skipped prefix interacts with the window: removing (or selector-evicting) a prefix doc pulls
     the first window doc up into the prefix and promotes the next-below-boundary doc in *)
  let c = C.create () in
  let add id s = ignore (C.insert c (d [ ("_id", Bson.str id); ("v", i s); ("t", Bson.str "x") ])) in
  add "one" 1; add "two" 2; add "three" 3; add "four" 4; add "five" 5;
  let live = Hashtbl.create 8 in
  let h =
    C.observe_changes
      (C.find c ~selector:(d [ ("t", Bson.str "x") ]) ~sort:(d [ ("v", i 1) ]) ~skip:1 ~limit:2 ())
      ~added:(fun id _ -> Hashtbl.replace live id ())
      ~removed:(fun id -> Hashtbl.remove live id)
      ()
  in
  (* window = skip "one" → {two, three} *)
  let init_ok = Hashtbl.mem live "two" && Hashtbl.mem live "three" && Hashtbl.length live = 2 in
  (* REMOVE the prefix doc → order two,three,four,five; skip 1 → {three, four} *)
  ignore (C.remove_id c "one");
  let after_remove = Hashtbl.mem live "three" && Hashtbl.mem live "four" && (not (Hashtbl.mem live "two")) in
  (* move the NEW prefix doc ("two") out of the selector → order three,four,five; skip 1 → {four, five} *)
  ignore (C.update c (d [ ("_id", Bson.str "two") ]) (d [ ("$set", d [ ("t", Bson.str "y") ]) ]));
  let after_exit = Hashtbl.mem live "four" && Hashtbl.mem live "five" && (not (Hashtbl.mem live "three")) in
  h.C.stop ();
  init_ok && after_remove && after_exit

let%test "windowed observe_changes: boundary edges — a tie re-evaluates, an update can re-enter, a non-full window admits" =
  let c = C.create () in
  let add id s = ignore (C.insert c (d [ ("_id", Bson.str id); ("score", i s) ])) in
  add "a" 30; add "b" 20;
  let live = Hashtbl.create 8 in
  let h =
    C.observe_changes (C.find c ~sort:(d [ ("score", i (-1)) ]) ~limit:3 ())
      ~added:(fun id _ -> Hashtbl.replace live id ())
      ~removed:(fun id -> Hashtbl.remove live id)
      ()
  in
  (* the window is NOT full (2 of 3) → even the lowest doc must be admitted (no short-circuit) *)
  add "k" 1;
  let admits = Hashtbl.mem live "k" && Hashtbl.length live = 3 in
  (* a strictly-below doc with a FULL window stays out (the short-circuit path) *)
  add "low" 0;
  let stays_out = (not (Hashtbl.mem live "low")) && Hashtbl.length live = 3 in
  (* a TIE with the boundary ("k" = 1) re-evaluates safely (no corruption, window stays 3) *)
  add "tie" 1;
  let tie_safe = Hashtbl.length live = 3 in
  (* the below-boundary doc UPDATED above the top must enter (the short-circuit must not blind us) *)
  ignore (C.update c (d [ ("_id", Bson.str "low") ]) (d [ ("$set", d [ ("score", i 99) ]) ]));
  let re_enters = Hashtbl.mem live "low" && Hashtbl.length live = 3 in
  h.C.stop ();
  admits && stays_out && tie_safe && re_enters

(* ── Fanout: the ordered fan-out monitor the change stream rides on ── *)
module F = C.Fanout

let%test "fanout: events deliver in publish order; a dead sub is skipped" =
  let f = F.create () in
  let got = ref [] in
  let s1 = F.subscribe f ~ready:true (fun e -> got := e :: !got) in
  let s2 = F.subscribe f ~ready:true (fun _ -> ()) in
  F.publish f 1;
  F.publish f 2;
  F.unsubscribe f s2;
  F.publish f 3;
  F.unsubscribe f s1;
  F.publish f 4;
  (* delivered only while alive, in order *)
  List.rev !got = [ 1; 2; 3 ] && F.count f = 0

let%test "fanout: a buffering sub holds events (in order) until ready flushes them" =
  let f = F.create () in
  let got = ref [] in
  let s = F.subscribe f ~ready:false (fun e -> got := e :: !got) in
  F.publish f 1;
  F.publish f 2;
  let before_ready = !got = [] in
  F.ready f s;
  F.publish f 3;
  before_ready && List.rev !got = [ 1; 2; 3 ]

let%test "fanout: a re-entrant publish from inside a delivery is delivered after, not lost (single drainer)" =
  let f = F.create () in
  let got = ref [] in
  let fired = ref false in
  let _ =
    F.subscribe f ~ready:true (fun e ->
        got := e :: !got;
        if not !fired then begin
          fired := true;
          F.publish f 99 (* re-entrant: we ARE the drainer; this enqueues and is drained next *)
        end)
  in
  F.publish f 1;
  List.rev !got = [ 1; 99 ]

(* ── multicore: domains hammering one collection stay linearized and uncorrupted ── *)
let%test "multicore: concurrent domain writers + a live observer — exact final state, legal transitions" =
  let c = C.create () in
  (* per-id transition legality: added → (changed* ) → removed, never out of order, tracked by the
     observer's own view (Adds/Removes balance per id) *)
  let seen : (string, int) Hashtbl.t = Hashtbl.create 64 in
  let illegal = ref 0 in
  let obs_lock = Mutex.create () in
  let on_added id =
    Mutex.lock obs_lock;
    (match Hashtbl.find_opt seen id with
    | Some n when n > 0 -> incr illegal (* added while already present *)
    | _ -> Hashtbl.replace seen id 1);
    Mutex.unlock obs_lock
  in
  let on_removed id =
    Mutex.lock obs_lock;
    (match Hashtbl.find_opt seen id with
    | Some 1 -> Hashtbl.replace seen id 0
    | _ -> incr illegal (* removed while absent *));
    Mutex.unlock obs_lock
  in
  let h = C.observe_changes (C.find c ()) ~added:(fun id _ -> on_added id) ~removed:on_removed () in
  let domains =
    List.init 4 (fun w ->
        Domain.spawn (fun () ->
            for k = 0 to 199 do
              let id = Printf.sprintf "w%d-%d" w k in
              ignore (C.insert c (d [ ("_id", Bson.str id); ("v", i k) ]));
              ignore (C.update c (d [ ("_id", Bson.str id) ]) (d [ ("$inc", d [ ("v", i 1) ]) ]));
              if k mod 2 = 0 then ignore (C.remove_id c id)
            done))
  in
  List.iter Domain.join domains;
  (* one final write from this domain becomes the drainer for anything still queued *)
  let flush = C.insert c (d [ ("_id", Bson.str "flush") ]) in
  ignore (C.remove_id c flush);
  h.C.stop ();
  (* exact survivors: per worker the odd k of 0..199 → 100 each → 400 total *)
  let final = C.count (C.find c ()) in
  let live_in_view = Hashtbl.fold (fun _ n acc -> acc + n) seen 0 in
  final = 400 && !illegal = 0 && live_in_view = 400

let%test "multicore: concurrent subscribes during a write storm see a complete snapshot+stream view" =
  let c = C.create () in
  for k = 0 to 49 do
    ignore (C.insert c (d [ ("_id", Bson.str (string_of_int k)) ]))
  done;
  let writer =
    Domain.spawn (fun () ->
        for k = 50 to 299 do
          ignore (C.insert c (d [ ("_id", Bson.str (string_of_int k)) ]))
        done)
  in
  (* subscribe mid-storm: replay + buffered stream must cover every doc exactly (dups impossible at
     this layer: an insert re-delivered after the snapshot diffs to Stayed/no-op, never added twice) *)
  let seen = Hashtbl.create 512 in
  let dup = ref 0 in
  let lk = Mutex.create () in
  let h =
    C.observe_changes (C.find c ())
      ~added:(fun id _ ->
        Mutex.lock lk;
        if Hashtbl.mem seen id then incr dup else Hashtbl.replace seen id ();
        Mutex.unlock lk)
      ()
  in
  Domain.join writer;
  (* drain stragglers by becoming the drainer once more *)
  let flush = C.insert c (d [ ("_id", Bson.str "flush") ]) in
  ignore (C.remove_id c flush);
  h.C.stop ();
  !dup = 0 && Hashtbl.length seen >= 300 (* all 300 ids seen exactly once (flush may add 1) *)

let () = exit (Fennec_hunt_unit.run ())
