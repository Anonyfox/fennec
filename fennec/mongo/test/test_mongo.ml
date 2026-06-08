(* Tests for the fennec-mongo pure trio — BSON values, the query engine, and Minimongo — using
   fennec's inline test tooling. [let%test] for examples and edge cases; [let%prop] for the laws
   (sort orders + permutes, $set-then-read, insert-then-find, diff/merge identities). The libraries
   themselves stay dependency-free; the tooling lives here. *)

module C = Minimongo.Collection
open Query

let d = Bson.doc
let i = Bson.int

(* ── Bson ── *)
let%test "get reads a top-level field" = Bson.get (d [ ("a", i 1) ]) "a" = Some (Bson.Int 1)
let%test "get of a missing field is None" = Bson.get (d [ ("a", i 1) ]) "b" = None
let%test "get of a non-document is None" = Bson.get (Bson.str "x") "a" = None

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
let%test "$gt operator" = Matcher.doc_matches (d [ ("n", d [ ("$gt", i 5) ]) ]) (d [ ("n", i 7) ])
let%test "$or operator" =
  Matcher.doc_matches
    (d [ ("$or", Bson.Array [ d [ ("a", i 1) ]; d [ ("b", i 2) ] ]) ])
    (d [ ("b", i 2) ])
let%test "get_path walks nested documents" =
  Matcher.get_path (d [ ("a", d [ ("b", i 5) ]) ]) "a.b" = Some (Bson.Int 5)
let%prop "a document matches its own equality selector" = fun (n : int) ->
  Matcher.doc_matches (d [ ("v", i n) ]) (d [ ("v", i n) ])
let%prop "$in matches a contained value" = fun (n : int) (m : int) ->
  Matcher.doc_matches (d [ ("v", d [ ("$in", Bson.Array [ i n; i m ]) ]) ]) (d [ ("v", i n) ])

(* ── Modifier ── *)
let%prop "$set then read returns the value" = fun (n : int) ->
  let r = Modifier.apply (d []) (d [ ("$set", d [ ("x", i n) ]) ]) in
  Bson.get r "x" = Some (Bson.Int n)
let%prop "$inc from absent yields the increment" = fun (n : int) ->
  let r = Modifier.apply (d []) (d [ ("$inc", d [ ("c", i n) ]) ]) in
  Bson.get r "c" = Some (Bson.Int n)
let%test "$unset removes a field" =
  Bson.get (Modifier.apply (d [ ("a", i 1) ]) (d [ ("$unset", d [ ("a", i 1) ]) ])) "a" = None
let%test "a non-operator modifier replaces, preserving _id" =
  let r = Modifier.apply (d [ ("_id", Bson.str "k"); ("a", i 1) ]) (d [ ("b", i 2) ]) in
  Bson.get r "_id" = Some (Bson.String "k")
  && Bson.get r "b" = Some (Bson.Int 2)
  && Bson.get r "a" = None

(* ── Projection ── *)
let%test "include keeps the listed fields and _id" =
  let p = Projection.of_fields (d [ ("a", i 1) ]) in
  Projection.apply p (d [ ("_id", Bson.str "x"); ("a", i 1); ("b", i 2) ])
  = d [ ("_id", Bson.str "x"); ("a", i 1) ]
let%test "exclude drops the listed fields" =
  let p = Projection.of_fields (d [ ("b", i 0) ]) in
  Projection.apply p (d [ ("a", i 1); ("b", i 2) ]) = d [ ("a", i 1) ]

(* ── Sorter ── *)
let vals docs =
  List.filter_map (fun x -> match Bson.get x "v" with Some (Bson.Int n) -> Some n | _ -> None) docs

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
(* build a WELL-FORMED document (unique top-level keys) — duplicate keys aren't valid BSON, and
   diff/merge are only meaningful over well-formed docs (a property run surfaced that a naive
   key-colliding generator produces duplicate keys with differing values). *)
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
let%test "update $set mutates the stored document" =
  let c = C.create () in
  let id = C.insert c (d [ ("v", i 1) ]) in
  let _ = C.update c (d [ ("_id", Bson.str id) ]) (d [ ("$set", d [ ("v", i 2) ]) ]) in
  (match C.find_one (C.find c ~selector:(d [ ("_id", Bson.str id) ]) ()) with
   | Some doc -> Bson.get doc "v" = Some (Bson.Int 2)
   | None -> false)
let%test "observe_changes fires added on insert" =
  let c = C.create () in
  let added = ref 0 in
  let h = C.observe_changes (C.find c ()) ~added:(fun _ _ -> incr added) () in
  let _ = C.insert c (d [ ("v", i 1) ]) in
  h.C.stop ();
  !added = 1

let () = exit (Fennec_hunt_unit.run ())
