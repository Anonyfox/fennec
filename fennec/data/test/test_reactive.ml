(* The Meteor feature suite, run against the in-memory reactive instance. CRUD, query options,
   projection, upsert, idGeneration, document transform, the observe engine, multi-collection
   publish/subscribe with the merge box, methods, allow/deny, ObjectID, and EJSON — the surface a
   reactive app builds on. Publications use unique names per test (the registry is module-global). *)

open Fennec_data
module R = Reactive.Mini
module C = R.Collection
module B = Bson

let doc kvs = B.Document kvs
let i = B.int
let geti d k = match B.get d k with Some (B.Int n) -> Some n | _ -> None
let coll ?id_generation ?transform name = C.create ?id_generation ?transform ~name (Minimongo.create ())

(* ── CRUD ── *)
let%test "insert returns an _id; count; find_one" =
  let t = coll "tasks" in
  let id = C.insert t (doc [ ("n", i 1) ]) in
  String.length id > 0
  && C.count t () = 1
  && (match C.find_one t ~selector:(doc [ ("_id", B.String id) ]) () with
      | Some d -> geti d "n" = Some 1
      | None -> false)
let%test "update is applied" =
  let t = coll "u" in
  let id = C.insert t (doc [ ("n", i 1) ]) in
  let _ = C.update t (doc [ ("_id", B.String id) ]) (doc [ ("$set", doc [ ("n", i 2) ]) ]) in
  (match C.find_one t ~selector:(doc [ ("_id", B.String id) ]) () with
   | Some d -> geti d "n" = Some 2
   | None -> false)

(* ── query options ── *)
let%test "sort + skip + limit window" =
  let t = coll "s" in
  List.iter (fun n -> ignore (C.insert t (doc [ ("n", i n) ]))) [ 30; 10; 20 ];
  List.filter_map (fun d -> geti d "n") (C.fetch (C.find t ~sort:(doc [ ("n", i 1) ]) ~skip:1 ~limit:1 ()))
  = [ 20 ]
let%test "projection keeps a field, drops the rest" =
  let t = coll "p" in
  let id = C.insert t (doc [ ("n", i 1); ("secret", B.String "x") ]) in
  match C.fetch (C.find t ~selector:(doc [ ("_id", B.String id) ]) ~fields:(doc [ ("n", i 1) ]) ()) with
  | [ d ] -> geti d "n" = Some 1 && B.get d "secret" = None
  | _ -> false

(* ── upsert / idGeneration / transform ── *)
let%test "upsert reports number_affected and inserted_id" =
  let r = C.upsert (coll "up") (doc [ ("room", i 5) ]) (doc [ ("$set", doc [ ("hit", B.Bool true) ]) ]) in
  r.C.number_affected = 1 && r.C.inserted_id <> None
let%test "STRING idGeneration mints a 17-char _id" =
  String.length (C.insert (coll "sc") (doc [ ("a", i 1) ])) = 17
let%test "MONGO idGeneration mints a 24-hex _id" =
  String.length (C.insert (coll ~id_generation:R.MONGO "mc") (doc [ ("a", i 1) ])) = 24
let%test "collection transform applies; per-cursor transform:None disables it" =
  let tag = function B.Document kvs -> B.Document (("seen", B.Bool true) :: kvs) | x -> x in
  let t = coll ~transform:tag "tc" in
  let _ = C.insert t (doc [ ("a", i 1) ]) in
  (match C.find_one t () with Some d -> B.get d "seen" = Some (B.Bool true) | None -> false)
  && (match C.fetch (C.find t ~transform:None ()) with d :: _ -> B.get d "seen" = None | [] -> false)

(* ── observe ── *)
let%test "observe_changes: added, changed, removed-on-move-out" =
  let t = coll "oc" in
  let _ = C.insert t (doc [ ("_id", B.String "k"); ("room", i 1); ("v", i 1) ]) in
  let log = ref [] in
  let h =
    C.observe_changes
      (C.find t ~selector:(doc [ ("room", i 1) ]) ())
      ~added:(fun id _ -> log := ("add:" ^ id) :: !log)
      ~changed:(fun id _ _ -> log := ("chg:" ^ id) :: !log)
      ~removed:(fun id -> log := ("rem:" ^ id) :: !log) ()
  in
  let initial = !log = [ "add:k" ] in
  let _ = C.update t (doc [ ("_id", B.String "k") ]) (doc [ ("$set", doc [ ("v", i 2) ]) ]) in
  let changed = match !log with x :: _ -> x = "chg:k" | _ -> false in
  let _ = C.update t (doc [ ("_id", B.String "k") ]) (doc [ ("$set", doc [ ("room", i 9) ]) ]) in
  let removed = match !log with x :: _ -> x = "rem:k" | _ -> false in
  h.Reactive.stop ();
  initial && changed && removed

(* ── publish / subscribe ── *)
let%test "publish/subscribe merge box sees the live insert" =
  let feed = coll "feed" in
  let _ = C.insert feed (doc [ ("n", i 1) ]) in
  R.publish "feed_pub" (fun () -> R.Cursor (R.cursor feed ()));
  let sub = R.subscribe "feed_pub" in
  let one = sub.R.is_ready () && List.length (sub.R.documents ()) = 1 in
  let _ = C.insert feed (doc [ ("n", i 2) ]) in
  let two = List.length (sub.R.documents ()) = 2 in
  let scoped = List.length (sub.R.documents_of "feed") = 2 && sub.R.collections () = [ "feed" ] in
  sub.R.stop ();
  one && two && scoped
let%test "publish projection hides a field, including after a live update" =
  let pf = coll "pf" in
  let id = C.insert pf (doc [ ("n", i 1); ("hush", B.String "x") ]) in
  R.publish "pf_pub" (fun () -> R.Cursor (R.cursor pf ~fields:(doc [ ("n", i 1) ]) ()));
  let sub = R.subscribe "pf_pub" in
  let hidden () = List.for_all (fun (_, d) -> B.get d "hush" = None) (sub.R.documents ()) in
  let before = hidden () in
  let _ = C.update pf (doc [ ("_id", B.String id) ]) (doc [ ("$set", doc [ ("hush", B.String "y"); ("n", i 2) ]) ]) in
  let after = hidden () in
  sub.R.stop ();
  before && after

(* ── methods / allow-deny / ObjectID / EJSON ── *)
let%test "call runs a registered method" =
  R.methods [ ("sum", fun _ args -> match args with [ B.Int a; B.Int b ] -> B.Int (a + b) | _ -> B.Null) ];
  R.call "sum" [ B.Int 2; B.Int 3 ] = B.Int 5
let%test "allow/deny gate client inserts" =
  let sec = coll "sec" in
  let denied f = try ignore (f ()); false with R.Error _ -> true in
  let no_rules = denied (fun () -> C.insert_from_client sec (doc [ ("x", i 1) ])) in
  C.allow sec ~insert:(fun _ _ -> true) ();
  let allowed = String.length (C.insert_from_client sec (doc [ ("x", i 1) ])) > 0 in
  C.deny sec ~insert:(fun _ _ -> true) ();
  let deny_wins = denied (fun () -> C.insert_from_client sec (doc [ ("x", i 2) ])) in
  no_rules && allowed && deny_wins
let%test "ObjectID make / validate / reject" =
  let o = R.ObjectID.make () in
  String.length (R.ObjectID.to_hex_string o) = 24
  && R.ObjectID.make ~hex:o () = o
  && not (R.ObjectID.is_valid "nope")
let%test "EJSON clone + key-order-insensitive equals" =
  let v = doc [ ("a", i 1); ("b", B.Array [ i 1; i 2 ]) ] in
  R.EJSON.equals (R.EJSON.clone v) v
  && R.EJSON.equals (doc [ ("a", i 1); ("b", i 2) ]) (doc [ ("b", i 2); ("a", i 1) ])

let%test "publish preserves the Object_id _id type for a MONGO collection" =
  let c = coll ~id_generation:R.MONGO "mids" in
  let _ = C.insert c (doc [ ("n", i 1) ]) in
  R.publish "mids_pub" (fun () -> R.Cursor (R.cursor c ()));
  let sub = R.subscribe "mids_pub" in
  let ok =
    match sub.R.documents () with
    | [ (_, d) ] -> ( match B.get d "_id" with Some (B.Object_id _) -> true | _ -> false)
    | _ -> false
  in
  sub.R.stop ();
  ok

let () = exit (Fennec_hunt_unit.run ())
