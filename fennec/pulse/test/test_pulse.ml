(* The Meteor feature suite, run against the in-memory reactive instance. CRUD, query options,
   projection, upsert, idGeneration, document transform, the observe engine, multi-collection
   publish/subscribe with the merge box, methods (the one blessed client write path), ObjectID, and
   EJSON — the surface a reactive app builds on. Publications use unique names per test (the
   registry is module-global). *)

open Fennec_pulse
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
  && (match C.fetch (C.find t ~transform:C.Disable ()) with d :: _ -> B.get d "seen" = None | [] -> false)

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
  h.stop ();
  initial && changed && removed

(* ── publish / subscribe ── *)
let%test "publish/subscribe merge box sees the live insert" =
  let feed = coll "feed" in
  let _ = C.insert feed (doc [ ("n", i 1) ]) in
  R.publish "feed_pub" (fun _ -> R.Cursor (R.cursor feed ()));
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
  R.publish "pf_pub" (fun _ -> R.Cursor (R.cursor pf ~fields:(doc [ ("n", i 1) ]) ()));
  let sub = R.subscribe "pf_pub" in
  let hidden () = List.for_all (fun (_, d) -> B.get d "hush" = None) (sub.R.documents ()) in
  let before = hidden () in
  let _ = C.update pf (doc [ ("_id", B.String id) ]) (doc [ ("$set", doc [ ("hush", B.String "y"); ("n", i 2) ]) ]) in
  let after = hidden () in
  sub.R.stop ();
  before && after

(* ── methods (THE client write path — no allow/deny in fennec, by decree) / ObjectID / EJSON ── *)
let%test "call runs a registered method" =
  R.methods [ ("sum", fun _ args -> match args with [ B.Int a; B.Int b ] -> B.Int (a + b) | _ -> B.Null) ];
  R.call "sum" [ B.Int 2; B.Int 3 ] = B.Int 5

let%test "methods: the invocation carries user_id and can rebind it via set_user_id" =
  R.methods
    [ ("whoami", fun inv _ -> (match inv.R.user_id with Some u -> B.str u | None -> B.Null));
      ("login", fun inv _ -> inv.R.set_user_id (Some "alice"); B.Bool true) ];
  let rebound = ref None in
  let anon = R.call "whoami" [] = B.Null in
  let as_user = R.apply ~user_id:(Some "u1") "whoami" [] = B.str "u1" in
  let _ = R.apply ~set_user_id:(fun u -> rebound := u) "login" [] in
  anon && as_user && !rebound = Some "alice"
(* ── the typed method layer: one shared value; the codec IS the validation ── *)
module MT = Fennec_pulse_method

let%test "typed methods: handle decodes args + encodes result; a malformed call is a 400 BEFORE the handler" =
  let m = MT.Method.define "typed_sum" ~args:(MT.Codec.a2 MT.Codec.int MT.Codec.int) ~result:MT.Codec.int in
  let ran = ref 0 in
  R.handle m (fun _ (a, b) ->
      incr ran;
      a + b);
  let ok = R.call "typed_sum" [ B.int 2; B.int 40 ] = B.Int 42 in
  let bad = try ignore (R.call "typed_sum" [ B.str "x" ]); None with R.Error { code; _ } -> Some code in
  ok && bad = Some "400" && !ran = 1

let%test "codec: roundtrips, EJSON float-ints, decode errors carry the shape" =
  let open MT.Codec in
  (match (list int).dec ((list int).enc [ 1; 2; 3 ]) with Ok [ 1; 2; 3 ] -> true | _ -> false)
  && (match (option string).dec Bson.Null with Ok None -> true | _ -> false)
  && (match int.dec (Bson.Float 7.0) with Ok 7 -> true | _ -> false)
  && (match string.dec (Bson.Int 3) with Error e -> e = "expected string, got int" | Ok _ -> false)
  && (match (a2 int bool).dec_args [ B.int 1 ] with Error _ -> true | Ok _ -> false)

let%test "seed streams: same (seed, scope) mints the SAME ids both sides; another scope diverges" =
  let s1 = MT.Method.Seed.stream ~seed:"abc" ~scope:"tasks" in
  let s2 = MT.Method.Seed.stream ~seed:"abc" ~scope:"tasks" in
  let s3 = MT.Method.Seed.stream ~seed:"abc" ~scope:"other" in
  let id_of s = Query.Id.random_id ~rng:s () in
  let a = id_of s1 and b = id_of s2 and c = id_of s3 in
  a = b && a <> c && id_of s1 = id_of s2 (* the streams stay in lockstep, not just their head *)

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
  R.publish "mids_pub" (fun _ -> R.Cursor (R.cursor c ()));
  let sub = R.subscribe "mids_pub" in
  let ok =
    match sub.R.documents () with
    | [ (_, d) ] -> ( match B.get d "_id" with Some (B.Object_id _) -> true | _ -> false)
    | _ -> false
  in
  sub.R.stop ();
  ok

(* ── multi-collection aggregation: $lookup resolves a foreign collection from the instance's
   named-collection registry, so in-memory joins span collections like a real database ── *)
let%test "$lookup joins a foreign collection from the registry (in-memory multi-collection)" =
  let orders = coll "orders_lk" and customers = coll "customers_lk" in
  let _ = C.insert customers (doc [ ("_id", B.String "c7"); ("name", B.String "Ada") ]) in
  let _ = C.insert orders (doc [ ("_id", B.String "o1"); ("cust", B.String "c7") ]) in
  match
    C.aggregate orders
      [ doc [ ("$lookup", doc [ ("from", B.String "customers_lk"); ("localField", B.String "cust");
                                ("foreignField", B.String "_id"); ("as", B.String "c") ]) ] ]
  with
  | [ row ] -> (match B.get row "c" with Some (B.Array [ cust ]) -> B.get cust "name" = Some (B.String "Ada") | _ -> false)
  | _ -> false

let%test "$lookup against an unregistered collection yields an empty join (no crash)" =
  let orders = coll "orders_lk2" in
  let _ = C.insert orders (doc [ ("_id", B.String "o1"); ("cust", B.String "x") ]) in
  match
    C.aggregate orders
      [ doc [ ("$lookup", doc [ ("from", B.String "nope_missing"); ("localField", B.String "cust");
                                ("foreignField", B.String "_id"); ("as", B.String "c") ]) ] ]
  with
  | [ row ] -> B.get row "c" = Some (B.Array [])
  | _ -> false

(* ── parameterized publications: the publication closure receives the subscription's params ── *)
let%test "a publication receives its subscription params (parameterized cursor)" =
  let posts = coll "posts_param" in
  let _ = C.insert posts (doc [ ("room", i 1); ("t", B.String "a") ]) in
  let _ = C.insert posts (doc [ ("room", i 2); ("t", B.String "b") ]) in
  R.publish "byroom" (fun params ->
      match params with
      | [ Bson.Document [ ("room", r) ] ] -> R.Cursor (R.cursor posts ~selector:(doc [ ("room", r) ]) ())
      | _ -> R.Cursor (R.cursor posts ()));
  (* run with room=1: only the room-1 doc is replayed as an Added beat *)
  let seen = ref [] in
  let h =
    R.run_publication "byroom" ~params:[ Bson.Document [ ("room", i 1) ] ]
      ~on:(function Reactive.Added { fields; _ } -> seen := fields :: !seen | _ -> ())
  in
  h.stop ();
  match !seen with [ fields ] -> List.assoc_opt "room" fields = Some (B.Int 1) | _ -> false

let%test "Collection.forget removes a collection from the $lookup registry" =
  let orders = coll "orders_fg" and customers = coll "customers_fg" in
  let _ = C.insert customers (doc [ ("_id", B.String "c1"); ("name", B.String "Zed") ]) in
  let _ = C.insert orders (doc [ ("_id", B.String "o1"); ("cust", B.String "c1") ]) in
  let lk () =
    C.aggregate orders
      [ doc [ ("$lookup", doc [ ("from", B.String "customers_fg"); ("localField", B.String "cust");
                                ("foreignField", B.String "_id"); ("as", B.String "c") ]) ] ]
  in
  let joined_before = match lk () with [ row ] -> (match B.get row "c" with Some (B.Array [ _ ]) -> true | _ -> false) | _ -> false in
  C.forget "customers_fg";
  let empty_after = match lk () with [ row ] -> B.get row "c" = Some (B.Array []) | _ -> false in
  joined_before && empty_after

(* ── RX9: the observe-multiplexer query key — same (collection, query) ⇒ same key ⇒ one shared
   observe; key order widens sharing, distinct queries stay distinct ── *)
let%test "query_key: a selector's key ORDER doesn't change the key (wider observe sharing)" =
  let q sel = Query_key.of_query ~collection:"t" (Backend.query ~selector:sel ()) in
  q (doc [ ("a", i 1); ("b", i 2) ]) = q (doc [ ("b", i 2); ("a", i 1) ])

let%test "query_key: a different selector / collection / limit yields a different key" =
  let q ?(coll = "t") ?(limit = 0) sel = Query_key.of_query ~collection:coll (Backend.query ~selector:sel ~limit ()) in
  q (doc [ ("a", i 1) ]) <> q (doc [ ("a", i 2) ])
  && q (doc []) <> q ~coll:"u" (doc [])
  && q (doc []) <> q ~limit:5 (doc [])

(* ── RX9: the observe multiplexer — one shared backend observe per (collection, query) ── *)
let%test "mux: same-query subscriptions share ONE observe; a distinct query gets its own; teardown frees" =
  let c = coll "mux_share" in
  let _ = C.insert c (doc [ ("v", i 1) ]) in
  R.publish "mux_p" (fun _ -> R.Cursor (C.find c ()));
  R.publish "mux_q" (fun _ -> R.Cursor (C.find c ~selector:(doc [ ("v", i 1) ]) ()));
  let before = R.live_query_count () in
  let s1 = R.run_publication "mux_p" ~params:[] ~on:(fun _ -> ()) in
  let s2 = R.run_publication "mux_p" ~params:[] ~on:(fun _ -> ()) in
  (* same (collection, query) as s1 → shared *)
  let s3 = R.run_publication "mux_q" ~params:[] ~on:(fun _ -> ()) in
  (* different query → its own observe *)
  let shared = R.live_query_count () = before + 2 in
  s1.stop ();
  s2.stop ();
  s3.stop ();
  shared && R.live_query_count () = before

let%test "mux: a single mutation fans to ALL sharers, and a late joiner gets the current state" =
  let c = coll "mux_fan" in
  R.publish "mux_fan_p" (fun _ -> R.Cursor (C.find c ()));
  let got1 = ref 0 and got2 = ref 0 in
  let s1 = R.run_publication "mux_fan_p" ~params:[] ~on:(fun _ -> incr got1) in
  let s2 = R.run_publication "mux_fan_p" ~params:[] ~on:(fun _ -> incr got2) in
  let _ = C.insert c (doc [ ("v", i 9) ]) in
  (* one mutation → fanned to BOTH subscribers *)
  let fanned = !got1 >= 1 && !got2 >= 1 in
  let late = ref 0 in
  let s3 = R.run_publication "mux_fan_p" ~params:[] ~on:(fun _ -> incr late) in
  (* late joiner gets the existing doc replayed (≥1 beat) without re-observing the backend *)
  let late_got = !late >= 1 in
  s1.stop ();
  s2.stop ();
  s3.stop ();
  fanned && late_got

let%test "mux: a stale double-stop is idempotent — it cannot evict a fresh same-key mux" =
  let c = coll "mux_evict" in
  R.publish "mux_evict_p" (fun _ -> R.Cursor (C.find c ()));
  let before = R.live_query_count () in
  let s1 = R.run_publication "mux_evict_p" ~params:[] ~on:(fun _ -> ()) in
  s1.stop ();
  (* mux torn down; a fresh subscription rebuilds it under the SAME key *)
  let s2 = R.run_publication "mux_evict_p" ~params:[] ~on:(fun _ -> ()) in
  s1.stop ();
  (* this STALE second stop of s1 must be a no-op — not evict s2's fresh mux *)
  let ok = R.live_query_count () = before + 1 in
  s2.stop ();
  ok && R.live_query_count () = before

let%test "collections: two anonymous collections get distinct synthetic names (no \"\" wire collision)" =
  let a = C.create (Minimongo.create ()) and b = C.create (Minimongo.create ()) in
  let na = C.name a and nb = C.name b in
  na <> nb && na <> "" && nb <> ""

let%test "mux (multicore): concurrent same-key subscribers across domains all see every doc; teardown drains" =
  let c = coll "mux_mc" in
  for k = 0 to 19 do
    ignore (C.insert c (doc [ ("_id", B.str (string_of_int k)) ]))
  done;
  R.publish "mux_mc_p" (fun _ -> R.Cursor (C.find c ()));
  let before = R.live_query_count () in
  let writing = Atomic.make true in
  let writer =
    Domain.spawn (fun () ->
        for k = 20 to 119 do
          ignore (C.insert c (doc [ ("_id", B.str (string_of_int k)) ]))
        done;
        Atomic.set writing false)
  in
  (* 3 domains subscribe to the SAME query mid-write-storm: snapshot + buffered stream must cover
     every doc for each. Delivery is asynchronous under contention (a write may return before its
     event is delivered — the active drainer carries it), so each subscriber WAITS for its view to
     converge rather than assuming synchronous delivery; the bound turns a genuine loss into a fail. *)
  let counts =
    List.init 3 (fun _ ->
        Domain.spawn (fun () ->
            let seen = Hashtbl.create 256 in
            let h =
              R.run_publication "mux_mc_p" ~params:[]
                ~on:(function
                  | Reactive.Added { id; _ } -> Hashtbl.replace seen id ()
                  | _ -> ())
            in
            while Atomic.get writing do
              Domain.cpu_relax ()
            done;
            let spins = ref 0 in
            while Hashtbl.length seen < 120 && !spins < 50_000_000 do
              incr spins;
              Domain.cpu_relax ()
            done;
            h.stop ();
            Hashtbl.length seen))
  in
  let seen_counts = List.map Domain.join counts in
  Domain.join writer;
  (* every subscriber converged on all 120 docs; the mux table drained back to baseline *)
  List.for_all (fun n -> n = 120) seen_counts && R.live_query_count () = before

let%test "ordered observe: moved_to carries from/to indexes and changed_at the current index" =
  let c = coll "ord_obs" in
  let _ = C.insert c (doc [ ("_id", B.str "x"); ("r", i 1) ]) in
  let _ = C.insert c (doc [ ("_id", B.str "y"); ("r", i 2) ]) in
  let _ = C.insert c (doc [ ("_id", B.str "z"); ("r", i 3) ]) in
  let moved = ref None and chg_at = ref (-1) in
  let h =
    C.observe (C.find c ~sort:(doc [ ("r", i 1) ]) ())
      ~moved_to:(fun _ from to_ _ -> moved := Some (from, to_))
      ~changed_at:(fun _ _ idx -> chg_at := idx)
      ()
  in
  (* z's sort key drops to the front: order x,y,z → z,x,y — z moves index 2 → 0 *)
  ignore (C.update c (doc [ ("_id", B.str "z") ]) (doc [ ("$set", doc [ ("r", i 0) ]) ]));
  h.stop ();
  !moved = Some (2, 0) && !chg_at = 0

let%test "publication: a sorted+limited cursor maintains its top-N over the wire (window, not a leak)" =
  let c = coll "win_pub" in
  let add id s = ignore (C.insert c (doc [ ("_id", B.str id); ("score", i s) ])) in
  add "a" 30; add "b" 20; add "k" 10;
  R.publish "win_p" (fun _ -> R.Cursor (C.find c ~sort:(doc [ ("score", i (-1)) ]) ~limit:2 ()));
  let live = Hashtbl.create 8 in
  let s =
    R.run_publication "win_p" ~params:[]
      ~on:(function
        | Reactive.Added { id; _ } -> Hashtbl.replace live id ()
        | Reactive.Removed { id; _ } -> Hashtbl.remove live id
        | _ -> ())
  in
  (* initial top-2 *)
  let init_ok = Hashtbl.mem live "a" && Hashtbl.mem live "b" && Hashtbl.length live = 2 in
  (* a better-ranked doc displaces the boundary doc on the live wire *)
  add "d" 40;
  let windowed = Hashtbl.mem live "d" && Hashtbl.mem live "a" && (not (Hashtbl.mem live "b")) && Hashtbl.length live = 2 in
  s.stop ();
  init_ok && windowed

let%test "mux: a publication feeding Cursors [] is a no-op stream (ready, zero beats, no observe)" =
  R.publish "mux_empty_p" (fun _ -> R.Cursors []);
  let before = R.live_query_count () in
  let n = ref 0 in
  let s = R.run_publication "mux_empty_p" ~params:[] ~on:(fun _ -> incr n) in
  let ok = !n = 0 && R.live_query_count () = before in
  s.stop ();
  ok && R.live_query_count () = before

let () = exit (Fennec_hunt_unit.run ())
