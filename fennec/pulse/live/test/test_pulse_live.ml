(* The client live-data layer. The merge store's §5b semantics — precedence (earliest subscription
   wins a field), progressive enrichment (fields merge across subscriptions), refcounted removal,
   changed/sub_stopped — proven purely; and the Fur `find` binding proven to recompute reactively
   as the store changes (Fur signals run native, so no browser is needed). *)

module MS = Fennec_pulse_live.Merge_store
module Live = Fennec_pulse_live.Live
module B = Bson

let one = function [| doc |] -> Some doc | _ -> None

(* ── merge store ── *)
let%test "added then fetch finds the merged doc" =
  let s = MS.create () in
  MS.added s ~sub:"a" ~collection:"items" ~id:"1" ~fields:[ ("n", B.int 1) ];
  match one (MS.fetch s "items" ()) with
  | Some d -> B.get d "_id" = Some (B.String "1") && B.get d "n" = Some (B.Int 1)
  | None -> false

let%test "earliest subscription wins a field conflict" =
  let s = MS.create () in
  MS.added s ~sub:"a" ~collection:"c" ~id:"1" ~fields:[ ("x", B.int 10) ];
  MS.added s ~sub:"b" ~collection:"c" ~id:"1" ~fields:[ ("x", B.int 20) ];
  match one (MS.fetch s "c" ()) with Some d -> B.get d "x" = Some (B.Int 10) | None -> false

let%test "progressive enrichment merges fields across subscriptions" =
  let s = MS.create () in
  MS.added s ~sub:"a" ~collection:"c" ~id:"1" ~fields:[ ("title", B.str "T") ];
  MS.added s ~sub:"b" ~collection:"c" ~id:"1" ~fields:[ ("body", B.str "B") ];
  match one (MS.fetch s "c" ()) with
  | Some d -> B.get d "title" = Some (B.String "T") && B.get d "body" = Some (B.String "B")
  | None -> false

let%test "doc survives while any subscription covers it, dropped when none do" =
  let s = MS.create () in
  MS.added s ~sub:"a" ~collection:"c" ~id:"1" ~fields:[ ("n", B.int 1) ];
  MS.added s ~sub:"b" ~collection:"c" ~id:"1" ~fields:[ ("n", B.int 1) ];
  MS.removed s ~sub:"a" ~collection:"c" ~id:"1";
  let still = Array.length (MS.fetch s "c" ()) = 1 in
  MS.removed s ~sub:"b" ~collection:"c" ~id:"1";
  still && Array.length (MS.fetch s "c" ()) = 0

let%test "changed updates a field; sub_stopped drops the subscription's docs" =
  let s = MS.create () in
  MS.added s ~sub:"a" ~collection:"c" ~id:"1" ~fields:[ ("n", B.int 1) ];
  MS.changed s ~sub:"a" ~collection:"c" ~id:"1" ~fields:[ ("n", B.int 9) ] ~cleared:[];
  let updated = match one (MS.fetch s "c" ()) with Some d -> B.get d "n" = Some (B.Int 9) | None -> false in
  MS.sub_stopped s "a";
  updated && Array.length (MS.fetch s "c" ()) = 0

(* ── Fur binding ── *)
let%test "Live.find signal recomputes reactively as the store changes" =
  let lv = Live.create () in
  let result = Live.find lv "items" () in
  let empty = Array.length (Fur.peek result) = 0 in
  MS.added (Live.store lv) ~sub:"a" ~collection:"items" ~id:"1" ~fields:[ ("n", B.int 1) ];
  let one_doc = match one (Fur.peek result) with Some d -> B.get d "n" = Some (B.Int 1) | None -> false in
  MS.added (Live.store lv) ~sub:"a" ~collection:"items" ~id:"2" ~fields:[ ("n", B.int 2) ];
  empty && one_doc && Array.length (Fur.peek result) = 2

(* ── wire→cache routing (DDP delta → merge store), including the ordered deltas ── *)
module WR = Fennec_pulse_live.Wire_route
module Msg = Fennec_ddp.Message

let%test "wire: addedBefore surfaces the doc (ordered delta is NOT dropped)" =
  let s = MS.create () in
  let handled =
    WR.apply_delta s (Msg.Added_before { collection = "c"; id = "1"; fields = [ ("n", B.int 1) ]; before = None })
  in
  handled && (match one (MS.fetch s "c" ()) with Some d -> B.get d "n" = Some (B.Int 1) | None -> false)

let%test "wire: movedBefore is handled as a no-op (pure reorder — nothing lost or spuriously added)" =
  let s = MS.create () in
  WR.apply_delta s (Msg.Moved_before { collection = "c"; id = "1"; before = None })
  && Array.length (MS.fetch s "c" ()) = 0

let%test "wire: added/changed route through; a control frame (ready) does not" =
  let s = MS.create () in
  let a = WR.apply_delta s (Msg.Added { collection = "c"; id = "1"; fields = [ ("n", B.int 1) ]; sub = Some "x" }) in
  let c = WR.apply_delta s (Msg.Changed { collection = "c"; id = "1"; fields = [ ("n", B.int 2) ]; cleared = []; sub = Some "x" }) in
  let upd = match one (MS.fetch s "c" ()) with Some d -> B.get d "n" = Some (B.Int 2) | None -> false in
  a && c && upd && not (WR.apply_delta s (Msg.Ready { subs = [ "x" ] }))

(* ── SSR seed payload: carries the COLLECTION so hydration is robust when a publication's name
   differs from its collection (the browser can't re-derive it — publish is a no-op there) ── *)
module Seed = Fennec_pulse_live.Seed

let%test "seed: round-trips the documents AND their collection (single group)" =
  let docs = [ B.doc [ ("_id", B.str "1"); ("n", B.int 1) ]; B.doc [ ("_id", B.str "2") ] ] in
  match Seed.decode (Seed.encode [ ("messages", docs) ]) with
  | [ ("messages", got) ] -> List.length got = 2 && B.get (List.hd got) "n" = Some (B.Int 1)
  | _ -> false

let%test "seed: carries MULTIPLE collections' groups (a multi-collection publication)" =
  (* one publication feeding rooms + messages seeds BOTH, each under its real collection *)
  let groups =
    [ ("rooms", [ B.doc [ ("_id", B.str "r1") ] ]);
      ("messages", [ B.doc [ ("_id", B.str "m1") ]; B.doc [ ("_id", B.str "m2") ] ]) ]
  in
  match Seed.decode (Seed.encode groups) with [ ("rooms", [ _ ]); ("messages", [ _; _ ]) ] -> true | _ -> false

let%test "seed: a malformed / legacy payload decodes to [] (no crash)" =
  Seed.decode "not json" = [] && Seed.decode "{}" = []

(* ── coalescing: a burst of mutations notifies each reactive view ONCE, not per-doc (the O(W^2)
   subscription-replay guard) ── *)
let%test "merge store: a seed burst fires a view's listeners ONCE (coalesced, not W times)" =
  let box = MS.create () in
  let fires = ref 0 in
  let _ = MS.on_change box "tasks" (fun () -> incr fires) in
  MS.seed box ~sub:"s1" ~collection:"tasks"
    [ B.doc [ ("_id", B.str "a") ]; B.doc [ ("_id", B.str "b") ]; B.doc [ ("_id", B.str "c") ] ];
  !fires = 1

(* ── client-side aggregation across the cache's many collections ── *)
let%test "client aggregate: $lookup joins across the client's collections" =
  let s = MS.create () in
  MS.added s ~sub:"x" ~collection:"customers" ~id:"c7" ~fields:[ ("name", B.str "Ada") ];
  MS.added s ~sub:"x" ~collection:"orders" ~id:"o1" ~fields:[ ("cust", B.str "c7") ];
  match
    Array.to_list
      (MS.aggregate s "orders"
         [ B.doc [ ("$lookup", B.doc [ ("from", B.str "customers"); ("localField", B.str "cust");
                                       ("foreignField", B.str "_id"); ("as", B.str "c") ]) ] ])
  with
  | [ row ] -> (match B.get row "c" with Some (B.Array [ cust ]) -> B.get cust "name" = Some (B.String "Ada") | _ -> false)
  | _ -> false

let%test "Live.aggregate recomputes reactively as the primary collection changes" =
  let lv = Live.create () in
  let r = Live.aggregate lv "nums" [ B.doc [ ("$match", B.doc []) ] ] in
  let before = Array.length (Fur.peek r) in
  MS.added (Live.store lv) ~sub:"a" ~collection:"nums" ~id:"1" ~fields:[ ("v", B.int 1) ];
  before = 0 && Array.length (Fur.peek r) = 1

(* ── seed↔live quiescence: SSR-seeded docs the live snapshot doesn't re-confirm are dropped ── *)
let%test "quiesce: a seeded doc the live snapshot doesn't re-confirm is dropped on ready" =
  let s = MS.create () in
  MS.seed s ~sub:"s1" ~collection:"c"
    [ B.doc [ ("_id", B.str "D"); ("n", B.int 1) ]; B.doc [ ("_id", B.str "E"); ("n", B.int 2) ] ];
  let both = Array.length (MS.fetch s "c" ()) = 2 in
  (* the live snapshot re-confirms only D; E was deleted server-side between SSR and the socket *)
  MS.added s ~sub:"s1" ~collection:"c" ~id:"D" ~fields:[ ("n", B.int 1) ];
  MS.quiesce s "s1";
  let only_d =
    match Array.to_list (MS.fetch s "c" ()) with [ d ] -> B.get d "_id" = Some (B.String "D") | _ -> false
  in
  both && only_d

let%test "quiesce: a live `changed` also confirms a seeded doc (kept, not dropped)" =
  let s = MS.create () in
  MS.seed s ~sub:"s1" ~collection:"c" [ B.doc [ ("_id", B.str "D"); ("n", B.int 1) ] ];
  MS.changed s ~sub:"s1" ~collection:"c" ~id:"D" ~fields:[ ("n", B.int 9) ] ~cleared:[];
  MS.quiesce s "s1";
  (match Array.to_list (MS.fetch s "c" ()) with [ d ] -> B.get d "n" = Some (B.Int 9) | _ -> false)

(* ── reconnect resync: resync_begin + quiesce heals the cache (drops docs the server no longer sends) ── *)
let%test "resync_begin + quiesce drops a doc the post-reconnect snapshot no longer includes" =
  let s = MS.create () in
  MS.added s ~sub:"s1" ~collection:"c" ~id:"D" ~fields:[ ("n", B.int 1) ];
  MS.added s ~sub:"s1" ~collection:"c" ~id:"E" ~fields:[ ("n", B.int 2) ];
  (* reconnect: re-mark all of s1's docs tentative; the resubscription re-adds only D (E was deleted
     during the outage), then quiesce on the new ready *)
  MS.resync_begin s "s1";
  MS.added s ~sub:"s1" ~collection:"c" ~id:"D" ~fields:[ ("n", B.int 1) ];
  MS.quiesce s "s1";
  match Array.to_list (MS.fetch s "c" ()) with
  | [ d ] -> B.get d "_id" = Some (B.String "D")
  | _ -> false

(* ── MONGO-idGeneration collections: the typed Object_id _id survives on the client ── *)
let%test "MONGO collection: a seeded Object_id _id survives the merge + a live change (not coerced)" =
  let s = MS.create () in
  let oid = String.make 24 'a' in
  MS.seed s ~sub:"s1" ~collection:"things" [ B.doc [ ("_id", B.Object_id oid); ("n", B.int 1) ] ];
  let is_oid () =
    match one (MS.fetch s "things" ()) with
    | Some d -> ( match B.get d "_id" with Some (B.Object_id x) -> x = oid | _ -> false)
    | None -> false
  in
  let seeded = is_oid () in
  (* a live change carries the id as a hex STRING on the wire; the collection stays MONGO so the _id
     is reconstructed as Object_id (so find/$lookup by _id keep matching the server) *)
  MS.changed s ~sub:"s1" ~collection:"things" ~id:oid ~fields:[ ("n", B.int 2) ] ~cleared:[];
  seeded && is_oid ()
  && (match one (MS.fetch s "things" ()) with Some d -> B.get d "n" = Some (B.Int 2) | None -> false)

let%test "MONGO collection: a String-_id doc seeded BEFORE an Object_id doc is still typed Object_id (mixed batch)" =
  let s = MS.create () in
  let a = String.make 24 'a' and b = String.make 24 'b' in
  (* the String-_id doc comes FIRST; an Object_id doc follows in the SAME group → whole collection MONGO *)
  MS.seed s ~sub:"s1" ~collection:"things" [ B.doc [ ("_id", B.str a) ]; B.doc [ ("_id", B.Object_id b) ] ];
  (* every doc must read back with an Object_id _id, and a find BY Object_id must match the first one *)
  let all_typed =
    Array.for_all (fun d -> match B.get d "_id" with Some (B.Object_id _) -> true | _ -> false) (MS.fetch s "things" ())
  in
  let found = MS.fetch s "things" ~selector:(B.doc [ ("_id", B.Object_id a) ]) () in
  all_typed && Array.length found = 1

(* ── edge cases proven covered ── *)
let%test "merge store: removed of an unknown id (and a double-remove) is a safe no-op" =
  let s = MS.create () in
  MS.removed s ~sub:"a" ~collection:"c" ~id:"ghost";
  (* never added *)
  MS.added s ~sub:"a" ~collection:"c" ~id:"x" ~fields:[ ("v", B.int 1) ];
  MS.removed s ~sub:"a" ~collection:"c" ~id:"x";
  MS.removed s ~sub:"a" ~collection:"c" ~id:"x";
  (* double remove *)
  Array.length (MS.fetch s "c" ()) = 0

let%test "seed: a duplicate _id within one group resolves to a single row (union of fields)" =
  let s = MS.create () in
  MS.seed s ~sub:"a" ~collection:"c"
    [ B.doc [ ("_id", B.str "d"); ("x", B.int 1) ]; B.doc [ ("_id", B.str "d"); ("y", B.int 2) ] ];
  match MS.fetch s "c" () with
  | [| row |] -> B.get row "x" = Some (B.Int 1) && B.get row "y" = Some (B.Int 2)
  | _ -> false

(* ── optimistic simulation: a sim sub wins, rolls back via sub_stopped, and can hide (delete) ── *)
let%test "sim: a simulation's field wins over the real sub; dropping it reveals server truth" =
  let s = MS.create () in
  MS.added s ~sub:"real" ~collection:"c" ~id:"x" ~fields:[ ("v", B.int 1) ];
  MS.begin_sim s "sim:m1";
  MS.changed s ~sub:"sim:m1" ~collection:"c" ~id:"x" ~fields:[ ("v", B.int 99) ] ~cleared:[];
  let optimistic = match MS.fetch s "c" () with [| d |] -> B.get d "v" = Some (B.Int 99) | _ -> false in
  MS.sub_stopped s "sim:m1";
  let revealed = match MS.fetch s "c" () with [| d |] -> B.get d "v" = Some (B.Int 1) | _ -> false in
  optimistic && revealed

let%test "sim: a LATER simulation wins over an earlier one on the same field" =
  let s = MS.create () in
  MS.begin_sim s "sim:a";
  MS.begin_sim s "sim:b";
  MS.added s ~sub:"sim:a" ~collection:"c" ~id:"x" ~fields:[ ("v", B.int 1) ];
  MS.changed s ~sub:"sim:b" ~collection:"c" ~id:"x" ~fields:[ ("v", B.int 2) ] ~cleared:[];
  match MS.fetch s "c" () with [| d |] -> B.get d "v" = Some (B.Int 2) | _ -> false

let%test "sim_hide: an optimistic delete hides the doc; dropping the sim restores it — unless really gone" =
  let s = MS.create () in
  MS.added s ~sub:"real" ~collection:"c" ~id:"x" ~fields:[ ("v", B.int 1) ];
  MS.added s ~sub:"real" ~collection:"c" ~id:"y" ~fields:[ ("v", B.int 2) ];
  MS.begin_sim s "sim:m";
  MS.sim_hide s ~sub:"sim:m" ~collection:"c" ~id:"x";
  MS.sim_hide s ~sub:"sim:m" ~collection:"c" ~id:"y";
  let hidden = Array.length (MS.fetch s "c" ()) = 0 in
  (* the server really removed y meanwhile; x it kept *)
  MS.removed s ~sub:"real" ~collection:"c" ~id:"y";
  MS.sub_stopped s "sim:m";
  let after = MS.fetch s "c" () in
  hidden && Array.length after = 1
  && (match B.get after.(0) "_id" with Some (B.String "x") -> true | _ -> false)

let%test "Sim.writes: insert mints the seeded id, update runs the real modifier, remove hides" =
  let s = MS.create () in
  MS.added s ~sub:"real" ~collection:"tasks" ~id:"t1" ~fields:[ ("n", B.int 1) ];
  let w = Fennec_pulse_live.Sim.writes s ~sim:"sim:m" ~seed:"seed1" in
  let module Mth = Fennec_pulse_method in
  let id = w.Mth.Method.insert "tasks" (B.doc [ ("title", B.str "hello") ]) in
  (* the minted id is exactly the (seed, collection)-stream's prediction *)
  let predicted = Query.Id.random_id ~rng:(Mth.Method.Seed.stream ~seed:"seed1" ~scope:"tasks") () in
  let upd = w.Mth.Method.update "tasks" (B.doc [ ("_id", B.str "t1") ]) (B.doc [ ("$inc", B.doc [ ("n", B.int 41) ]) ]) in
  let n_now =
    match MS.fetch s "tasks" ~selector:(B.doc [ ("_id", B.str "t1") ]) () with
    | [| d |] -> B.get d "n" = Some (B.Int 42)
    | _ -> false
  in
  let rm = w.Mth.Method.remove "tasks" (B.doc [ ("_id", B.str "t1") ]) in
  let gone = Array.length (MS.fetch s "tasks" ~selector:(B.doc [ ("_id", B.str "t1") ]) ()) = 0 in
  id = predicted && upd = 1 && n_now && rm = 1 && gone
  && Array.length (MS.fetch s "tasks" ()) = 1 (* the optimistic insert remains *)

let%test "changed: a changed for a doc this sub never added creates it (Meteor-tolerant, not dropped)" =
  let s = MS.create () in
  MS.changed s ~sub:"a" ~collection:"c" ~id:"x" ~fields:[ ("v", B.int 1) ] ~cleared:[];
  match MS.fetch s "c" () with [| d |] -> B.get d "v" = Some (B.Int 1) | _ -> false

let%test "multicore: an SSR-shared store survives concurrent domain seeds + deltas + reads, exactly" =
  let s = MS.create () in
  let listener_fires = Atomic.make 0 in
  let _ = MS.on_change s "c" (fun () -> Atomic.incr listener_fires) in
  let domains =
    List.init 4 (fun w ->
        Domain.spawn (fun () ->
            let sub = "s" ^ string_of_int w in
            (* a seed burst + live deltas + interleaved reads, per domain on ITS OWN sub *)
            MS.seed s ~sub ~collection:"c"
              (List.init 25 (fun k -> B.doc [ ("_id", B.str (Printf.sprintf "%d-%d" w k)) ]));
            for k = 0 to 24 do
              let id = Printf.sprintf "%d-%d" w k in
              MS.changed s ~sub ~collection:"c" ~id ~fields:[ ("v", B.int k) ] ~cleared:[];
              ignore (MS.fetch s "c" ())
            done;
            (* drop the odd ones *)
            for k = 0 to 24 do
              if k mod 2 = 1 then MS.removed s ~sub ~collection:"c" ~id:(Printf.sprintf "%d-%d" w k)
            done))
  in
  List.iter Domain.join domains;
  (* 4 domains × 13 surviving (even k of 0..24) = 52; every fire happened outside the lock *)
  Array.length (MS.fetch s "c" ()) = 52 && Atomic.get listener_fires > 0

let%test "Live.aggregate recomputes when a FOREIGN $lookup collection changes (not just the primary)" =
  let lv = Live.create () in
  MS.added (Live.store lv) ~sub:"a" ~collection:"orders" ~id:"o1" ~fields:[ ("cust", B.str "c1") ];
  let r =
    Live.aggregate lv "orders"
      [ B.doc [ ("$lookup", B.doc [ ("from", B.str "customers"); ("localField", B.str "cust");
                                    ("foreignField", B.str "_id"); ("as", B.str "c") ]) ] ]
  in
  let before = match one (Fur.peek r) with Some row -> B.get row "c" = Some (B.Array []) | None -> false in
  (* a change to the FOREIGN collection must retrigger the join *)
  MS.added (Live.store lv) ~sub:"a" ~collection:"customers" ~id:"c1" ~fields:[ ("name", B.str "Ada") ];
  let after =
    match one (Fur.peek r) with
    | Some row -> ( match B.get row "c" with Some (B.Array [ cust ]) -> B.get cust "name" = Some (B.String "Ada") | _ -> false)
    | None -> false
  in
  before && after

let () = exit (Fennec_hunt_unit.run ())
