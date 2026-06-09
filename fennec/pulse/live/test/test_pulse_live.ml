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

let%test "seed: round-trips the documents AND their collection" =
  let docs = [ B.doc [ ("_id", B.str "1"); ("n", B.int 1) ]; B.doc [ ("_id", B.str "2") ] ] in
  match Seed.decode (Seed.encode ~collection:"messages" docs) with
  | Some ("messages", got) -> List.length got = 2 && B.get (List.hd got) "n" = Some (B.Int 1)
  | _ -> false

let%test "seed: the collection travels independently of the publication name (name <> collection)" =
  (* a publication named "inbox" feeding collection "messages" seeds under "messages", so the client
     installs there and find/live (which use the real collection) line up *)
  match Seed.decode (Seed.encode ~collection:"messages" [ B.doc [ ("_id", B.str "1") ] ]) with
  | Some (c, _) -> c = "messages"
  | None -> false

let%test "seed: a malformed / legacy payload decodes to None (no crash)" =
  Seed.decode "not json" = None && Seed.decode "[1,2,3]" = None

let () = exit (Fennec_hunt_unit.run ())
