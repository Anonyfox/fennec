(* In-memory MongoDB (Minimongo) — the _id-keyed store, mutations, cursors, and the reactive
   observe engine. Mutations emit change events (the "simulated change stream"); observe/
   observeChanges recompute reactively off those events using the pure matcher/diff/transition core.
   No Eio, no polling, no systhreads — a mutation IS the event.

   CONCURRENCY: safe under OCaml 5 multicore (the fennec server runs one domain per core). The
   discipline, enforced structurally rather than by scattered guards:
   - [t.lock] guards every read snapshot and every commit; nothing else. No callback, no IO, and no
     other lock is ever taken under it except the Fanout queue's own (a leaf).
   - Change events enqueue into [t.fan] ATOMICALLY with their commit (under [t.lock]) and are
     delivered by Fanout's single drainer OUTSIDE all locks, in commit order — so observers may
     re-entrantly mutate the collection (no reentrant lock needed), a suspended delivery (socket IO)
     blocks nothing, and every observer sees a linearized stream.
   - The observe engines register BUFFERING and snapshot under the same [t.lock] acquisition, replay
     outside it, then flip ready — events landing during the replay are buffered, never lost or
     reordered (re-delivery of an event the snapshot already contains diffs to a no-op).
   Compiled to JavaScript all of this collapses to the old synchronous single-threaded behavior.

   Insertion order is kept as a REVERSED id list ([rorder], newest first) so insert is O(1); reads
   reverse it. All store lookups are total ([find_opt]) so a re-entrant observer that mutates the
   collection mid-notification can never raise. *)

open Bson
module Fanout = Fanout

type doc = Bson.t
type change_op = Insert | Update | Remove

(* A simulated change-stream event. *)
type change = {
  op : change_op;
  id : string;
  new_doc : doc option; (* full doc after insert/update; None for remove *)
  old_doc : doc option; (* full doc before update/remove; None for insert *)
}

type observer = change -> unit

type t = {
  lock : Mutex.t; (* guards store + rorder (commits and snapshots only — never callbacks) *)
  store : (string, doc) Hashtbl.t; (* _id -> full document *)
  mutable rorder : string list; (* ids newest-first (reverse insertion order); O(1) insert *)
  fan : change Fanout.t; (* the change stream: enqueued under [lock], delivered outside *)
  gen_id : unit -> string; (* must be pure/non-blocking: runs under [lock] *)
}

let create ?(gen_id = fun () -> Query.Id.random_id ()) () =
  { lock = Mutex.create (); store = Hashtbl.create 64; rorder = []; fan = Fanout.create (); gen_id }

let with_lock m f =
  Mutex.lock m;
  match f () with
  | v ->
      Mutex.unlock m;
      v
  | exception e ->
      Mutex.unlock m;
      raise e

type handle = { stop : unit -> unit }

(* raw simulated change stream: subscribe to insert/update/remove events from now on (tail of the
   stream — no replay, so it registers live immediately) *)
let watch t (f : observer) : handle =
  let sub = Fanout.subscribe t.fan ~ready:true f in
  { stop = (fun () -> Fanout.unsubscribe t.fan sub) }

(* the write fence: run [k] once every change event committed so far has been DELIVERED to the
   observers — see {!Fanout.on_drained} *)
let on_drained t k = Fanout.on_drained t.fan k

(* ids in insertion order — call under [t.lock] *)
let ids_unlocked t = List.rev t.rorder

(* ---- mutations ------------------------------------------------------------
   Shape of every mutation: with_lock (commit + Fanout.enqueue) → Fanout.pump. The enqueue-inside /
   pump-outside split is what makes delivery order the commit order without holding the lock across
   observer callbacks. *)

let ensure_id t (d : doc) : string * doc =
  match get d "_id" with
  | Some v -> (Query.Diff.id_to_string v, d)
  | None ->
      let id = t.gen_id () in
      (id, Document (("_id", String id) :: Query.Diff.kvs_of d))

(* commit an insert — call under [t.lock] *)
let insert_unlocked t (d : doc) : string =
  let id, d = ensure_id t d in
  if not (Hashtbl.mem t.store id) then t.rorder <- id :: t.rorder;
  Hashtbl.replace t.store id d;
  Fanout.enqueue t.fan { op = Insert; id; new_doc = Some d; old_doc = None };
  id

let insert t (d : doc) : string =
  let id = with_lock t.lock (fun () -> insert_unlocked t d) in
  Fanout.pump t.fan;
  id

(* matching ids in insertion order — call under [t.lock] *)
let matching_unlocked t selector =
  List.filter
    (fun id ->
      match Hashtbl.find_opt t.store id with
      | Some d -> Query.Matcher.doc_matches selector d
      | None -> false)
    (ids_unlocked t)

(* update by selector; returns number affected. The match + modify + commit run atomically. *)
let update t ?(multi = false) ?(upsert = false) (selector : doc) (modifier : doc) : int =
  let n =
    with_lock t.lock (fun () ->
        let ms = matching_unlocked t selector in
        let ms = if multi then ms else match ms with x :: _ -> [ x ] | [] -> [] in
        match ms with
        | [] when upsert ->
            (* seed an insert from the selector's plain equality fields (keep embedded documents;
               drop only operator keys and operator-expression values) + the modifier *)
            let base =
              Document
                (List.filter_map
                   (fun (k, v) ->
                     if Bson.is_operator_key k then None
                     else if Query.Modifier.is_operator_doc v then None
                     else Some (k, v))
                   (Query.Diff.kvs_of selector))
            in
            ignore (insert_unlocked t (Query.Modifier.apply ~insert:true base modifier));
            1
        | [] -> 0
        | _ ->
            let n = ref 0 in
            List.iter
              (fun id ->
                match Hashtbl.find_opt t.store id with
                | None -> ()
                | Some old ->
                    let nw = Query.Modifier.apply old modifier in
                    let nw =
                      match (get nw "_id", get old "_id") with
                      | None, Some idv -> Document (("_id", idv) :: Query.Diff.kvs_of nw)
                      | _ -> nw
                    in
                    Hashtbl.replace t.store id nw;
                    incr n;
                    Fanout.enqueue t.fan { op = Update; id; new_doc = Some nw; old_doc = Some old })
              ms;
            !n)
  in
  Fanout.pump t.fan;
  n

let remove t (selector : doc) : int =
  let n =
    with_lock t.lock (fun () ->
        let ms = matching_unlocked t selector in
        let dead = Hashtbl.create (List.length ms + 1) in
        List.iter (fun id -> Hashtbl.replace dead id ()) ms;
        List.iter
          (fun id ->
            match Hashtbl.find_opt t.store id with
            | Some old ->
                Hashtbl.remove t.store id;
                Fanout.enqueue t.fan { op = Remove; id; new_doc = None; old_doc = Some old }
            | None -> ())
          ms;
        (* single pass over the order list rather than one filter per removed id *)
        t.rorder <- List.filter (fun x -> not (Hashtbl.mem dead x)) t.rorder;
        List.length ms)
  in
  Fanout.pump t.fan;
  n

(* remove a single document BY its id — an O(1) hash delete (plus one order-list compaction),
   skipping the O(n) selector scan [remove {_id: …}] would run. Returns whether a doc was present. *)
let remove_id t (id : string) : bool =
  let removed =
    with_lock t.lock (fun () ->
        match Hashtbl.find_opt t.store id with
        | None -> false
        | Some old ->
            Hashtbl.remove t.store id;
            t.rorder <- List.filter (fun x -> x <> id) t.rorder;
            Fanout.enqueue t.fan { op = Remove; id; new_doc = None; old_doc = Some old };
            true)
  in
  if removed then Fanout.pump t.fan;
  removed

(* ---- cursors / queries ---------------------------------------------------- *)

type cursor = {
  coll : t;
  selector : doc;
  sort : doc;
  skip : int;
  limit : int; (* 0 = unbounded *)
  fields : doc; (* projection spec *)
}

let find t ?(selector = Document []) ?(sort = Document []) ?(skip = 0) ?(limit = 0)
    ?(fields = Document []) () =
  { coll = t; selector; sort; skip; limit; fields }

(* the pure window pipeline — call the *_unlocked forms under [cur.coll.lock] *)
let all_docs_unlocked t = List.filter_map (Hashtbl.find_opt t.store) (ids_unlocked t)
let matched_unlocked cur = List.filter (Query.Matcher.doc_matches cur.selector) (all_docs_unlocked cur.coll)

let windowed_unlocked cur =
  let xs = Query.Sorter.sort cur.sort (matched_unlocked cur) in
  let rec drop n = function [] -> [] | _ :: tl when n > 0 -> drop (n - 1) tl | l -> l in
  let xs = if cur.skip > 0 then drop cur.skip xs else xs in
  if cur.limit > 0 then
    let rec take n acc = function x :: tl when n > 0 -> take (n - 1) (x :: acc) tl | _ -> List.rev acc in
    take cur.limit [] xs
  else xs

(* a consistent snapshot of the cursor's window (the lock covers only the snapshot, not callers) *)
let windowed cur = with_lock cur.coll.lock (fun () -> windowed_unlocked cur)
let projection cur = Query.Projection.of_fields cur.fields
let fetch cur = List.map (Query.Projection.apply (projection cur)) (windowed cur)

let count cur =
  with_lock cur.coll.lock (fun () ->
      List.fold_left
        (fun n d -> if Query.Matcher.doc_matches cur.selector d then n + 1 else n)
        0
        (all_docs_unlocked cur.coll))

let is_empty cur =
  with_lock cur.coll.lock (fun () ->
      not (List.exists (Query.Matcher.doc_matches cur.selector) (all_docs_unlocked cur.coll)))

let for_each cur f = List.iter f (fetch cur)
let map cur f = List.map f (fetch cur)
let first cur = match fetch cur with x :: _ -> Some x | [] -> None

let find_one t ?(selector = Document []) ?(sort = Document []) ?(skip = 0)
    ?(fields = Document []) () =
  first (find t ~selector ~sort ~skip ~limit:1 ~fields ())

(* run an aggregation pipeline over a SNAPSHOT of the collection (insertion order). The pipeline —
   and the [lookup] resolver, which may take OTHER collections' locks — runs outside this
   collection's lock, so cross-collection $lookup can never nest locks (deadlock-free by shape). *)
let aggregate ?(lookup = fun _ -> []) t (pipeline : Bson.t list) : doc list =
  let docs = with_lock t.lock (fun () -> all_docs_unlocked t) in
  Query.Aggregate.run ~lookup pipeline docs

(* distinct values of [key] over the documents matching [selector]; MongoDB unwraps array values
   (distinct over an array field yields its distinct elements), deduped by BSON equality *)
let distinct t ~key ?(selector = Document []) () : doc list =
  let push acc v = if List.exists (Bson.equal v) acc then acc else v :: acc in
  let add acc d =
    match Query.Matcher.get_path d key with
    | None -> acc
    | Some (Array xs) -> List.fold_left push acc xs
    | Some v -> push acc v
  in
  List.rev (List.fold_left add [] (fetch (find t ~selector ())))

(* ---- reactive observation -------------------------------------------------
   Both engines follow the buffered-replay protocol: register BUFFERING + snapshot under ONE lock
   acquisition (so the snapshot is exactly the delivered-prefix state), replay the snapshot outside
   the lock, then flip ready — Fanout delivers anything that landed in between, in order. An event
   the snapshot already contained re-delivers harmlessly (the cache/diff turns it into a no-op). *)

(* observeChanges — field-level, unordered membership routing. Honors selector + projection on live
   deltas. A WINDOWED cursor (skip/limit > 0) maintains its window live: a relevant event re-snapshots
   the window and diffs it against the cache, so enter/leave/displacement all surface as membership
   deltas and the cache never exceeds the window. Costs: an un-windowed delta is O(fields) (the
   single changed doc); a windowed delta is O(M log M + window) for the M selector-matching docs —
   and O(1) for writes that can't affect the window (non-matching, not cached). *)
let observe_changes cur ?(added = fun _ _ -> ()) ?(changed = fun _ _ _ -> ())
    ?(removed = fun _ -> ()) () : handle =
  let t = cur.coll in
  let p = projection cur in
  let cache : (string, doc) Hashtbl.t = Hashtbl.create 64 in
  (* the cheap incremental path: membership = the selector alone, so one event routes in O(fields) *)
  let route_incremental (ch : change) =
    let id = ch.id in
    match ch.op with
    | Remove -> if Hashtbl.mem cache id then ( Hashtbl.remove cache id; removed id)
    | Insert | Update -> (
        let full = match ch.new_doc with Some d -> d | None -> Document [] in
        let was = Hashtbl.mem cache id in
        let now = Query.Matcher.doc_matches cur.selector full in
        match Query.Diff.transition ~was ~now with
        | Entered ->
            let f = Query.Projection.apply p (Query.Diff.fields_without_id full) in
            Hashtbl.replace cache id f;
            added id f
        | Stayed ->
            let nw = Query.Projection.apply p (Query.Diff.fields_without_id full) in
            let old = match Hashtbl.find_opt cache id with Some o -> o | None -> Document [] in
            let chg, cleared = Query.Diff.diff_fields ~old_doc:old ~new_doc:nw in
            Hashtbl.replace cache id nw;
            if chg <> [] || cleared <> [] then changed id (Document chg) cleared
        | Left ->
            Hashtbl.remove cache id;
            removed id
        | Outside -> ())
  in
  (* the windowed path: membership depends on sort/skip/limit, so a doc entering can displace another
     and a doc leaving promotes one — a relevant event re-snapshots the window (a consistent locked
     read; we are in delivery, outside all locks) and field-diffs it against the cache.

     Relevance is exact: a write can affect the window iff it touches a WINDOW doc, its new form
     MATCHES the selector (it might enter), or — with skip > 0 — its OLD form matched (a doc leaving
     the skipped prefix shifts every rank below it, changing the window without itself being in it).

     The boundary short-circuit makes the dominant miss case O(1): with skip = 0 and a FULL window,
     a matching out-of-window doc that sorts STRICTLY below the last window doc cannot enter — no
     re-snapshot. (skip = 0 only: a prefix interaction can shift the window from above. Strict
     comparison only: ties re-snapshot, since relative order at a tie is the sorter's business. The
     boundary is kept un-projected, so the comparator always sees the sort fields. An empty sort spec
     compares everything equal — never strictly below — so insertion-order windows never
     short-circuit.) Per relevant write the cost stays O(N + M log M) over the M matching docs; per
     short-circuited write it is one match + one compare. *)
  let cmp = Query.Sorter.of_spec cur.sort in
  let boundary : doc option ref = ref None in
  let set_boundary fresh =
    boundary :=
      (if cur.skip = 0 && cur.limit > 0 && List.length fresh = cur.limit then
         List.nth_opt fresh (cur.limit - 1)
       else None)
  in
  let route_windowed (ch : change) =
    let matches = function Some d -> Query.Matcher.doc_matches cur.selector d | None -> false in
    let in_window = Hashtbl.mem cache ch.id in
    let matches_new = match ch.op with Remove -> false | Insert | Update -> matches ch.new_doc in
    let prefix_affected = cur.skip > 0 && matches ch.old_doc in
    let cannot_enter =
      (not in_window) && matches_new && (not prefix_affected) && cur.skip = 0
      && Hashtbl.length cache = cur.limit
      && match (ch.new_doc, !boundary) with Some d, Some b -> cmp d b > 0 | _ -> false
    in
    if (in_window || matches_new || prefix_affected) && not cannot_enter then begin
      let fresh = windowed cur in
      set_boundary fresh;
      let fresh_tbl : (string, doc) Hashtbl.t = Hashtbl.create 16 in
      List.iter
        (fun d ->
          Hashtbl.replace fresh_tbl (Query.Diff.doc_id d)
            (Query.Projection.apply p (Query.Diff.fields_without_id d)))
        fresh;
      let gone = Hashtbl.fold (fun id _ acc -> if Hashtbl.mem fresh_tbl id then acc else id :: acc) cache [] in
      List.iter (fun id -> Hashtbl.remove cache id; removed id) gone;
      Hashtbl.iter
        (fun id f ->
          match Hashtbl.find_opt cache id with
          | None ->
              Hashtbl.replace cache id f;
              added id f
          | Some old ->
              let chg, cleared = Query.Diff.diff_fields ~old_doc:old ~new_doc:f in
              if chg <> [] || cleared <> [] then begin
                Hashtbl.replace cache id f;
                changed id (Document chg) cleared
              end)
        fresh_tbl
    end
  in
  let route = if cur.skip > 0 || cur.limit > 0 then route_windowed else route_incremental in
  let sub, initial =
    with_lock t.lock (fun () -> (Fanout.subscribe t.fan ~ready:false route, windowed_unlocked cur))
  in
  set_boundary initial;
  List.iter
    (fun d ->
      let id = Query.Diff.doc_id d in
      let f = Query.Projection.apply p (Query.Diff.fields_without_id d) in
      Hashtbl.replace cache id f;
      added id f)
    initial;
  Fanout.ready t.fan sub;
  { stop = (fun () -> Fanout.unsubscribe t.fan sub) }

(* observe — document-level. Recomputes the ordered window and diffs, so sort/skip/limit are
   honored and callbacks receive full documents. *)
let observe cur ?(added = fun _ -> ()) ?(changed = fun _ _ -> ()) ?(removed = fun _ -> ()) () :
    handle =
  let t = cur.coll in
  let snap_unlocked () = List.map (fun d -> (Query.Diff.doc_id d, d)) (windowed_unlocked cur) in
  let prev = ref [] in
  let recompute (_ : change) =
    let nw = with_lock t.lock (fun () -> snap_unlocked ()) in
    Query.Diff.diff_ordered ~old_list:!prev ~new_list:nw
      ~added_before:(fun id _ _ -> match List.assoc_opt id nw with Some d -> added d | None -> ())
      ~changed:(fun id _ _ ->
        match (List.assoc_opt id nw, List.assoc_opt id !prev) with
        | Some d, Some o -> changed d o
        | Some d, None -> changed d d
        | _ -> ())
      ~moved_before:(fun _ _ -> ())
      ~removed:(fun id -> match List.assoc_opt id !prev with Some o -> removed o | None -> ());
    prev := nw
  in
  let sub, initial =
    with_lock t.lock (fun () -> (Fanout.subscribe t.fan ~ready:false recompute, snap_unlocked ()))
  in
  prev := initial;
  List.iter (fun (_, d) -> added d) initial;
  Fanout.ready t.fan sub;
  { stop = (fun () -> Fanout.unsubscribe t.fan sub) }
