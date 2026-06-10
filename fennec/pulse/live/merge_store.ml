(* The client-side merge store — Meteor's SessionDocumentView relocated to the client (DATAFLOW.md
   §5b). The server is stateless on the hot path: it forwards each subscription's observe delta
   TAGGED with the sub id, and the client does the merge here, where it is cheap and distributed
   (the client holds every visible doc in minimongo anyway).

   Per collection: one Minimongo store holding the WINNING (merged) documents. Per (collection, id):
   a [doc_view] with
     - exists_in : which subs currently include the doc (refcount for removal),
     - fields    : per field, a precedence list ordered by subscription precedence (earliest sub
                   wins; on clear/remove the next sub's value takes over).
   Each sub also remembers the set of (collection,id) it touched, so stopping a sub is O(that sub's
   docs). The collection carries a version + listeners so the Fur binding can recompute. Pure
   (bson + minimongo) → native AND JS.

   CONCURRENCY: in the browser the store is single-threaded; on the server one SSR-shared store may
   be touched by several domains' render fibers. One [t.lock] guards every read snapshot and every
   mutation; listener notification ("fires") ALWAYS happens after the lock is released — a listener
   may freely re-enter (Live's recompute calls [fetch]). Each public mutation is one atomic section
   whose touched collections fire exactly ONCE afterwards (the burst coalescing this also buys is
   what keeps a W-doc seed at O(W) instead of O(W²) re-snapshots). *)

module C = Minimongo

type field_entry = { sub : string; prec : int; value : Bson.t }

type doc_view = {
  exists_in : (string, int) Hashtbl.t; (* subId -> precedence *)
  fields : (string, field_entry list) Hashtbl.t; (* field -> precedence list (asc by prec) *)
  (* sim subs currently HIDING this doc (an optimistic delete): while non-empty the doc is absent
     from the visible store but the view survives — dropping the sim restores it (precedence can't
     express absence, so hiding is its own axis) *)
  hidden_by : (string, unit) Hashtbl.t;
}

type collection_view = {
  store : C.t;
  docs : (string, doc_view) Hashtbl.t;
  listeners : (int, unit -> unit) Hashtbl.t;
  mutable lc : int;
  mutable version : int;
  (* ids in this collection are Object_id (a MONGO-idGeneration collection), so [recompute]
     reconstructs the typed [_id] — the DDP wire and the seed carry it as a hex string, but a MONGO
     collection's documents must keep [Bson.Object_id] so [find]/[$lookup] by [_id] match the server.
     Inferred from a seeded document's typed [_id]; a live-only MONGO collection (never seeded)
     defaults to string ids. *)
  mutable oid : bool;
  (* memoized full-collection fetch for $lookup resolution, keyed by [version]: a primary-only change
     bumps the primary's version but not a foreign's, so the foreign fetch is reused, not rebuilt on
     every aggregate recompute *)
  mutable agg_cache : (int * Bson.t list) option;
  (* O(1) dirty-set membership within a mutation section — avoids an O(W) List.memq per touched view *)
  mutable in_dirty : bool;
}

type sub_info = {
  order : int; (* subscription precedence: lower = wins (distinct from field_entry.prec) *)
  contributed : (string, string * string) Hashtbl.t; (* key -> (collection,id) *)
}

type t = {
  lock : Mutex.t; (* guards everything below; never held across a listener fire *)
  collections : (string, collection_view) Hashtbl.t;
  subs : (string, sub_info) Hashtbl.t;
  (* per sub: docs it SEEDED (SSR) that the live snapshot hasn't re-confirmed yet — dropped on the
     sub's first [ready] (quiescence), so a doc deleted between SSR and the socket opening doesn't
     linger as a stale fast-render row *)
  tentative : (string, (string * string, unit) Hashtbl.t) Hashtbl.t;
  mutable seq : int;
  (* sim subs get precedences from a NEGATIVE, descending band: every simulation wins over every
     real subscription, and a later simulation wins over an earlier one (per field) *)
  mutable sim_seq : int;
  (* the collections a mutation section touched — their listeners fire (once each) after unlock *)
  mutable dirty : collection_view list;
}

let create () =
  { lock = Mutex.create (); collections = Hashtbl.create 8; subs = Hashtbl.create 16;
    tentative = Hashtbl.create 16; seq = 0; sim_seq = 0; dirty = [] }

let with_lock t f =
  Mutex.lock t.lock;
  match f () with
  | v ->
      Mutex.unlock t.lock;
      v
  | exception e ->
      Mutex.unlock t.lock;
      raise e

(* run a mutation as one atomic section, then fire each touched collection's listeners ONCE, outside
   the lock (so a listener can re-enter — Live's recompute calls [fetch]) *)
let mutate t f =
  let v, fired =
    with_lock t (fun () ->
        let v = f () in
        let d = t.dirty in
        t.dirty <- [];
        List.iter (fun cv -> cv.in_dirty <- false) d;
        (v, d))
  in
  List.iter (fun cv -> Hashtbl.iter (fun _ f -> f ()) cv.listeners) fired;
  v

(* ---- collections (internal: call under [t.lock]) -------------------------- *)

let ensure_collection t name : collection_view =
  match Hashtbl.find_opt t.collections name with
  | Some cv -> cv
  | None ->
      let cv =
        { store = C.create (); docs = Hashtbl.create 64; listeners = Hashtbl.create 4; lc = 0; version = 0;
          oid = false; agg_cache = None; in_dirty = false }
      in
      Hashtbl.replace t.collections name cv;
      cv

(* bump the version + mark the view for the post-unlock notify (coalesced: once per section) *)
let bump t cv =
  cv.version <- cv.version + 1;
  if not cv.in_dirty then begin
    cv.in_dirty <- true;
    t.dirty <- cv :: t.dirty
  end

let store t name = with_lock t (fun () -> (ensure_collection t name).store)
let version t name = with_lock t (fun () -> match Hashtbl.find_opt t.collections name with Some cv -> cv.version | None -> 0)

let on_change t name f =
  with_lock t (fun () ->
      let cv = ensure_collection t name in
      cv.lc <- cv.lc + 1;
      let id = cv.lc in
      Hashtbl.replace cv.listeners id f;
      id)

let off_change t name id =
  with_lock t (fun () ->
      match Hashtbl.find_opt t.collections name with Some cv -> Hashtbl.remove cv.listeners id | None -> ())

(* ---- subscriptions registry (internal) ------------------------------------ *)

let ensure_sub t sub : sub_info =
  match Hashtbl.find_opt t.subs sub with
  | Some i -> i
  | None ->
      let i = { order = t.seq; contributed = Hashtbl.create 32 } in
      t.seq <- t.seq + 1;
      Hashtbl.replace t.subs sub i;
      i

let sub_prec t sub = match Hashtbl.find_opt t.subs sub with Some i -> i.order | None -> max_int

let track t sub collection id =
  match Hashtbl.find_opt t.subs sub with
  | Some i -> Hashtbl.replace i.contributed (collection ^ "\000" ^ id) (collection, id)
  | None -> ()

let untrack t sub collection id =
  match Hashtbl.find_opt t.subs sub with
  | Some i -> Hashtbl.remove i.contributed (collection ^ "\000" ^ id)
  | None -> ()

(* ---- field precedence (internal) ------------------------------------------ *)

let set_field dv field sub prec value =
  let cur = match Hashtbl.find_opt dv.fields field with Some l -> l | None -> [] in
  let without = List.filter (fun e -> e.sub <> sub) cur in
  let sorted = List.sort (fun a b -> compare a.prec b.prec) ({ sub; prec; value } :: without) in
  Hashtbl.replace dv.fields field sorted

let clear_field dv field sub =
  match Hashtbl.find_opt dv.fields field with
  | None -> ()
  | Some cur -> (
      match List.filter (fun e -> e.sub <> sub) cur with
      | [] -> Hashtbl.remove dv.fields field
      | l -> Hashtbl.replace dv.fields field l)

let drop_sub_from_doc dv sub =
  Hashtbl.remove dv.exists_in sub;
  Hashtbl.remove dv.hidden_by sub;
  (* iterate over a snapshot of keys since clear_field mutates the table *)
  let fkeys = Hashtbl.fold (fun k _ acc -> k :: acc) dv.fields [] in
  List.iter (fun f -> clear_field dv f sub) fkeys

(* rebuild the winning document into minimongo (or remove it) and mark the view dirty *)
let recompute t cv id dv =
  (* a MONGO collection keeps its Object_id _id (the wire/seed carry the hex as a string) *)
  let id_val = if cv.oid then Bson.Object_id id else Bson.String id in
  if Hashtbl.length dv.exists_in = 0 && Hashtbl.length dv.hidden_by = 0 then begin
    (* gone from every sub → keyed delete (O(1) hash remove, not an O(n) selector scan) *)
    ignore (C.remove_id cv.store id);
    Hashtbl.remove cv.docs id
  end
  else if Hashtbl.length dv.hidden_by > 0 then
    (* optimistically deleted: absent from the visible store, but the view survives so dropping the
       sim (or a real removal) recomputes it back into existence / out entirely *)
    ignore (C.remove_id cv.store id)
  else begin
    (* still present → INSERT overwrites the store entry with the full merged doc and leaves [rorder]
       untouched when the id is already there (insert only conses a NEW id), so the common change/add
       path no longer pays the O(n) rorder compaction a remove+insert would *)
    let kvs =
      Hashtbl.fold (fun f entries acc -> match entries with e :: _ -> (f, e.value) :: acc | [] -> acc) dv.fields []
    in
    ignore (C.insert cv.store (Bson.Document (("_id", id_val) :: kvs)))
  end;
  bump t cv

let doc_of cv id =
  match Hashtbl.find_opt cv.docs id with
  | Some dv -> dv
  | None ->
      let dv = { exists_in = Hashtbl.create 2; fields = Hashtbl.create 8; hidden_by = Hashtbl.create 1 } in
      Hashtbl.replace cv.docs id dv;
      dv

(* ---- the sub-tagged DDP data ops ------------------------------------------
   Internal *_u forms run under [t.lock]; the public ops below wrap them in [mutate]. *)

(* quiescence bookkeeping: [seed] marks a doc tentative for its sub; a live [added]/[changed] confirms
   (clears) it; [quiesce] (on the sub's first [ready]) drops whatever stayed tentative. *)
let mark_tentative_u t sub collection id =
  let set =
    match Hashtbl.find_opt t.tentative sub with
    | Some s -> s
    | None ->
        let s = Hashtbl.create 16 in
        Hashtbl.replace t.tentative sub s;
        s
  in
  Hashtbl.replace set (collection, id) ()

let confirm_u t sub collection id =
  match Hashtbl.find_opt t.tentative sub with Some s -> Hashtbl.remove s (collection, id) | None -> ()

(* added: this sub now includes [id] in [collection] with [fields] *)
let added_u t ~sub ~collection ~id ~fields =
  let _ = ensure_sub t sub in
  let cv = ensure_collection t collection in
  let dv = doc_of cv id in
  let prec = sub_prec t sub in
  Hashtbl.replace dv.exists_in sub prec;
  List.iter (fun (f, v) -> set_field dv f sub prec v) fields;
  track t sub collection id;
  recompute t cv id dv;
  confirm_u t sub collection id

(* changed: this sub updated [fields] / unset [cleared] of a doc *)
let changed_u t ~sub ~collection ~id ~fields ~cleared =
  let _ = ensure_sub t sub in
  let cv = ensure_collection t collection in
  let dv = doc_of cv id in
  let prec = sub_prec t sub in
  (* tolerate a [changed] for a doc this sub hasn't [added] yet — an out-of-order frame, or a real
     Meteor server re-confirming a doc via [changed] (e.g. after another sub had dropped it locally).
     Ensure this sub's membership + track it, so the row isn't lost and a following [quiesce] won't
     drop a still-live doc. For an already-present doc this is idempotent (same sub, same prec). *)
  Hashtbl.replace dv.exists_in sub prec;
  List.iter (fun (f, v) -> set_field dv f sub prec v) fields;
  List.iter (fun f -> clear_field dv f sub) cleared;
  track t sub collection id;
  recompute t cv id dv;
  confirm_u t sub collection id

(* removed: this sub no longer includes [id] *)
let removed_u t ~sub ~collection ~id =
  match Hashtbl.find_opt t.collections collection with
  | None -> ()
  | Some cv -> (
      match Hashtbl.find_opt cv.docs id with
      | None -> ()
      | Some dv -> drop_sub_from_doc dv sub; untrack t sub collection id; recompute t cv id dv)

let added t ~sub ~collection ~id ~fields = mutate t (fun () -> added_u t ~sub ~collection ~id ~fields)

let changed t ~sub ~collection ~id ~fields ~cleared =
  mutate t (fun () -> changed_u t ~sub ~collection ~id ~fields ~cleared)

let removed t ~sub ~collection ~id = mutate t (fun () -> removed_u t ~sub ~collection ~id)

(* sub_stopped: drop everything this sub contributed (O(that sub's docs)) *)
let sub_stopped t sub =
  mutate t (fun () ->
      match Hashtbl.find_opt t.subs sub with
      | None -> ()
      | Some info ->
          Hashtbl.iter
            (fun _ (collection, id) ->
              match Hashtbl.find_opt t.collections collection with
              | Some cv -> (
                  match Hashtbl.find_opt cv.docs id with
                  | Some dv -> drop_sub_from_doc dv sub; recompute t cv id dv
                  | None -> ())
              | None -> ())
            info.contributed;
          Hashtbl.remove t.subs sub;
          Hashtbl.remove t.tentative sub)

(* ---- queries --------------------------------------------------------------- *)

let fetch t name ?selector ?sort ?skip ?limit ?fields () : Bson.t array =
  let st = store t name in
  Array.of_list (C.fetch (C.find st ?selector ?sort ?skip ?limit ?fields ()))

(* aggregation over a collection, with $lookup / $unionWith foreign collections resolved across the
   client's OTHER collections — the same multi-collection joins the server does, now on the client.
   One lock acquisition = a consistent multi-collection snapshot (the pipeline is pure compute; the
   inner minimongo calls take only their own leaf locks). *)
let aggregate t name (pipeline : Bson.t list) : Bson.t array =
  with_lock t (fun () ->
      let cv = ensure_collection t name in
      (* resolve a foreign collection, MEMOIZED by its [version]: a primary-only change reuses the
         foreign's last fetch instead of re-materializing it on every recompute *)
      let lookup other =
        match Hashtbl.find_opt t.collections other with
        | None -> []
        | Some o -> (
            match o.agg_cache with
            | Some (v, docs) when v = o.version -> docs
            | _ ->
                let docs = C.fetch (C.find o.store ()) in
                o.agg_cache <- Some (o.version, docs);
                docs)
      in
      Array.of_list (C.aggregate cv.store ~lookup pipeline))

(* SSR / hydration seed: install docs into a collection as if from one sub. One atomic section, so
   the whole burst fires each touched collection ONCE (O(W), not O(W²) re-snapshots). *)
let seed t ~sub ~collection (docs : Bson.t list) =
  let id_of kvs =
    match List.assoc_opt "_id" kvs with Some (Bson.String s) | Some (Bson.Object_id s) -> Some s | _ -> None
  in
  (* FIRST pass: if ANY doc carries an Object_id _id, this collection is MONGO — set [oid] ONCE, up
     front, so a doc seeded AHEAD of the first Object_id isn't materialized with a String _id and left
     mistyped (a mixed or legacy-ordered batch; the RX6 typed-_id invariant must hold for the whole
     group, not just the docs after the first typed one). *)
  let mongo =
    List.exists
      (fun d ->
        match d with
        | Bson.Document kvs -> ( match List.assoc_opt "_id" kvs with Some (Bson.Object_id _) -> true | _ -> false)
        | _ -> false)
      docs
  in
  mutate t (fun () ->
      if mongo then (ensure_collection t collection).oid <- true;
      List.iter
        (fun d ->
          match d with
          | Bson.Document kvs -> (
              (* a string OR Object_id _id is accepted; a doc with no usable _id is skipped (never "") *)
              match id_of kvs with
              | None -> ()
              | Some id ->
                  let fields = List.filter (fun (k, _) -> k <> "_id") kvs in
                  added_u t ~sub ~collection ~id ~fields;
                  mark_tentative_u t sub collection id (* tentative until the live snapshot confirms *))
          | _ -> ())
        docs)

(* on a sub's first [ready], drop any doc it SEEDED but the live snapshot didn't re-confirm — the
   quiescence pass that keeps SSR fast-render from leaving stale rows behind.

   Correctness rests on the DDP contract that the server sends ALL of a publication's initial documents
   BEFORE [ready] (and the wire preserves order), so by the time the client handles [ready] it has
   already applied every confirming [added]. fennec honors this by replaying the initial set
   synchronously inside [run_publication] before emitting ready — for both the in-memory backend and
   the native change-stream driver (ready-after-data). A backend that emitted [ready] before its
   replay would violate the contract and could drop a valid seeded doc here. *)
let quiesce t sub =
  mutate t (fun () ->
      match Hashtbl.find_opt t.tentative sub with
      | None -> ()
      | Some set ->
          Hashtbl.iter (fun (collection, id) () -> removed_u t ~sub ~collection ~id) set;
          Hashtbl.remove t.tentative sub)

(* ---- optimistic simulation (latency compensation) --------------------------
   A method stub's writes ride a SIM SUB: a virtual subscription from the negative precedence band,
   so its field values win over every real subscription instantly — and "rollback" when the server's
   [updated] arrives is just {!sub_stopped}: the precedence fallthrough this store already implements
   reveals server truth with no bespoke undo machinery. Deletes need their own axis (precedence can't
   express absence): {!sim_hide} tombstones a doc until the sim drops. *)

(* register [sub] as a simulation: top precedence, later sims over earlier (per field) *)
let begin_sim t sub =
  with_lock t (fun () ->
      if not (Hashtbl.mem t.subs sub) then begin
        t.sim_seq <- t.sim_seq - 1;
        Hashtbl.replace t.subs sub { order = t.sim_seq; contributed = Hashtbl.create 16 }
      end)

(* optimistically DELETE [id] for the duration of [sub] (a sim registered via {!begin_sim}): the doc
   leaves the visible store but its view survives — dropping the sim restores it (unless a real
   removal landed meanwhile, in which case it stays gone) *)
let sim_hide t ~sub ~collection ~id =
  mutate t (fun () ->
      match Hashtbl.find_opt t.collections collection with
      | None -> ()
      | Some cv -> (
          match Hashtbl.find_opt cv.docs id with
          | None -> ()
          | Some dv ->
              Hashtbl.replace dv.hidden_by sub ();
              track t sub collection id;
              recompute t cv id dv))

(* on reconnect: re-mark everything [sub] currently holds as tentative, so the resubscription's fresh
   snapshot (which re-adds the still-present docs, confirming them) plus the [ready] {!quiesce} drops
   whatever the server stopped sending during the outage — the same quiescence pass that heals the
   SSR seed, reused to heal the cache after a dropped socket. *)
let resync_begin t sub =
  with_lock t (fun () ->
      match Hashtbl.find_opt t.subs sub with
      | None -> ()
      | Some info -> Hashtbl.iter (fun _ (collection, id) -> mark_tentative_u t sub collection id) info.contributed)
