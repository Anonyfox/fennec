(* The live-query engine, native side: ONE MongoDB change stream per collection, fanned out
   in-process to per-query views that maintain a result cache and emit field-level deltas. Reactive
   observe over real MongoDB, reusing the pure [Query] semantics (matcher/diff/projection) so this
   shares no code with minimongo yet behaves identically. A polling guard keeps the connection count
   under the pool size. Requires an ambient Eio switch (set via [set_switch]) and a replica set.

   CONCURRENCY: subscribes/stops arrive from any server domain; deltas dispatch from the stream
   daemons. One [_lock] guards the registries, sink/sub lists, and view caches — held only for pure
   lookups/commits, NEVER across driver IO ([raw_fetch]/[watch]/stream close all suspend on Eio: a
   stdlib mutex held across a suspension would block the whole domain) and never across callbacks
   (snapshot under the lock, deliver outside). A subscriber registers BEFORE its cache snapshot in
   one lock section, so an event arriving in between is delivered live AND present in the snapshot —
   re-delivery of such a prefix is tolerated by every consumer (the multiplexer / client merge box
   are idempotent), loss is impossible. *)

module Q = Query
module Bson_json = Fennec_mongo_bson_json.Bson_json

type handle = { stop : unit -> unit }

(* ambient switch for the change-stream daemons *)
let _sw : Eio.Switch.t option ref = ref None
let set_switch sw = _sw := Some sw

let require_sw () =
  match !_sw with Some s -> s | None -> failwith "Mongo.Live: no switch set. Call Live.set_switch ~sw."

(* the one registry/cache lock — pure sections only (see the header) *)
let _lock = Mutex.create ()

let with_lock f =
  Mutex.lock _lock;
  match f () with
  | v ->
      Mutex.unlock _lock;
      v
  | exception e ->
      Mutex.unlock _lock;
      raise e

(* a deferred query over a collection *)
type query = {
  coll : Collection.t;
  selector : Bson.t;
  sort : Bson.t;
  skip : int;
  limit : int;
  fields : Bson.t; (* projection spec *)
}

let query ?(selector = Bson.Document []) ?(sort = Bson.Document []) ?(skip = 0) ?(limit = 0)
    ?(fields = Bson.Document []) coll =
  { coll; selector; sort; skip; limit; fields }

let build_opts q =
  let kv = [] in
  let kv = match q.fields with Bson.Document [] -> kv | f -> ("projection", f) :: kv in
  let kv = if q.limit > 0 then ("limit", Bson.Int q.limit) :: kv else kv in
  let kv = if q.skip > 0 then ("skip", Bson.Int q.skip) :: kv else kv in
  let kv = match q.sort with Bson.Document [] -> kv | s -> ("sort", s) :: kv in
  Bson.Document kv

let raw_fetch q = Collection.find q.coll ~filter:q.selector ~opts:(build_opts q) ()
let coll_key q = q.coll.Collection.db ^ "." ^ q.coll.Collection.name

let ev_id (ev : Change_stream.event) =
  Q.Diff.id_to_string (Option.value ~default:Bson.Null (Bson.get ev.Change_stream.document_key "_id"))

(* canonical query key so identical (selector,opts) dedup to one view *)
let rec canon (d : Bson.t) : Bson.t =
  match d with
  | Bson.Document kvs ->
      Bson.Document (List.sort (fun (a, _) (b, _) -> String.compare a b) (List.map (fun (k, v) -> (k, canon v)) kvs))
  | Bson.Array xs -> Bson.Array (List.map canon xs)
  | x -> x

let query_key q = Bson_json.to_string (canon (Bson.Document [ ("s", q.selector); ("o", build_opts q) ]))

(* drive a change stream on a daemon fiber; close performed by the daemon itself after a poll cycle
   (libmongoc is not safe for concurrent close vs watch_next) *)
let spawn_observer cs ~dispatch : handle =
  let sw = require_sw () in
  let stopped = ref false in
  let done_p, done_r = Eio.Promise.create () in
  Eio.Fiber.fork_daemon ~sw (fun () ->
      let rec loop () =
        if !stopped then (Change_stream.close cs; Eio.Promise.resolve done_r (); `Stop_daemon)
        else ((match Change_stream.next cs with Some ev -> dispatch ev | None -> ()); loop ())
      in
      loop ());
  { stop = (fun () -> if not !stopped then (stopped := true; Eio.Promise.await done_p)) }

(* ---- instrumentation + guard (all reads/writes under [_lock]) ---- *)
let _streams_opened = ref 0
let _live_streams = ref 0
let _polled = ref 0
let _budget = ref 200
let _poll_interval = ref 0.25
let streams_opened_total () = with_lock (fun () -> !_streams_opened)
let live_streams () = with_lock (fun () -> !_live_streams)
let polled_collections () = with_lock (fun () -> !_polled)
let reset_stats () = with_lock (fun () -> _streams_opened := 0; _polled := 0)
let set_collection_stream_budget n = with_lock (fun () -> _budget := max 0 n)
let set_poll_interval s = _poll_interval := s

(* ---- one observer per collection ---- *)
type coll_obs = {
  ck : string;
  sinks : (int, Change_stream.event -> unit) Hashtbl.t;
  mutable handle : handle option; (* None while the stream is still being opened *)
  polled : bool;
  mutable refs : int;
}

let _collections : (string, coll_obs) Hashtbl.t = Hashtbl.create 16
let _sink_id = ref 0

(* find-or-create the per-collection observer. The registry section is pure; the stream OPEN (IO)
   happens outside the lock, after which the handle is published — a concurrent release that emptied
   the refs meanwhile is detected and the fresh stream closed (no orphan daemon). *)
let get_coll_obs q : coll_obs =
  let ck = coll_key q in
  let o, fresh =
    with_lock (fun () ->
        match Hashtbl.find_opt _collections ck with
        | Some o -> (o, false)
        | None ->
            let polled = !_live_streams >= !_budget in
            let o = { ck; sinks = Hashtbl.create 16; handle = None; polled; refs = 0 } in
            if polled then incr _polled else (incr _streams_opened; incr _live_streams);
            Hashtbl.replace _collections ck o;
            (o, true))
  in
  (if fresh && not o.polled then begin
     (* watch the WHOLE collection (no server $match): one stream serves all of this collection's
        query-views, and in-process routing needs every event — including move-outs whose post-image
        no longer matches. Open OUTSIDE the lock (it suspends); deliver outside it too. *)
     let cs = Change_stream.watch q.coll ~full_document:true ~max_await_ms:200 () in
     let dispatch ev =
       let sinks = with_lock (fun () -> Hashtbl.fold (fun _ s acc -> s :: acc) o.sinks []) in
       List.iter (fun s -> s ev) sinks
     in
     let base = spawn_observer cs ~dispatch in
     let released = ref false in
     let wrapped =
       { stop = (fun () -> base.stop (); with_lock (fun () -> if not !released then (released := true; decr _live_streams))) }
     in
     let orphaned =
       with_lock (fun () ->
           o.handle <- Some wrapped;
           (* if the table no longer maps [ck] to THIS obs (a release emptied it, possibly followed
              by a rebuild), nobody can ever release our handle — stop the fresh stream ourselves *)
           match Hashtbl.find_opt _collections ck with Some o' -> o' != o | None -> true)
     in
     if orphaned then wrapped.stop ()
   end);
  o

(* call under [_lock] *)
let retain o = o.refs <- o.refs + 1

(* drop a reference; returns the handle to stop (OUTSIDE the lock — stopping awaits the daemon) *)
let release_u o =
  o.refs <- o.refs - 1;
  if o.refs <= 0 then begin
    Hashtbl.remove _collections o.ck;
    let h = o.handle in
    o.handle <- None;
    h
  end
  else None

(* call under [_lock]; the returned detach must also run under it *)
let attach_sink_u o sink =
  incr _sink_id;
  let sid = !_sink_id in
  Hashtbl.replace o.sinks sid sink;
  fun () -> Hashtbl.remove o.sinks sid

let project_full proj fulldoc = Q.Projection.apply proj (Q.Diff.fields_without_id fulldoc)

let spawn_poller (step : unit -> unit) : unit -> unit =
  let stopped = ref false in
  let done_p, done_r = Eio.Promise.create () in
  Eio.Fiber.fork_daemon ~sw:(require_sw ()) (fun () ->
      let rec loop () =
        if !stopped then (Eio.Promise.resolve done_r (); `Stop_daemon)
        else ((try Internal.run (fun () -> Unix.sleepf !_poll_interval) with _ -> ()); if not !stopped then step (); loop ())
      in
      loop ());
  fun () -> if not !stopped then (stopped := true; Eio.Promise.await done_p)

(* ---- per-query views (field-level, deduped) ---- *)
type ucb = {
  ua : string -> Bson.t -> unit;
  uc : string -> Bson.t -> string list -> unit;
  ur : string -> unit;
}

type query_view = {
  vk : string;
  q : query;
  proj : Q.Projection.t;
  cache : (string, Bson.t) Hashtbl.t; (* under [_lock] *)
  mutable subs : (int * ucb) list; (* under [_lock] *)
  mutable vrefs : int;
  obs : coll_obs;
  mutable detach : unit -> unit; (* run under [_lock] (sink detach) — the poller variant is wrapped *)
}

let _views : (string, query_view) Hashtbl.t = Hashtbl.create 64
let _sub_id = ref 0

(* one delta for one view: COMPUTE + commit under the lock (pure: matcher/diff/projection), deliver
   to the subscriber snapshot outside it *)
let route v (ev : Change_stream.event) =
  let id = ev_id ev in
  let fulldoc = Option.value ~default:(Bson.Document []) ev.Change_stream.full_document in
  let deliveries =
    with_lock (fun () ->
        let fan mk = List.map (fun (_, s) -> fun () -> mk s) v.subs in
        match ev.Change_stream.op with
        | Change_stream.Delete ->
            if Hashtbl.mem v.cache id then (Hashtbl.remove v.cache id; fan (fun s -> s.ur id)) else []
        | Change_stream.Drop | Change_stream.Drop_database | Change_stream.Invalidate ->
            let ids = Hashtbl.fold (fun k _ acc -> k :: acc) v.cache [] in
            Hashtbl.clear v.cache;
            List.concat_map (fun i -> fan (fun s -> s.ur i)) ids
        | Change_stream.Insert | Change_stream.Update | Change_stream.Replace -> (
            let was = Hashtbl.mem v.cache id in
            let now = Q.Matcher.doc_matches v.q.selector fulldoc in
            match Q.Diff.transition ~was ~now with
            | Q.Diff.Entered ->
                let f = project_full v.proj fulldoc in
                Hashtbl.replace v.cache id f;
                fan (fun s -> s.ua id f)
            | Q.Diff.Stayed ->
                let nw = project_full v.proj fulldoc in
                let old = Option.value ~default:(Bson.Document []) (Hashtbl.find_opt v.cache id) in
                let chg, cl = Q.Diff.diff_fields ~old_doc:old ~new_doc:nw in
                Hashtbl.replace v.cache id nw;
                if chg <> [] || cl <> [] then fan (fun s -> s.uc id (Bson.Document chg) cl) else []
            | Q.Diff.Left ->
                Hashtbl.remove v.cache id;
                fan (fun s -> s.ur id)
            | Q.Diff.Outside -> [])
        | Change_stream.Rename | Change_stream.Other _ -> [])
  in
  List.iter (fun d -> d ()) deliveries

(* poller reconciliation: the fetch (IO) runs OUTSIDE the lock; the diff/commit inside; delivery after *)
let reconcile v =
  let fresh = raw_fetch v.q in
  let deliveries =
    with_lock (fun () ->
        let out = ref [] in
        let fan mk = List.iter (fun (_, s) -> out := (fun () -> mk s) :: !out) v.subs in
        let seen = Hashtbl.create 64 in
        List.iter
          (fun d ->
            let id = Q.Diff.doc_id d in
            let f = Q.Diff.fields_without_id d in
            Hashtbl.replace seen id ();
            match Hashtbl.find_opt v.cache id with
            | None ->
                Hashtbl.replace v.cache id f;
                fan (fun s -> s.ua id f)
            | Some old ->
                let chg, cl = Q.Diff.diff_fields ~old_doc:old ~new_doc:f in
                if chg <> [] || cl <> [] then begin
                  Hashtbl.replace v.cache id f;
                  fan (fun s -> s.uc id (Bson.Document chg) cl)
                end)
          fresh;
        let gone = Hashtbl.fold (fun id _ acc -> if Hashtbl.mem seen id then acc else id :: acc) v.cache [] in
        List.iter (fun id -> Hashtbl.remove v.cache id; fan (fun s -> s.ur id)) gone;
        List.rev !out)
  in
  List.iter (fun d -> d ()) deliveries

(* find-or-create the per-query view. Sink attach happens BEFORE the initial fetch, so an event
   landing in between is routed into the cache (not lost); the fetch then only fills ids the stream
   has not already written (never clobbering newer routed state with the older fetch). *)
let get_view q : query_view =
  let vk = coll_key q ^ "\x00" ^ query_key q in
  match with_lock (fun () -> Hashtbl.find_opt _views vk) with
  | Some v -> v
  | None ->
      let o = get_coll_obs q in
      (* re-check + create + attach in one section (a racing creator wins; we then use theirs) *)
      let v, fresh =
        with_lock (fun () ->
            match Hashtbl.find_opt _views vk with
            | Some v -> (v, false)
            | None ->
                let v =
                  { vk; q; proj = Q.Projection.of_fields q.fields; cache = Hashtbl.create 64; subs = [];
                    vrefs = 0; obs = o; detach = (fun () -> ()) }
                in
                retain o;
                (if not o.polled then begin
                   (* a WINDOWED query (skip/limit) can have a doc displaced/promoted by another doc's
                      change, so incremental routing is wrong — a relevant event instead re-runs the
                      windowed fetch and diffs (reconcile, the same machinery the poller uses; IO runs
                      outside the lock, and the stream daemon serializes these per collection). An
                      event that can't affect the window (not matching, not cached) is O(1)-skipped. *)
                   let windowed = q.limit > 0 || q.skip > 0 in
                   let sink =
                     if windowed then (fun ev ->
                       let id = ev_id ev in
                       let relevant =
                         with_lock (fun () -> Hashtbl.mem v.cache id)
                         ||
                         match ev.Change_stream.op with
                         | Change_stream.Insert | Change_stream.Update | Change_stream.Replace -> (
                             match ev.Change_stream.full_document with
                             | Some d -> Q.Matcher.doc_matches v.q.selector d
                             | None -> false)
                         | Change_stream.Delete -> false
                         | _ -> true (* drop/invalidate: always reconcile *)
                       in
                       if relevant then reconcile v)
                     else fun ev -> route v ev
                   in
                   let det = attach_sink_u o sink in
                   (* the stored detach is invoked OUTSIDE the lock (in stop), so it locks itself *)
                   v.detach <- (fun () -> with_lock det)
                 end);
                Hashtbl.replace _views vk v;
                (v, true))
      in
      if fresh then begin
        (* initial snapshot (IO, outside the lock), then merge-without-clobber under it *)
        let docs = raw_fetch q in
        with_lock (fun () ->
            List.iter
              (fun d ->
                let id = Q.Diff.doc_id d in
                if not (Hashtbl.mem v.cache id) then Hashtbl.replace v.cache id (Q.Diff.fields_without_id d))
              docs);
        if o.polled then begin
          let stop_poller = spawn_poller (fun () -> reconcile v) in
          with_lock (fun () -> v.detach <- (fun () -> stop_poller ()))
        end
      end;
      v

let observe_changes q ?(added = fun _ _ -> ()) ?(changed = fun _ _ _ -> ()) ?(removed = fun _ -> ()) () : handle =
  let v = get_view q in
  (* register FIRST, snapshot the cache in the SAME section: an event in between is delivered live
     AND present in the snapshot — duplicate-tolerated downstream; a loss is impossible *)
  let sid, replay =
    with_lock (fun () ->
        incr _sub_id;
        let sid = !_sub_id in
        v.subs <- (sid, { ua = added; uc = changed; ur = removed }) :: v.subs;
        v.vrefs <- v.vrefs + 1;
        (sid, Hashtbl.fold (fun id f acc -> (id, f) :: acc) v.cache []))
  in
  List.iter (fun (id, f) -> added id f) replay;
  let done_ = ref false in
  {
    stop =
      (fun () ->
        if not !done_ then begin
          done_ := true;
          (* poller teardown awaits its daemon, and a released stream stop awaits too — collect what
             to stop under the lock, run it OUTSIDE (it suspends) *)
          let to_stop =
            with_lock (fun () ->
                v.subs <- List.filter (fun (i, _) -> i <> sid) v.subs;
                v.vrefs <- v.vrefs - 1;
                if v.vrefs <= 0 then begin
                  Hashtbl.remove _views v.vk;
                  let detach = v.detach in
                  v.detach <- (fun () -> ());
                  Some (detach, release_u v.obs)
                end
                else None)
          in
          match to_stop with
          | None -> ()
          | Some (detach, stream) -> (
              detach ();
              match stream with Some h -> h.stop () | None -> ())
        end);
  }

(* ordered, positional observe — re-fetch + diff on relevant events. Single-flight: [resync] does IO
   + user callbacks, so it must run outside all locks; an event arriving mid-resync queues exactly
   one re-run (the classic run-again flag), keeping [prev] single-writer without ever blocking a
   domain on a suspended fetch. (No framework callers — the DDP path rides [observe_changes].) *)
let observe q ~added_before ~changed ~moved_before ~removed () : handle =
  let o = get_coll_obs q in
  with_lock (fun () -> retain o);
  let prev = ref [] in
  let busy = Atomic.make false in
  let again = Atomic.make false in
  let rec resync () =
    if Atomic.compare_and_set busy false true then begin
      let cur_list = List.map (fun d -> (Q.Diff.doc_id d, d)) (raw_fetch q) in
      Q.Diff.diff_ordered ~old_list:!prev ~new_list:cur_list ~added_before ~changed ~moved_before ~removed;
      prev := cur_list;
      Atomic.set busy false;
      if Atomic.exchange again false then resync ()
    end
    else Atomic.set again true
  in
  resync ();
  let detach =
    if o.polled then
      let stop_poller = spawn_poller resync in
      fun () -> stop_poller ()
    else begin
      let det =
        with_lock (fun () ->
            attach_sink_u o (fun ev ->
                let fulldoc = Option.value ~default:(Bson.Document []) ev.Change_stream.full_document in
                let id = ev_id ev in
                if Q.Matcher.doc_matches q.selector fulldoc || List.mem_assoc id !prev then resync ()))
      in
      fun () -> with_lock det
    end
  in
  let done_ = ref false in
  {
    stop =
      (fun () ->
        if not !done_ then begin
          done_ := true;
          detach ();
          let stream = with_lock (fun () -> release_u o) in
          match stream with Some h -> h.stop () | None -> ()
        end);
  }
