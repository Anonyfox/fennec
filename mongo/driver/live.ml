(* The live-query engine, native side: ONE MongoDB change stream per collection, fanned out
   in-process to per-query views that maintain a result cache and emit field-level deltas. Reactive
   observe over real MongoDB, reusing the pure [Query] semantics (matcher/diff/projection) so this
   shares no code with minimongo yet behaves identically. A polling guard keeps the connection count
   under the pool size. Requires an ambient Eio switch (set via [set_switch]) and a replica set. *)

module Q = Query
module Bson_json = Fennec_mongo_bson_json.Bson_json

type handle = { stop : unit -> unit }

(* ambient switch for the change-stream daemons *)
let _sw : Eio.Switch.t option ref = ref None
let set_switch sw = _sw := Some sw

let require_sw () =
  match !_sw with Some s -> s | None -> failwith "Mongo.Live: no switch set. Call Live.set_switch ~sw."

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

(* ---- instrumentation + guard ---- *)
let _streams_opened = ref 0
let _live_streams = ref 0
let _polled = ref 0
let _budget = ref 200
let _poll_interval = ref 0.25
let streams_opened_total () = !_streams_opened
let live_streams () = !_live_streams
let polled_collections () = !_polled
let reset_stats () = _streams_opened := 0; _polled := 0
let set_collection_stream_budget n = _budget := max 0 n
let set_poll_interval s = _poll_interval := s

(* ---- one observer per collection ---- *)
type coll_obs = {
  ck : string;
  sinks : (int, Change_stream.event -> unit) Hashtbl.t;
  mutable handle : handle;
  polled : bool;
  mutable refs : int;
}

let _collections : (string, coll_obs) Hashtbl.t = Hashtbl.create 16
let _sink_id = ref 0

let get_coll_obs q : coll_obs =
  let ck = coll_key q in
  match Hashtbl.find_opt _collections ck with
  | Some o -> o
  | None ->
      let polled = !_live_streams >= !_budget in
      let o = { ck; sinks = Hashtbl.create 16; handle = { stop = (fun () -> ()) }; polled; refs = 0 } in
      (if polled then incr _polled
       else begin
         incr _streams_opened;
         incr _live_streams;
         (* watch the WHOLE collection (no server $match): one stream serves all of this
            collection's query-views, and in-process routing needs every event — including move-outs
            whose post-image no longer matches. *)
         let cs = Change_stream.watch q.coll ~full_document:true ~max_await_ms:200 () in
         let base = spawn_observer cs ~dispatch:(fun ev -> Hashtbl.iter (fun _ s -> s ev) o.sinks) in
         let released = ref false in
         o.handle <- { stop = (fun () -> base.stop (); if not !released then (released := true; decr _live_streams)) }
       end);
      Hashtbl.replace _collections ck o;
      o

let retain o = o.refs <- o.refs + 1
let release o = o.refs <- o.refs - 1; if o.refs <= 0 then (o.handle.stop (); Hashtbl.remove _collections o.ck)

let attach_sink o sink =
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
  cache : (string, Bson.t) Hashtbl.t;
  mutable subs : (int * ucb) list;
  mutable vrefs : int;
  obs : coll_obs;
  mutable detach : unit -> unit;
}

let _views : (string, query_view) Hashtbl.t = Hashtbl.create 64
let _sub_id = ref 0

let route v (ev : Change_stream.event) =
  let id = ev_id ev in
  let fulldoc = Option.value ~default:(Bson.Document []) ev.Change_stream.full_document in
  let fan_added f = List.iter (fun (_, s) -> s.ua id f) v.subs in
  let fan_changed c cl = List.iter (fun (_, s) -> s.uc id c cl) v.subs in
  let fan_removed_id i = List.iter (fun (_, s) -> s.ur i) v.subs in
  match ev.Change_stream.op with
  | Change_stream.Delete -> if Hashtbl.mem v.cache id then (Hashtbl.remove v.cache id; fan_removed_id id)
  | Change_stream.Drop | Change_stream.Drop_database | Change_stream.Invalidate ->
      let ids = Hashtbl.fold (fun k _ acc -> k :: acc) v.cache [] in
      Hashtbl.clear v.cache;
      List.iter fan_removed_id ids
  | Change_stream.Insert | Change_stream.Update | Change_stream.Replace -> (
      let was = Hashtbl.mem v.cache id in
      let now = Q.Matcher.doc_matches v.q.selector fulldoc in
      match Q.Diff.transition ~was ~now with
      | Q.Diff.Entered -> let f = project_full v.proj fulldoc in Hashtbl.replace v.cache id f; fan_added f
      | Q.Diff.Stayed ->
          let nw = project_full v.proj fulldoc in
          let old = Option.value ~default:(Bson.Document []) (Hashtbl.find_opt v.cache id) in
          let chg, cl = Q.Diff.diff_fields ~old_doc:old ~new_doc:nw in
          Hashtbl.replace v.cache id nw;
          if chg <> [] || cl <> [] then fan_changed (Bson.Document chg) cl
      | Q.Diff.Left -> Hashtbl.remove v.cache id; fan_removed_id id
      | Q.Diff.Outside -> ())
  | Change_stream.Rename | Change_stream.Other _ -> ()

let reconcile v =
  let fresh = raw_fetch v.q in
  let seen = Hashtbl.create 64 in
  List.iter
    (fun d ->
      let id = Q.Diff.doc_id d in
      let f = Q.Diff.fields_without_id d in
      Hashtbl.replace seen id ();
      match Hashtbl.find_opt v.cache id with
      | None -> Hashtbl.replace v.cache id f; List.iter (fun (_, s) -> s.ua id f) v.subs
      | Some old ->
          let chg, cl = Q.Diff.diff_fields ~old_doc:old ~new_doc:f in
          if chg <> [] || cl <> [] then (Hashtbl.replace v.cache id f; List.iter (fun (_, s) -> s.uc id (Bson.Document chg) cl) v.subs))
    fresh;
  let gone = Hashtbl.fold (fun id _ acc -> if Hashtbl.mem seen id then acc else id :: acc) v.cache [] in
  List.iter (fun id -> Hashtbl.remove v.cache id; List.iter (fun (_, s) -> s.ur id) v.subs) gone

let get_view q : query_view =
  let vk = coll_key q ^ "\x00" ^ query_key q in
  match Hashtbl.find_opt _views vk with
  | Some v -> v
  | None ->
      let o = get_coll_obs q in
      retain o;
      let v =
        { vk; q; proj = Q.Projection.of_fields q.fields; cache = Hashtbl.create 64; subs = []; vrefs = 0; obs = o; detach = (fun () -> ()) }
      in
      List.iter (fun d -> Hashtbl.replace v.cache (Q.Diff.doc_id d) (Q.Diff.fields_without_id d)) (raw_fetch q);
      (if o.polled then v.detach <- spawn_poller (fun () -> reconcile v) else v.detach <- attach_sink o (fun ev -> route v ev));
      Hashtbl.replace _views vk v;
      v

let observe_changes q ?(added = fun _ _ -> ()) ?(changed = fun _ _ _ -> ()) ?(removed = fun _ -> ()) () : handle =
  let v = get_view q in
  incr _sub_id;
  let sid = !_sub_id in
  v.subs <- (sid, { ua = added; uc = changed; ur = removed }) :: v.subs;
  v.vrefs <- v.vrefs + 1;
  Hashtbl.iter (fun id f -> added id f) v.cache;
  let done_ = ref false in
  {
    stop =
      (fun () ->
        if not !done_ then begin
          done_ := true;
          v.subs <- List.filter (fun (i, _) -> i <> sid) v.subs;
          v.vrefs <- v.vrefs - 1;
          if v.vrefs <= 0 then (v.detach (); Hashtbl.remove _views v.vk; release v.obs)
        end);
  }

(* ordered, positional observe — re-fetch + diff on relevant events *)
let observe q ~added_before ~changed ~moved_before ~removed () : handle =
  let o = get_coll_obs q in
  retain o;
  let prev = ref [] in
  let resync () =
    let cur_list = List.map (fun d -> (Q.Diff.doc_id d, d)) (raw_fetch q) in
    Q.Diff.diff_ordered ~old_list:!prev ~new_list:cur_list ~added_before ~changed ~moved_before ~removed;
    prev := cur_list
  in
  resync ();
  let detach =
    if o.polled then spawn_poller resync
    else
      attach_sink o (fun ev ->
          let fulldoc = Option.value ~default:(Bson.Document []) ev.Change_stream.full_document in
          let id = ev_id ev in
          if Q.Matcher.doc_matches q.selector fulldoc || List.mem_assoc id !prev then resync ())
  in
  let done_ = ref false in
  { stop = (fun () -> if not !done_ then (done_ := true; detach (); release o)) }
