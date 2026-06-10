(* The reactive Meteor surface as a functor over a storage backend — the ONE surface, instantiated
   on the in-memory minimongo engine ([Mini], pure → also JS) and (later) on a native driver. An
   explicit [REACTIVE] signature (with the collection type abstract) lets one test suite run against
   any backend and prove feature parity.

   Folds in the backend-agnostic features: idGeneration, document transform, cursor options
   (sort/skip/limit/fields), ObjectID, ordered observe, multi-collection publish/subscribe with a
   merge box, allow/deny, methods, and EJSON structural ops. Reactivity is callback-driven (the
   backend's observe_changes); there is no Tracker here — the client supplies its own reactive
   graph (e.g. Fur signals). *)

type doc = Bson.t
type live_handle = { stop : unit -> unit }

(* One field-level change a publication's live query emits — the fennec-internal "Beat" of the Pulse.
   A DDP session lowers each beat onto the wire (Fennec_ddp.Message). Distinct from the
   Meteor-compatible Collection.observe_changes callbacks, which stay added/changed/removed. *)
type beat =
  | Added of { collection : string; id : string; fields : (string * doc) list }
  | Changed of {
      collection : string;
      id : string;
      fields : (string * doc) list;
      cleared : string list;
    }
  | Removed of { collection : string; id : string }

module type REACTIVE = sig
  type backend_collection

  exception Error of { code : string; reason : string }

  type invocation = {
    user_id : string option;
    is_simulation : bool;
    set_user_id : string option -> unit;
  }

  type method_handler = invocation -> doc list -> doc

  val methods : (string * method_handler) list -> unit
  val call : ?user_id:string option -> string -> doc list -> doc

  val apply :
    ?user_id:string option ->
    ?is_simulation:bool ->
    ?set_user_id:(string option -> unit) ->
    string ->
    doc list ->
    doc

  val handle : ('a, 'r) Method.t -> (invocation -> 'a -> 'r) -> unit
  val set_seeded_id_provider : (string -> (int -> int) option) -> unit

  type id_generation = STRING | MONGO

  module ObjectID : sig
    type t = string

    val make : ?hex:string -> unit -> t
    val is_valid : string -> bool
    val to_hex_string : t -> string
    val equals : t -> t -> bool
  end

  module Collection : sig
    type t
    type cursor

    (** a cursor's transform disposition: [Inherit] the collection's, [Disable] it, or [Override f] *)
    type cursor_transform = Inherit | Disable | Override of (doc -> doc)

    val create :
      ?id_generation:id_generation ->
      ?transform:(doc -> doc) ->
      ?name:string ->
      backend_collection ->
      t

    val name : t -> string
    val forget : string -> unit
    val insert : t -> doc -> string

    val find :
      t ->
      ?selector:doc ->
      ?sort:doc ->
      ?skip:int ->
      ?limit:int ->
      ?fields:doc ->
      ?transform:cursor_transform ->
      unit ->
      cursor

    val fetch : cursor -> doc list
    val map : cursor -> (doc -> 'a) -> 'a list
    val for_each : cursor -> (doc -> unit) -> unit
    val find_one : t -> ?selector:doc -> ?sort:doc -> ?fields:doc -> unit -> doc option
    val count : t -> ?selector:doc -> unit -> int
    val aggregate : t -> doc list -> doc list
    val distinct : t -> key:string -> ?selector:doc -> unit -> doc list
    val update : t -> ?multi:bool -> ?upsert:bool -> doc -> doc -> int

    type upsert_result = { number_affected : int; inserted_id : string option }

    val upsert : t -> ?multi:bool -> doc -> doc -> upsert_result
    val remove : t -> doc -> int

    val observe_changes :
      cursor ->
      ?added:(string -> doc -> unit) ->
      ?changed:(string -> doc -> string list -> unit) ->
      ?removed:(string -> unit) ->
      unit ->
      live_handle

    val observe :
      cursor ->
      ?added:(doc -> unit) ->
      ?changed:(doc -> doc -> unit) ->
      ?removed:(doc -> unit) ->
      ?added_at:(doc -> int -> string option -> unit) ->
      ?changed_at:(doc -> doc -> int -> unit) ->
      ?removed_at:(doc -> int -> unit) ->
      ?moved_to:(doc -> int -> int -> string option -> unit) ->
      unit ->
      live_handle

  end

  type cursor_kind = Cursor of Collection.cursor | Cursors of Collection.cursor list

  val cursor :
    Collection.t ->
    ?selector:doc ->
    ?sort:doc ->
    ?skip:int ->
    ?limit:int ->
    ?fields:doc ->
    unit ->
    Collection.cursor

  val publish : string -> (doc list -> cursor_kind) -> unit

  type subscription = {
    documents : unit -> (string * doc) list;
    documents_of : string -> (string * doc) list;
    collections : unit -> string list;
    is_ready : unit -> bool;
    stop : unit -> unit;
  }

  val subscribe : string -> subscription
  val publications : unit -> string list
  val method_names : unit -> string list

  val run_publication : string -> params:doc list -> on:(beat -> unit) -> live_handle

  (** number of active SHARED backend observes (the RX9 multiplexer) — an operational gauge of
      distinct live queries, not subscriptions *)
  val live_query_count : unit -> int

  (** the write fence: run [k] once every delta already committed has been DELIVERED to the live
      subscribers (a method's [updated] rides this) *)
  val fence : (unit -> unit) -> unit

  module EJSON : sig
    val equals : ?key_order_sensitive:bool -> doc -> doc -> bool
    val clone : doc -> doc
  end
end

module Make (B : Backend.S) : REACTIVE with type backend_collection = B.collection = struct
  type backend_collection = B.collection

  exception Error of { code : string; reason : string }

  let error ?(reason = "") code = raise (Error { code; reason })

  (* Lock discipline (multicore — the server runs one Eio domain per core): every module-global
     table in this functor is guarded by a short lock covering ONLY lookups and commits — never a
     user callback, never IO, never another non-leaf lock — so deadlock is impossible by shape.
     Handlers and observers always run OUTSIDE the locks. *)
  let with_lock m f =
    Mutex.lock m;
    match f () with
    | v ->
        Mutex.unlock m;
        v
    | exception e ->
        Mutex.unlock m;
        raise e

  (* guards _methods + _pubs + _collections (registration is cold; lookups are a few ns) *)
  let _reg_lock = Mutex.create ()

  (* ---- methods ---- *)
  type invocation = {
    user_id : string option;
    is_simulation : bool;
    set_user_id : string option -> unit;
        (* rebinds the CONNECTION's user for subsequent calls (a login method's job — Meteor's
           [this.setUserId]); a no-op off-connection (direct [call]/[apply], simulations) *)
  }

  type method_handler = invocation -> doc list -> doc

  let _methods : (string, method_handler) Hashtbl.t = Hashtbl.create 16

  let methods defs =
    with_lock _reg_lock (fun () -> List.iter (fun (n, h) -> Hashtbl.replace _methods n h) defs)

  let apply ?(user_id = None) ?(is_simulation = false) ?(set_user_id = fun _ -> ()) name
      (args : doc list) : doc =
    match with_lock _reg_lock (fun () -> Hashtbl.find_opt _methods name) with
    | None -> error ~reason:(Printf.sprintf "Method '%s' not found" name) "404"
    | Some h -> h { user_id; is_simulation; set_user_id } args (* the handler runs outside the lock *)

  (* the seeded-id provider — server glue (fennec.pulse.server) installs a per-method-call lookup so
     [Collection]'s id minting draws from the client's randomSeed streams (latency compensation: the
     stub and the handler mint the SAME insert ids and the optimistic doc converges with the real
     one). Default: none — normal random ids, one ref deref of cost. *)
  let _seeded_rng : (string -> (int -> int) option) ref = ref (fun _ -> None)
  let set_seeded_id_provider f = _seeded_rng := f

  (* the TYPED method layer: attach a handler to a shared Method.t value. A decode failure becomes a
     400 BEFORE the handler runs — the codec is the validation; the result encodes on the way out. *)
  let handle (m : ('a, 'r) Method.t) (f : invocation -> 'a -> 'r) : unit =
    methods
      [ ( Method.name m,
          fun inv params ->
            match (Method.args m).Codec.dec_args params with
            | Error e -> error "400" ~reason:("invalid arguments: " ^ e)
            | Ok a -> (Method.result m).Codec.enc (f inv a) ) ]

  let call ?(user_id = None) name args = apply ~user_id name args

  (* ---- ObjectID (pure; rng from minimongo's Id) ---- *)
  type id_generation = STRING | MONGO

  module ObjectID = struct
    type t = string

    let is_valid h =
      String.length h = 24
      && String.for_all
           (function '0' .. '9' | 'a' .. 'f' | 'A' .. 'F' -> true | _ -> false)
           h

    let make ?hex () =
      match hex with
      | Some h -> if is_valid h then h else invalid_arg "ObjectID: invalid hex"
      | None -> Query.Id.object_id ()

    let to_hex_string t = t
    let equals a b = String.equal a b
  end

  (* ---- Collection ---- *)
  module Collection = struct
    type t = {
      backend : B.collection;
      uid : int;
          (* unique per collection HANDLE — keys the observe multiplexer (RX9), so two same-named or
             two unnamed handles never collide onto one shared observe *)
      name : string;
      id_generation : id_generation;
      transform : (doc -> doc) option;
    }

    (* a cursor's transform disposition — replaces a triple-state [(doc -> doc) option option] *)
    type cursor_transform = Inherit | Disable | Override of (doc -> doc)

    type cursor = {
      coll : t;
      q : Backend.query;
      cur_transform : cursor_transform;
    }

    (* The named-collection registry, scoped to THIS functor application (one [Reactive.Make (B)] =
       one database): [create ~name] records the backend here and [aggregate] resolves $lookup /
       $unionWith foreign collections from it, so in-memory joins span collections. For two independent
       databases, apply [Make] twice. Unnamed collections are NOT registered — a transient collection
       you don't name neither accumulates here nor becomes a join target; re-using a name is last-wins;
       a dynamically-named collection can be reclaimed with {!forget}. *)
    let _collections : (string, B.collection) Hashtbl.t = Hashtbl.create 16

    (* atomic: [create] may be called concurrently from multiple server domains (dynamic per-tenant
       collections), and [uid] keys the observe multiplexer — a torn/duplicated counter would collide
       two handles onto one shared observe. fetch_and_add is the one safe primitive here. *)
    let _uid_counter = Atomic.make 0

    let create ?(id_generation = STRING) ?transform ?name backend =
      let uid = Atomic.fetch_and_add _uid_counter 1 in
      (* an unnamed (or empty-named) collection gets a UNIQUE synthetic name: it is never registered
         for $lookup, but beats / the DDP wire still need a collection identity — and unique-per-
         handle means two anonymous collections can never collide in a client's cache (the old ""
         sentinel made them all one wire identity). "_anon…" is reserved for this. *)
      let name, registered =
        match name with Some s when s <> "" -> (s, true) | _ -> ("_anon" ^ string_of_int uid, false)
      in
      if registered then with_lock _reg_lock (fun () -> Hashtbl.replace _collections name backend);
      { backend; uid; name; id_generation; transform }

    let name c = c.name

    (* drop a named collection from the registry — for a long-running server that creates dynamically
       named collections (e.g. per-tenant), so they (and their $lookup-resolvability) are reclaimed
       rather than pinned for the process lifetime *)
    let forget nm = with_lock _reg_lock (fun () -> Hashtbl.remove _collections nm)

    let mint_id c (d : doc) : string * doc =
      let kvs = Query.Diff.kvs_of d in
      if List.mem_assoc "_id" kvs then (Query.Diff.doc_id d, d)
      else
        (* ids draw from the ambient seeded stream when a method call carries a randomSeed (latency
           compensation — the client stub mints the same ids), else from the normal RNG *)
        match c.id_generation with
        | STRING ->
            let id = Query.Id.random_id ?rng:(!_seeded_rng c.name) () in
            (id, Bson.Document (("_id", Bson.String id) :: kvs))
        | MONGO ->
            let id = Query.Id.object_id ?rng:(!_seeded_rng c.name) () in
            (id, Bson.Document (("_id", Bson.Object_id id) :: kvs))

    let insert c (d : doc) : string =
      let _id, d = mint_id c d in
      ignore (B.insert c.backend d);
      _id

    let effective_transform (cur : cursor) =
      match cur.cur_transform with Inherit -> cur.coll.transform | Disable -> None | Override f -> Some f

    let apply_tf cur d =
      match effective_transform cur with Some f -> f d | None -> d

    let find c ?(selector = Bson.Document []) ?(sort = Bson.Document [])
        ?(skip = 0) ?(limit = 0) ?(fields = Bson.Document []) ?(transform = Inherit) () : cursor =
      {
        coll = c;
        q = Backend.query ~selector ~sort ~skip ~limit ~fields ();
        cur_transform = transform;
      }

    let raw_fetch (cur : cursor) = B.find cur.coll.backend cur.q
    let fetch cur = List.map (apply_tf cur) (raw_fetch cur)
    let map cur f = List.map f (fetch cur)
    let for_each cur f = List.iter f (fetch cur)

    let find_one c ?(selector = Bson.Document []) ?(sort = Bson.Document [])
        ?(fields = Bson.Document []) () =
      let cur = find c ~selector ~sort ~limit:1 ~fields () in
      match fetch cur with x :: _ -> Some x | [] -> None

    let count c ?(selector = Bson.Document []) () = B.count c.backend selector

    (* one-shot aggregation; rows are computed, so the collection transform is NOT applied. $lookup /
       $unionWith foreign collections resolve (by name) from this instance's registry, so in-memory
       joins span collections — and a native (mongod) backend ignores the resolver and joins itself. *)
    let aggregate c pipeline =
      (* the registry lookup is locked; the foreign B.find runs OUTSIDE it (it takes the foreign
         collection's own lock) — locks never nest, so A→B and B→A joins can't deadlock *)
      let lookup from =
        match with_lock _reg_lock (fun () -> Hashtbl.find_opt _collections from) with
        | Some bk -> B.find bk (Backend.query ())
        | None -> []
      in
      B.aggregate c.backend ~lookup pipeline
    let distinct c ~key ?(selector = Bson.Document []) () = B.distinct c.backend key selector

    let update c ?(multi = false) ?(upsert = false) sel m =
      B.update c.backend ~multi ~upsert sel m

    type upsert_result = { number_affected : int; inserted_id : string option }

    let upsert c ?(multi = false) sel m =
      (* Detect whether the upsert inserted by snapshotting before/after. We can't trust the
         backend's modified-count: a real-Mongo upsert that INSERTS reports nModified=0 (the new
         doc is "upserted", not "modified"), whereas Meteor's numberAffected counts it as 1. *)
      let ids docs = List.map Query.Diff.doc_id docs in
      let before_ids = ids (B.find c.backend (Backend.query ~selector:sel ())) in
      let n = B.update c.backend ~multi ~upsert:true sel m in
      let after_ids = ids (B.find c.backend (Backend.query ~selector:sel ())) in
      let inserted = List.length after_ids > List.length before_ids in
      (* the inserted id is the one now present that was not before — not merely [after]'s head,
         which would be wrong when the selector also matched pre-existing documents *)
      let inserted_id =
        if inserted then List.find_opt (fun id -> not (List.mem id before_ids)) after_ids else None
      in
      { number_affected = (if inserted then 1 else n); inserted_id }

    let remove c sel = B.remove c.backend sel

    let observe_changes (cur : cursor) ?(added = fun _ _ -> ())
        ?(changed = fun _ _ _ -> ()) ?(removed = fun _ -> ()) () : live_handle =
      let h = B.observe_changes cur.coll.backend cur.q ~added ~changed ~removed in
      { stop = h.Backend.stop }

    (* ordered, document-level observe: re-fetch the sorted window on each event and diff (pure).
       The full Meteor callback family — added/addedAt, changed/changedAt, removed/removedAt,
       movedTo — at the in-process API (the DDP wire stays unordered, exactly like Meteor's own
       server: per the DDP spec the ordered messages "are not currently used by Meteor"; clients
       re-sort in minimongo). transform is applied to documents handed to callbacks. *)
    let observe (cur : cursor) ?(added = fun _ -> ()) ?(changed = fun _ _ -> ())
        ?(removed = fun _ -> ()) ?(added_at = fun _ _ _ -> ())
        ?(changed_at = fun _ _ _ -> ()) ?(removed_at = fun _ _ -> ())
        ?(moved_to = fun _ _ _ _ -> ()) () : live_handle =
      let tf d = apply_tf cur d in
      let snap () = List.map (fun d -> (Query.Diff.doc_id d, d)) (raw_fetch cur) in
      let order = ref (List.map fst (snap ())) in
      let prev = ref (snap ()) in
      let index_in lst id =
        let rec go i = function
          | [] -> -1
          | x :: _ when x = id -> i
          | _ :: tl -> go (i + 1) tl
        in
        go 0 lst
      in
      let index_of id = index_in !order id in
      List.iteri (fun i (_, d) -> added (tf d); added_at (tf d) i None) !prev;
      let recompute () =
        let nw = snap () in
        let nw_order = List.map fst nw in
        Query.Diff.diff_ordered ~old_list:!prev ~new_list:nw
          ~added_before:(fun id _ before ->
            (match List.assoc_opt id nw with
             | Some d ->
                 order := nw_order;
                 added d;
                 added_at d (index_of id) before
             | None -> ()))
          ~changed:(fun id _ _ ->
            match (List.assoc_opt id nw, List.assoc_opt id !prev) with
            | Some d, Some o ->
                changed (tf d) (tf o);
                changed_at (tf d) (tf o) (index_in nw_order id)
            | Some d, None ->
                changed (tf d) (tf d);
                changed_at (tf d) (tf d) (index_in nw_order id)
            | _ -> ())
          ~moved_before:(fun id before ->
            (* from = the doc's index in the PRE-move order, to = in the post-move order *)
            let from = index_of id in
            order := nw_order;
            match List.assoc_opt id nw with
            | Some d -> moved_to (tf d) from (index_in nw_order id) before
            | None -> ())
          ~removed:(fun id ->
            match List.assoc_opt id !prev with
            | Some o ->
                let idx = index_of id in
                order := List.filter (fun x -> x <> id) !order;
                removed (tf o);
                removed_at (tf o) idx
            | None -> ());
        prev := nw
      in
      let h =
        B.observe_changes cur.coll.backend cur.q
          ~added:(fun _ _ -> recompute ())
          ~changed:(fun _ _ _ -> recompute ())
          ~removed:(fun _ -> recompute ())
      in
      { stop = h.Backend.stop }

    (* NO allow/deny, deliberately: client writes go through METHODS, full stop — the one blessed
       path. Rule machinery for direct client mutations is the part of Meteor its own community
       regretted; fennec never ships it. *)
  end

  (* ---- publish / subscribe (multi-collection merge box) ---- *)
  type cursor_kind =
    | Cursor of Collection.cursor
    | Cursors of Collection.cursor list

  let cursor coll ?(selector = Bson.Document []) ?(sort = Bson.Document [])
      ?(skip = 0) ?(limit = 0) ?(fields = Bson.Document []) () =
    Collection.find coll ~selector ~sort ~skip ~limit ~fields ()

  let _pubs : (string, doc list -> cursor_kind) Hashtbl.t = Hashtbl.create 16
  let publish name f = with_lock _reg_lock (fun () -> Hashtbl.replace _pubs name f)

  type subscription = {
    documents : unit -> (string * doc) list;
    documents_of : string -> (string * doc) list;
    collections : unit -> string list;
    is_ready : unit -> bool;
    stop : unit -> unit;
  }

  (* reconstruct the [_id] field with the collection's id type — a MONGO collection's _id is an
     Object_id, not a String, so the merge box must not coerce it *)
  let with_id id_gen id fields =
    let idv = match id_gen with MONGO -> Bson.Object_id id | STRING -> Bson.String id in
    Bson.Document (("_id", idv) :: Query.Diff.kvs_of fields)

  let subscribe name : subscription =
    match with_lock _reg_lock (fun () -> Hashtbl.find_opt _pubs name) with
    | None -> failwith (Printf.sprintf "subscribe: no publication %S" name)
    | Some f ->
        (* merge box: collection name -> (id -> doc). A multi-collection publication is fed by
           several observes whose deltas may arrive on different fibers, and the getters read from
           the caller's — one box-local lock covers both (the callbacks do nothing else under it). *)
        let box_lock = Mutex.create () in
        let boxes : (string, (string, doc) Hashtbl.t) Hashtbl.t = Hashtbl.create 8 in
        let box_for coll =
          match Hashtbl.find_opt boxes coll with
          | Some b -> b
          | None -> let b = Hashtbl.create 64 in Hashtbl.replace boxes coll b; b
        in
        let stoppers = ref [] in
        let observe_one (cur : Collection.cursor) =
          let coll = Collection.(cur.coll.name) in
          let id_gen = Collection.(cur.coll.id_generation) in
          let h =
            Collection.observe_changes cur
              ~added:(fun id fields ->
                with_lock box_lock (fun () -> Hashtbl.replace (box_for coll) id (with_id id_gen id fields)))
              ~changed:(fun id fields cleared ->
                with_lock box_lock (fun () ->
                    let b = box_for coll in
                    let base =
                      match Hashtbl.find_opt b id with Some d -> d | None -> Bson.Document []
                    in
                    Hashtbl.replace b id
                      (Query.Diff.merge_doc base ~updated:(Query.Diff.kvs_of fields) ~removed:cleared)))
              ~removed:(fun id -> with_lock box_lock (fun () -> Hashtbl.remove (box_for coll) id))
              ()
          in
          stoppers := h.stop :: !stoppers
        in
        (match f [] with
         | Cursor c -> observe_one c
         | Cursors cs -> List.iter observe_one cs);
        let docs_of coll =
          with_lock box_lock (fun () ->
              match Hashtbl.find_opt boxes coll with
              | Some b -> Hashtbl.fold (fun k v acc -> (k, v) :: acc) b []
              | None -> [])
        in
        {
          documents =
            (fun () ->
              with_lock box_lock (fun () ->
                  Hashtbl.fold
                    (fun _ b acc -> Hashtbl.fold (fun k v a -> (k, v) :: a) b acc)
                    boxes []));
          documents_of = docs_of;
          collections =
            (fun () -> with_lock box_lock (fun () -> Hashtbl.fold (fun k _ acc -> k :: acc) boxes []));
          is_ready = (fun () -> true);
          stop = (fun () -> List.iter (fun s -> s ()) !stoppers);
        }

  let publications () = with_lock _reg_lock (fun () -> Hashtbl.fold (fun k _ acc -> k :: acc) _pubs [])
  let method_names () = with_lock _reg_lock (fun () -> Hashtbl.fold (fun k _ acc -> k :: acc) _methods [])

  (* ---- the observe multiplexer (RX9) ---------------------------------------
     ONE backend observe per (collection-handle, query), shared across every subscription with that
     exact query: N sessions watching the same feed cost one observe + one selector eval per mutation
     (fanned to all N), not N. Keyed by (uid, canonical query) — a typed tuple, so same-named /
     anonymous handles can never collide and no separator convention is needed. Refcounted — the
     last subscriber out stops the observe and drops the entry.

     CONCURRENCY: the sinks ride a {!Fanout} — beats enqueue under [mlock] atomically with the
     [live]-table update (so a late joiner's snapshot is exactly the delivered prefix) and deliver
     outside all locks, in order, buffered for joiners mid-replay. Lock order is strictly
     _mux_lock → mlock → (fanout's own); the backend observe is built and stopped outside both. *)
  module Fanout = Minimongo.Fanout

  type mux = {
    fan : beat Fanout.t; (* the subscriber sinks *)
    mlock : Mutex.t; (* guards [live] + [handle], atomic with the fan enqueue *)
    live : (string, (string, doc) Hashtbl.t) Hashtbl.t; (* id -> field table (late-joiner replay) *)
    mutable handle : live_handle option; (* None while the backend observe is still being built *)
    collection : string;
    backend_fence : (unit -> unit) -> unit; (* fences the BACKEND delivery hop feeding this mux *)
  }

  let _mux_lock = Mutex.create ()
  let _muxes : (int * string, mux) Hashtbl.t = Hashtbl.create 64

  (* active shared observes — an operational gauge of how effectively the multiplexer is collapsing
     subscriptions (one entry per distinct (collection-handle, query), regardless of subscriber count) *)
  let live_query_count () = with_lock _mux_lock (fun () -> Hashtbl.length _muxes)

  (* The WRITE FENCE: run [k] once every delta already committed has been DELIVERED to the live
     subscribers — so a method's [updated] (the client's cue to drop its optimistic simulation) can
     never overtake the method's own data deltas. Deltas cross two fan-out hops (the backend change
     stream → the mux handlers → the mux fan → the sinks), so the fence drains them IN ORDER: phase 1
     waits on every live query's backend hop (whose drain pumps into the mux fans), phase 2 on the
     mux fans themselves. Exact for the in-memory backend; best-effort over mongod (its fence is
     immediate — see fennec_pulse_mongo). With no live queries there is nothing in flight: immediate. *)
  let fence (k : unit -> unit) : unit =
    let muxes = with_lock _mux_lock (fun () -> Hashtbl.fold (fun _ m acc -> m :: acc) _muxes []) in
    match muxes with
    | [] -> k ()
    | _ ->
        let n = List.length muxes in
        let pending2 = Atomic.make n in
        let phase2 () =
          List.iter
            (fun m ->
              Fanout.on_drained m.fan (fun () -> if Atomic.fetch_and_add pending2 (-1) = 1 then k ()))
            muxes
        in
        let pending1 = Atomic.make n in
        List.iter
          (fun m -> m.backend_fence (fun () -> if Atomic.fetch_and_add pending1 (-1) = 1 then phase2 ()))
          muxes

  (* subscribe [on] to the shared observe for [cur]; returns an idempotent unsubscribe that
     refcounts the mux *)
  let mux_subscribe (cur : Collection.cursor) ~on : unit -> unit =
    let coll = Collection.(cur.coll.name) in
    let key = (Collection.(cur.coll.uid), Query_key.of_query ~collection:coll cur.q) in
    (* find-or-create AND sink registration run under the table lock, so a concurrent teardown's
       count-check-and-remove can never interleave with a joiner *)
    let mux, fresh, sub, replay =
      with_lock _mux_lock (fun () ->
          match Hashtbl.find_opt _muxes key with
          | Some mux ->
              (* late joiner: snapshot [live] atomically with a BUFFERING registration — beats landing
                 after this instant queue up and flush, in order, after the snapshot replay *)
              with_lock mux.mlock (fun () ->
                  let sub = Fanout.subscribe mux.fan ~ready:false on in
                  let replay =
                    Hashtbl.fold
                      (fun id tbl acc -> (id, Hashtbl.fold (fun k v a -> (k, v) :: a) tbl []) :: acc)
                      mux.live []
                  in
                  (mux, false, sub, replay))
          | None ->
              let backend = Collection.(cur.coll.backend) in
              let mux =
                { fan = Fanout.create (); mlock = Mutex.create (); live = Hashtbl.create 64;
                  handle = None; collection = coll; backend_fence = (fun k -> B.fence backend k) }
              in
              Hashtbl.replace _muxes key mux;
              (* the first subscriber is live from the start: the only producer into this fan is the
                 backend observe built below, so nothing can be missed or reordered *)
              (mux, true, Fanout.subscribe mux.fan ~ready:true on, []))
    in
    (* drop this mux's table entry iff it still owns it (a teardown+rebuild may have replaced it) *)
    let evict_if_owner () =
      match Hashtbl.find_opt _muxes key with
      | Some m when m == mux -> Hashtbl.remove _muxes key
      | _ -> ()
    in
    (if fresh then begin
       (* build the shared backend observe OUTSIDE all locks. Its synchronous replay flows through
          [beat] into the fan and reaches this first subscriber before this returns (RX7:
          ready-after-return). A same-key subscriber arriving mid-replay is buffering: it gets the
          prefix from its [live] snapshot and the rest from its buffer — complete either way. *)
       let beat update mk =
         with_lock mux.mlock (fun () ->
             update ();
             Fanout.enqueue mux.fan mk);
         Fanout.pump mux.fan
       in
       let set_fields tbl fs = List.iter (fun (k, v) -> Hashtbl.replace tbl k v) fs in
       let h =
         Collection.observe_changes cur
           ~added:(fun id fields ->
             let fs = Query.Diff.kvs_of fields in
             beat
               (fun () ->
                 let tbl = Hashtbl.create 8 in
                 set_fields tbl fs;
                 Hashtbl.replace mux.live id tbl)
               (Added { collection = coll; id; fields = fs }))
           ~changed:(fun id fields cleared ->
             let fs = Query.Diff.kvs_of fields in
             beat
               (fun () ->
                 match Hashtbl.find_opt mux.live id with
                 | Some tbl ->
                     List.iter (Hashtbl.remove tbl) cleared;
                     set_fields tbl fs
                 | None ->
                     let tbl = Hashtbl.create 8 in
                     set_fields tbl fs;
                     Hashtbl.replace mux.live id tbl)
               (Changed { collection = coll; id; fields = fs; cleared }))
           ~removed:(fun id ->
             beat (fun () -> Hashtbl.remove mux.live id) (Removed { collection = coll; id }))
           ()
       in
       (* publish the handle; if every subscriber left while we were building, tear down now *)
       let orphaned =
         with_lock _mux_lock (fun () ->
             with_lock mux.mlock (fun () -> mux.handle <- Some h);
             if Fanout.count mux.fan = 0 then (evict_if_owner (); true) else false)
       in
       if orphaned then h.stop ()
     end
     else begin
       (* deliver the snapshot OUTSIDE the locks, then flush whatever buffered meanwhile. A beat the
          snapshot already covers re-delivers harmlessly (the client merge box is idempotent). *)
       List.iter (fun (id, fields) -> on (Added { collection = mux.collection; id; fields })) replay;
       Fanout.ready mux.fan sub
     end);
    (* idempotent: a stale double-stop can't tear down (or evict) a mux it no longer belongs to *)
    let stopped = ref false in
    fun () ->
      if not !stopped then begin
        stopped := true;
        let to_stop =
          with_lock _mux_lock (fun () ->
              Fanout.unsubscribe mux.fan sub;
              if Fanout.count mux.fan = 0 then begin
                evict_if_owner ();
                with_lock mux.mlock (fun () -> mux.handle)
              end
              else None)
        in
        (* the backend observe is stopped OUTSIDE the mux locks (it takes the collection's own);
           when [handle] is still None the builder's orphan check (above) stops it instead *)
        match to_stop with Some h -> h.stop () | None -> ()
      end

  (* Run a publication's cursors with field-level observe deltas delivered as beats (the [collection]
     is per-doc), each backed by a SHARED observe (the multiplexer above). The delta-driven entry a
     DDP session uses — no merge box; the caller emits [ready] after this returns (the shared observe
     replays existing docs synchronously — as a fresh observe's [added]s, or a late joiner's [live]
     replay — during this call). *)
  let run_publication name ~params ~on : live_handle =
    match Hashtbl.find_opt _pubs name with
    | None -> { stop = (fun () -> ()) }
    | Some f ->
        let stoppers = ref [] in
        let observe_one cur = stoppers := mux_subscribe cur ~on :: !stoppers in
        (match f params with Cursor c -> observe_one c | Cursors cs -> List.iter observe_one cs);
        { stop = (fun () -> List.iter (fun s -> s ()) !stoppers) }

  (* ---- EJSON structural ops (pure) ---- *)
  module EJSON = struct
    let rec clone : doc -> doc = function
      | Bson.Document kvs -> Bson.Document (List.map (fun (k, v) -> (k, clone v)) kvs)
      | Bson.Array xs -> Bson.Array (List.map clone xs)
      | x -> x

    let rec equals ?(key_order_sensitive = false) (a : doc) (b : doc) : bool =
      match (a, b) with
      | Bson.Document x, Bson.Document y ->
          if key_order_sensitive then
            List.length x = List.length y
            && List.for_all2
                 (fun (k1, v1) (k2, v2) -> k1 = k2 && equals ~key_order_sensitive v1 v2)
                 x y
          else
            List.length x = List.length y
            && List.for_all
                 (fun (k, v) ->
                   match List.assoc_opt k y with
                   | Some v2 -> equals ~key_order_sensitive v v2
                   | None -> false)
                 x
      | Bson.Array xs, Bson.Array ys ->
          List.length xs = List.length ys
          && List.for_all2 (equals ~key_order_sensitive) xs ys
      | _ -> a = b
  end
end

(* the in-memory instance — pure, also compiles to JavaScript *)
module Mini = Make (Backend.Mini)
