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

  type invocation = { user_id : string option; is_simulation : bool }
  type method_handler = invocation -> doc list -> doc

  val methods : (string * method_handler) list -> unit
  val call : ?user_id:string option -> string -> doc list -> doc
  val apply : ?user_id:string option -> ?is_simulation:bool -> string -> doc list -> doc

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
      ?removed_at:(doc -> int -> unit) ->
      unit ->
      live_handle

    val allow :
      t ->
      ?insert:(string option -> doc -> bool) ->
      ?update:(string option -> doc -> bool) ->
      ?remove:(string option -> doc -> bool) ->
      unit ->
      unit

    val deny :
      t ->
      ?insert:(string option -> doc -> bool) ->
      ?update:(string option -> doc -> bool) ->
      ?remove:(string option -> doc -> bool) ->
      unit ->
      unit

    val insert_from_client : ?user_id:string option -> t -> doc -> string
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

  module EJSON : sig
    val equals : ?key_order_sensitive:bool -> doc -> doc -> bool
    val clone : doc -> doc
  end
end

module Make (B : Backend.S) : REACTIVE with type backend_collection = B.collection = struct
  type backend_collection = B.collection

  exception Error of { code : string; reason : string }

  let error ?(reason = "") code = raise (Error { code; reason })

  (* ---- methods ---- *)
  type invocation = { user_id : string option; is_simulation : bool }
  type method_handler = invocation -> doc list -> doc

  let _methods : (string, method_handler) Hashtbl.t = Hashtbl.create 16
  let methods defs = List.iter (fun (n, h) -> Hashtbl.replace _methods n h) defs

  let apply ?(user_id = None) ?(is_simulation = false) name (args : doc list) : doc =
    match Hashtbl.find_opt _methods name with
    | None -> error ~reason:(Printf.sprintf "Method '%s' not found" name) "404"
    | Some h -> h { user_id; is_simulation } args

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
    type rule = string option -> doc -> bool

    type rules = {
      mutable allow_insert : rule list;
      mutable allow_update : rule list;
      mutable allow_remove : rule list;
      mutable deny_insert : rule list;
      mutable deny_update : rule list;
      mutable deny_remove : rule list;
      mutable secured : bool;
    }

    type t = {
      backend : B.collection;
      uid : int;
          (* unique per collection HANDLE — keys the observe multiplexer (RX9), so two same-named or
             two unnamed handles never collide onto one shared observe *)
      name : string;
      id_generation : id_generation;
      transform : (doc -> doc) option;
      rules : rules;
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

    let create ?(id_generation = STRING) ?transform ?(name = "") backend =
      if name <> "" then Hashtbl.replace _collections name backend;
      {
        backend;
        uid = Atomic.fetch_and_add _uid_counter 1;
        name;
        id_generation;
        transform;
        rules =
          {
            allow_insert = []; allow_update = []; allow_remove = [];
            deny_insert = []; deny_remove = []; deny_update = [];
            secured = false;
          };
      }

    let name c = c.name

    (* drop a named collection from the registry — for a long-running server that creates dynamically
       named collections (e.g. per-tenant), so they (and their $lookup-resolvability) are reclaimed
       rather than pinned for the process lifetime *)
    let forget nm = Hashtbl.remove _collections nm

    let mint_id c (d : doc) : string * doc =
      let kvs = Query.Diff.kvs_of d in
      if List.mem_assoc "_id" kvs then (Query.Diff.doc_id d, d)
      else
        match c.id_generation with
        | STRING ->
            let id = Query.Id.random_id () in
            (id, Bson.Document (("_id", Bson.String id) :: kvs))
        | MONGO ->
            let id = Query.Id.object_id () in
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
      let lookup from =
        match Hashtbl.find_opt _collections from with Some bk -> B.find bk (Backend.query ()) | None -> []
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
       transform is applied to documents handed to callbacks. *)
    let observe (cur : cursor) ?(added = fun _ -> ()) ?(changed = fun _ _ -> ())
        ?(removed = fun _ -> ()) ?(added_at = fun _ _ _ -> ())
        ?(removed_at = fun _ _ -> ()) () : live_handle =
      let tf d = apply_tf cur d in
      let snap () = List.map (fun d -> (Query.Diff.doc_id d, d)) (raw_fetch cur) in
      let order = ref (List.map fst (snap ())) in
      let prev = ref (snap ()) in
      let index_of id =
        let rec go i = function
          | [] -> -1
          | x :: _ when x = id -> i
          | _ :: tl -> go (i + 1) tl
        in
        go 0 !order
      in
      List.iteri (fun i (_, d) -> added (tf d); added_at (tf d) i None) !prev;
      let recompute () =
        let nw = snap () in
        Query.Diff.diff_ordered ~old_list:!prev ~new_list:nw
          ~added_before:(fun id _ before ->
            (match List.assoc_opt id nw with
             | Some d ->
                 order := List.map fst nw;
                 added d;
                 added_at d (index_of id) before
             | None -> ()))
          ~changed:(fun id _ _ ->
            match (List.assoc_opt id nw, List.assoc_opt id !prev) with
            | Some d, Some o -> changed (tf d) (tf o)
            | Some d, None -> changed (tf d) (tf d)
            | _ -> ())
          ~moved_before:(fun _ _ -> order := List.map fst nw)
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

    (* allow / deny *)
    let allow c ?(insert = fun _ _ -> false) ?(update = fun _ _ -> false)
        ?(remove = fun _ _ -> false) () =
      c.rules.secured <- true;
      c.rules.allow_insert <- insert :: c.rules.allow_insert;
      c.rules.allow_update <- update :: c.rules.allow_update;
      c.rules.allow_remove <- remove :: c.rules.allow_remove

    let deny c ?(insert = fun _ _ -> false) ?(update = fun _ _ -> false)
        ?(remove = fun _ _ -> false) () =
      c.rules.secured <- true;
      c.rules.deny_insert <- insert :: c.rules.deny_insert;
      c.rules.deny_update <- update :: c.rules.deny_update;
      c.rules.deny_remove <- remove :: c.rules.deny_remove

    let passes allow deny uid d =
      (not (List.exists (fun f -> f uid d) deny))
      && List.exists (fun f -> f uid d) allow

    let insert_from_client ?(user_id = None) c d =
      if not c.rules.secured then
        error "403" ~reason:"Access denied. No allow validators set on collection."
      else if passes c.rules.allow_insert c.rules.deny_insert user_id d then insert c d
      else error "403" ~reason:"Access denied"
  end

  (* ---- publish / subscribe (multi-collection merge box) ---- *)
  type cursor_kind =
    | Cursor of Collection.cursor
    | Cursors of Collection.cursor list

  let cursor coll ?(selector = Bson.Document []) ?(sort = Bson.Document [])
      ?(skip = 0) ?(limit = 0) ?(fields = Bson.Document []) () =
    Collection.find coll ~selector ~sort ~skip ~limit ~fields ()

  let _pubs : (string, doc list -> cursor_kind) Hashtbl.t = Hashtbl.create 16
  let publish name f = Hashtbl.replace _pubs name f

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
    match Hashtbl.find_opt _pubs name with
    | None -> failwith (Printf.sprintf "subscribe: no publication %S" name)
    | Some f ->
        (* merge box: collection name -> (id -> doc) *)
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
              ~added:(fun id fields -> Hashtbl.replace (box_for coll) id (with_id id_gen id fields))
              ~changed:(fun id fields cleared ->
                let b = box_for coll in
                let base =
                  match Hashtbl.find_opt b id with Some d -> d | None -> Bson.Document []
                in
                Hashtbl.replace b id
                  (Query.Diff.merge_doc base ~updated:(Query.Diff.kvs_of fields) ~removed:cleared))
              ~removed:(fun id -> Hashtbl.remove (box_for coll) id)
              ()
          in
          stoppers := h.stop :: !stoppers
        in
        (match f [] with
         | Cursor c -> observe_one c
         | Cursors cs -> List.iter observe_one cs);
        let docs_of coll =
          match Hashtbl.find_opt boxes coll with
          | Some b -> Hashtbl.fold (fun k v acc -> (k, v) :: acc) b []
          | None -> []
        in
        {
          documents =
            (fun () ->
              Hashtbl.fold
                (fun _ b acc -> Hashtbl.fold (fun k v a -> (k, v) :: a) b acc)
                boxes []);
          documents_of = docs_of;
          collections = (fun () -> Hashtbl.fold (fun k _ acc -> k :: acc) boxes []);
          is_ready = (fun () -> true);
          stop = (fun () -> List.iter (fun s -> s ()) !stoppers);
        }

  let publications () = Hashtbl.fold (fun k _ acc -> k :: acc) _pubs []
  let method_names () = Hashtbl.fold (fun k _ acc -> k :: acc) _methods []

  (* ---- the observe multiplexer (RX9) ---------------------------------------
     ONE backend observe per (collection-handle, query), shared across every subscription with that
     exact query: N sessions watching the same feed cost one observe + one selector eval per mutation
     (fanned to all N), not N. Keyed by the collection's uid (so same-named / unnamed handles never
     collide) + the canonical query key. Refcounted — the last subscriber out stops the observe and
     drops the entry. Single Eio domain: a subscribe replays current state then registers, with no
     mutation interleaving in between, so a late joiner can't miss or double-apply a delta. *)
  type mux = {
    mutable handle : live_handle;
    subs : (int, beat -> unit) Hashtbl.t;
    live : (string, (string * doc) list) Hashtbl.t; (* id -> current field set, for late-joiner replay *)
    collection : string;
    mutable subc : int;
  }

  let _muxes : (string, mux) Hashtbl.t = Hashtbl.create 64

  (* active shared observes — an operational gauge of how effectively the multiplexer is collapsing
     subscriptions (one entry per distinct (collection-handle, query), regardless of subscriber count) *)
  let live_query_count () = Hashtbl.length _muxes

  (* merge a [changed] delta into a doc's stored field set (drop [cleared], overwrite/add [fields]) *)
  let _mux_merge fields_now fields cleared =
    let kept = List.filter (fun (k, _) -> not (List.mem k cleared)) fields_now in
    List.fold_left (fun acc (k, v) -> (k, v) :: List.filter (fun (k', _) -> k' <> k) acc) kept fields

  (* subscribe [on] to the shared observe for [cur]; returns an unsubscribe that refcounts the mux *)
  let mux_subscribe (cur : Collection.cursor) ~on : unit -> unit =
    let coll = Collection.(cur.coll.name) in
    let key = string_of_int Collection.(cur.coll.uid) ^ "\x00" ^ Query_key.of_query ~collection:coll cur.q in
    let mux, fresh =
      match Hashtbl.find_opt _muxes key with
      | Some mux -> (mux, false)
      | None ->
          let mux =
            { handle = { stop = (fun () -> ()) }; subs = Hashtbl.create 8; live = Hashtbl.create 64;
              collection = coll; subc = 0 }
          in
          Hashtbl.replace _muxes key mux;
          (mux, true)
    in
    let sid = mux.subc in
    mux.subc <- sid + 1;
    (if fresh then begin
       (* register BEFORE observing so the synchronous initial replay's [added]s reach [on] (and fill
          [live]) before this returns — the caller's ready-after-return contract (RX7) depends on it *)
       Hashtbl.replace mux.subs sid on;
       let fan b = Hashtbl.iter (fun _ f -> f b) mux.subs in
       let h =
         Collection.observe_changes cur
           ~added:(fun id fields ->
             let fs = Query.Diff.kvs_of fields in
             Hashtbl.replace mux.live id fs;
             fan (Added { collection = coll; id; fields = fs }))
           ~changed:(fun id fields cleared ->
             let fs = Query.Diff.kvs_of fields in
             Hashtbl.replace mux.live id (_mux_merge (try Hashtbl.find mux.live id with Not_found -> []) fs cleared);
             fan (Changed { collection = coll; id; fields = fs; cleared }))
           ~removed:(fun id ->
             Hashtbl.remove mux.live id;
             fan (Removed { collection = coll; id }))
           ()
       in
       mux.handle <- h
     end
     else begin
       (* late joiner: replay the current merged state to THIS sub only, then ride the shared stream *)
       Hashtbl.iter (fun id fields -> on (Added { collection = mux.collection; id; fields })) mux.live;
       Hashtbl.replace mux.subs sid on
     end);
    (* idempotent: a double-stop must NOT re-run teardown — otherwise the second call could see
       [subs] emptied by a co-subscriber leaving in between and stop the shared observe (or drop the
       _muxes entry) while a live subscriber still rides it *)
    let stopped = ref false in
    fun () ->
      if not !stopped then begin
        stopped := true;
        Hashtbl.remove mux.subs sid;
        if Hashtbl.length mux.subs = 0 then begin
          mux.handle.stop ();
          Hashtbl.remove _muxes key
        end
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
