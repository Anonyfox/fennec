(* The Fur binding over the merge store: a reactive [find] whose signal recomputes whenever the
   merged collection changes. Pure over Fur's isomorphic signals → runs native (tests/SSR) and in
   the browser. The DDP WebSocket client + [subscribe] (which feed the store) are a later
   js_of_ocaml addition; here [find] is the read side, driven by whatever feeds the store. *)

type t = {
  store : Merge_store.t;
  vlock : Mutex.t; (* guards [versions] — an SSR-shared client may be read from several domains *)
  versions : (string, int Fur.signal) Hashtbl.t;
}

let create () = { store = Merge_store.create (); vlock = Mutex.create (); versions = Hashtbl.create 8 }
let store t = t.store

(* The recompute SCHEDULER: how a store change reaches the Fur signals. Default = immediate (native,
   SSR, tests — synchronous semantics unchanged). The browser client installs a frame-batched
   scheduler (requestAnimationFrame), so a BURST of deltas — a subscription replay, a reconnect
   resync, a hot feed — costs ONE recompute/re-render per collection per frame instead of one per
   delta. The per-signal [pending] flag dedups within a batch window. *)
let _scheduler : ((unit -> unit) -> unit) ref = ref (fun k -> k ())
let set_scheduler f = _scheduler := f

(* one shared Fur signal per collection, bumped from the merge store's change listener. The lock
   covers only the table find-or-create (signal updates arrive via the store's fire, outside it). *)
let version_signal t name =
  Mutex.lock t.vlock;
  let s =
    match Hashtbl.find_opt t.versions name with
    | Some s -> s
    | None ->
        let s = Fur.signal (Merge_store.version t.store name) in
        let pending = ref false in
        let (_ : int) =
          Merge_store.on_change t.store name (fun () ->
              if not !pending then begin
                pending := true;
                !_scheduler (fun () ->
                    pending := false;
                    Fur.set s (Merge_store.version t.store name))
              end)
        in
        Hashtbl.replace t.versions name s;
        s
  in
  Mutex.unlock t.vlock;
  s

let find t name ?selector ?sort ?skip ?limit ?fields () : Bson.t array Fur.signal =
  let v = version_signal t name in
  let snap () = Merge_store.fetch t.store name ?selector ?sort ?skip ?limit ?fields () in
  let result = Fur.signal [||] in
  (* re-fetch whenever the collection version changes; the watch runs once now to populate, and is
     torn down on the enclosing component's cleanup *)
  let stop = Fur.watch (fun () -> ignore (Fur.get v); Fur.set result (snap ())) in
  Fur.on_cleanup stop;
  result

(* the TYPED reactive read over a collection declaration: the same live signal as [find], decoded
   at the boundary with the skip policy — documents that no longer match the declared shape are
   skipped (foreign garbage, legacy docs), warned ONCE per doc id so dev sees it without the UI
   ever crashing on it. *)
let _warned : (string, unit) Hashtbl.t = Hashtbl.create 8

let find_c t (def : 'a Def.t) ?(where = []) ?sort ?skip ?limit () : 'a array Fur.signal =
  let name = Def.name def in
  let codec = Def.codec def in
  let selector = match Filter.all where with [] -> None | q -> Some (Filter.to_bson q) in
  let sort = Option.map Sort.to_bson sort in
  let v = version_signal t name in
  let snap () =
    Merge_store.fetch t.store name ?selector ?sort ?skip ?limit ()
    |> Array.to_list
    |> List.filter_map (fun d ->
           match Codec.decode codec d with
           | Ok x -> Some x
           | Error es ->
               let key = name ^ ":" ^ (match Bson.get d "_id" with Some (Bson.String s) -> s | _ -> "?") in
               if not (Hashtbl.mem _warned key) then begin
                 Hashtbl.replace _warned key ();
                 prerr_endline ("fennec/typed: skipping malformed doc " ^ key ^ " — " ^ Codec.errors_to_string es)
               end;
               None)
    |> Array.of_list
  in
  let result = Fur.signal [||] in
  let stop = Fur.watch (fun () -> ignore (Fur.get v); Fur.set result (snap ())) in
  Fur.on_cleanup stop;
  result

(* the PROJECTED typed live read: the projection's object type, decoded from the cache slice;
   malformed rows skipped + warned once (same policy as find_c). [name] is the collection. *)
let find_p t (name : string) (p : 'o Proj.t) ?(where = []) ?sort ?skip ?limit () : 'o array Fur.signal =
  let selector = match Filter.all where with [] -> None | q -> Some (Filter.to_bson q) in
  let sort = Option.map Sort.to_bson sort in
  let v = version_signal t name in
  let snap () =
    Merge_store.fetch t.store name ?selector ?sort ?skip ?limit ()
    |> Array.to_list
    |> List.filter_map (fun d ->
           match Proj.decode p d with
           | Ok x -> Some x
           | Error es ->
               let key = name ^ ":" ^ (match Bson.get d "_id" with Some (Bson.String s) -> s | _ -> "?") in
               if not (Hashtbl.mem _warned key) then begin
                 Hashtbl.replace _warned key ();
                 prerr_endline ("fennec/typed: skipping malformed projection " ^ key ^ " — " ^ Codec.errors_to_string es)
               end;
               None)
    |> Array.of_list
  in
  let result = Fur.signal [||] in
  let stop = Fur.watch (fun () -> ignore (Fur.get v); Fur.set result (snap ())) in
  Fur.on_cleanup stop;
  result

(* the foreign collections a pipeline reads via $lookup.from / $unionWith — so [aggregate]'s signal
   recomputes when one of THEM changes too, not just the primary collection (else a join goes stale) *)
let foreign_collections (pipeline : Bson.t list) : string list =
  List.filter_map
    (fun stage ->
      match stage with
      | Bson.Document [ ("$lookup", Bson.Document spec) ] -> (
          match List.assoc_opt "from" spec with Some (Bson.String f) -> Some f | _ -> None)
      | Bson.Document [ ("$unionWith", Bson.String f) ] -> Some f
      | Bson.Document [ ("$unionWith", Bson.Document spec) ] -> (
          match List.assoc_opt "coll" spec with Some (Bson.String f) -> Some f | _ -> None)
      | _ -> None)
    pipeline

let aggregate t name (pipeline : Bson.t list) : Bson.t array Fur.signal =
  let primary = version_signal t name in
  let foreigns = List.map (version_signal t) (foreign_collections pipeline) in
  let snap () = Merge_store.aggregate t.store name pipeline in
  let result = Fur.signal [||] in
  (* recompute when the primary OR any referenced foreign ($lookup/$unionWith) collection changes *)
  let stop =
    Fur.watch (fun () ->
        ignore (Fur.get primary);
        List.iter (fun v -> ignore (Fur.get v)) foreigns;
        Fur.set result (snap ()))
  in
  Fur.on_cleanup stop;
  result
