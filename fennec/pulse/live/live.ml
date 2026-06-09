(* The Fur binding over the merge store: a reactive [find] whose signal recomputes whenever the
   merged collection changes. Pure over Fur's isomorphic signals → runs native (tests/SSR) and in
   the browser. The DDP WebSocket client + [subscribe] (which feed the store) are a later
   js_of_ocaml addition; here [find] is the read side, driven by whatever feeds the store. *)

type t = { store : Merge_store.t; versions : (string, int Fur.signal) Hashtbl.t }

let create () = { store = Merge_store.create (); versions = Hashtbl.create 8 }
let store t = t.store

(* one shared Fur signal per collection, bumped from the merge store's change listener *)
let version_signal t name =
  match Hashtbl.find_opt t.versions name with
  | Some s -> s
  | None ->
      let s = Fur.signal (Merge_store.version t.store name) in
      let (_ : int) =
        Merge_store.on_change t.store name (fun () -> Fur.set s (Merge_store.version t.store name))
      in
      Hashtbl.replace t.versions name s;
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
