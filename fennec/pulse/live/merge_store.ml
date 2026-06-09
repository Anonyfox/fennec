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
   (bson + minimongo) → native AND JS. *)

module C = Minimongo

type field_entry = { sub : string; prec : int; value : Bson.t }

type doc_view = {
  exists_in : (string, int) Hashtbl.t; (* subId -> precedence *)
  fields : (string, field_entry list) Hashtbl.t; (* field -> precedence list (asc by prec) *)
}

type collection_view = {
  store : C.t;
  docs : (string, doc_view) Hashtbl.t;
  listeners : (int, unit -> unit) Hashtbl.t;
  mutable lc : int;
  mutable version : int;
}

type sub_info = {
  order : int; (* subscription precedence: lower = wins (distinct from field_entry.prec) *)
  contributed : (string, string * string) Hashtbl.t; (* key -> (collection,id) *)
}

type t = {
  collections : (string, collection_view) Hashtbl.t;
  subs : (string, sub_info) Hashtbl.t;
  (* per sub: docs it SEEDED (SSR) that the live snapshot hasn't re-confirmed yet — dropped on the
     sub's first [ready] (quiescence), so a doc deleted between SSR and the socket opening doesn't
     linger as a stale fast-render row *)
  tentative : (string, (string * string, unit) Hashtbl.t) Hashtbl.t;
  mutable seq : int;
}

let create () =
  { collections = Hashtbl.create 8; subs = Hashtbl.create 16; tentative = Hashtbl.create 16; seq = 0 }

(* ---- collections --------------------------------------------------------- *)

let ensure_collection t name : collection_view =
  match Hashtbl.find_opt t.collections name with
  | Some cv -> cv
  | None ->
      let cv =
        { store = C.create (); docs = Hashtbl.create 64; listeners = Hashtbl.create 4; lc = 0; version = 0 }
      in
      Hashtbl.replace t.collections name cv;
      cv

let bump cv =
  cv.version <- cv.version + 1;
  Hashtbl.iter (fun _ f -> f ()) cv.listeners

let store t name = (ensure_collection t name).store
let version t name = match Hashtbl.find_opt t.collections name with Some cv -> cv.version | None -> 0

let on_change t name f =
  let cv = ensure_collection t name in
  cv.lc <- cv.lc + 1;
  let id = cv.lc in
  Hashtbl.replace cv.listeners id f;
  id

let off_change t name id =
  match Hashtbl.find_opt t.collections name with Some cv -> Hashtbl.remove cv.listeners id | None -> ()

(* ---- subscriptions registry ---------------------------------------------- *)

let ensure_sub t sub : sub_info =
  match Hashtbl.find_opt t.subs sub with
  | Some i -> i
  | None ->
      let i = { order = t.seq; contributed = Hashtbl.create 32 } in
      t.seq <- t.seq + 1;
      Hashtbl.replace t.subs sub i;
      i

let sub_prec t sub = match Hashtbl.find_opt t.subs sub with Some i -> i.order | None -> max_int

let ckey collection id = collection ^ "\000" ^ id

let track t sub collection id =
  match Hashtbl.find_opt t.subs sub with
  | Some i -> Hashtbl.replace i.contributed (ckey collection id) (collection, id)
  | None -> ()

let untrack t sub collection id =
  match Hashtbl.find_opt t.subs sub with
  | Some i -> Hashtbl.remove i.contributed (ckey collection id)
  | None -> ()

(* ---- field precedence ---------------------------------------------------- *)

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
  (* iterate over a snapshot of keys since clear_field mutates the table *)
  let fkeys = Hashtbl.fold (fun k _ acc -> k :: acc) dv.fields [] in
  List.iter (fun f -> clear_field dv f sub) fkeys

(* rebuild the winning document into minimongo (or remove it) and notify *)
let recompute cv id dv =
  let by_id = Bson.Document [ ("_id", Bson.String id) ] in
  ignore (C.remove cv.store by_id);
  if Hashtbl.length dv.exists_in = 0 then Hashtbl.remove cv.docs id
  else begin
    let kvs =
      Hashtbl.fold (fun f entries acc -> match entries with e :: _ -> (f, e.value) :: acc | [] -> acc) dv.fields []
    in
    ignore (C.insert cv.store (Bson.Document (("_id", Bson.String id) :: kvs)))
  end;
  bump cv

let doc_of cv id =
  match Hashtbl.find_opt cv.docs id with
  | Some dv -> dv
  | None ->
      let dv = { exists_in = Hashtbl.create 2; fields = Hashtbl.create 8 } in
      Hashtbl.replace cv.docs id dv;
      dv

(* ---- the sub-tagged DDP data ops ----------------------------------------- *)

(* quiescence bookkeeping: [seed] marks a doc tentative for its sub; a live [added]/[changed] confirms
   (clears) it; [quiesce] (on the sub's first [ready]) drops whatever stayed tentative. *)
let mark_tentative t sub collection id =
  let set =
    match Hashtbl.find_opt t.tentative sub with
    | Some s -> s
    | None ->
        let s = Hashtbl.create 16 in
        Hashtbl.replace t.tentative sub s;
        s
  in
  Hashtbl.replace set (collection, id) ()

let confirm t sub collection id =
  match Hashtbl.find_opt t.tentative sub with Some s -> Hashtbl.remove s (collection, id) | None -> ()

(* added: this sub now includes [id] in [collection] with [fields] *)
let added t ~sub ~collection ~id ~fields =
  let _ = ensure_sub t sub in
  let cv = ensure_collection t collection in
  let dv = doc_of cv id in
  let prec = sub_prec t sub in
  Hashtbl.replace dv.exists_in sub prec;
  List.iter (fun (f, v) -> set_field dv f sub prec v) fields;
  track t sub collection id;
  recompute cv id dv;
  confirm t sub collection id

(* changed: this sub updated [fields] / unset [cleared] of an existing doc *)
let changed t ~sub ~collection ~id ~fields ~cleared =
  match Hashtbl.find_opt t.collections collection with
  | None -> ()
  | Some cv -> (
      match Hashtbl.find_opt cv.docs id with
      | None -> ()
      | Some dv ->
          let prec = sub_prec t sub in
          List.iter (fun (f, v) -> set_field dv f sub prec v) fields;
          List.iter (fun f -> clear_field dv f sub) cleared;
          recompute cv id dv;
          confirm t sub collection id)

(* removed: this sub no longer includes [id] *)
let removed t ~sub ~collection ~id =
  match Hashtbl.find_opt t.collections collection with
  | None -> ()
  | Some cv -> (
      match Hashtbl.find_opt cv.docs id with
      | None -> ()
      | Some dv -> drop_sub_from_doc dv sub; untrack t sub collection id; recompute cv id dv)

(* sub_stopped: drop everything this sub contributed (O(that sub's docs)) *)
let sub_stopped t sub =
  match Hashtbl.find_opt t.subs sub with
  | None -> ()
  | Some info ->
      Hashtbl.iter
        (fun _ (collection, id) ->
          match Hashtbl.find_opt t.collections collection with
          | Some cv -> (
              match Hashtbl.find_opt cv.docs id with
              | Some dv -> drop_sub_from_doc dv sub; recompute cv id dv
              | None -> ())
          | None -> ())
        info.contributed;
      Hashtbl.remove t.subs sub;
      Hashtbl.remove t.tentative sub

(* ---- queries ------------------------------------------------------------- *)

let fetch t name ?selector ?sort ?skip ?limit ?fields () : Bson.t array =
  let cv = ensure_collection t name in
  Array.of_list (C.fetch (C.find cv.store ?selector ?sort ?skip ?limit ?fields ()))

(* aggregation over a collection, with $lookup / $unionWith foreign collections resolved across the
   client's OTHER collections — the same multi-collection joins the server does, now on the client *)
let aggregate t name (pipeline : Bson.t list) : Bson.t array =
  let cv = ensure_collection t name in
  let lookup other =
    match Hashtbl.find_opt t.collections other with Some o -> C.fetch (C.find o.store ()) | None -> []
  in
  Array.of_list (C.aggregate cv.store ~lookup pipeline)

(* SSR / hydration seed: install docs into a collection as if from one sub *)
let seed t ~sub ~collection (docs : Bson.t list) =
  List.iter
    (fun d ->
      match d with
      | Bson.Document kvs ->
          let id = match List.assoc_opt "_id" kvs with Some (Bson.String s) -> s | _ -> "" in
          let fields = List.filter (fun (k, _) -> k <> "_id") kvs in
          added t ~sub ~collection ~id ~fields;
          mark_tentative t sub collection id (* SSR seed: tentative until the live snapshot confirms *)
      | _ -> ())
    docs

(* on a sub's first [ready], drop any doc it SEEDED but the live snapshot didn't re-confirm — the
   quiescence pass that keeps SSR fast-render from leaving stale rows behind *)
let quiesce t sub =
  match Hashtbl.find_opt t.tentative sub with
  | None -> ()
  | Some set ->
      Hashtbl.iter (fun (collection, id) () -> removed t ~sub ~collection ~id) set;
      Hashtbl.remove t.tentative sub
