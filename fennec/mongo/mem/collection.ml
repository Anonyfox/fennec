(* In-memory collection: the _id-keyed store, mutations, cursors, and the reactive observe engine.
   Mutations synchronously emit change events (the "simulated change stream"); observe/
   observeChanges recompute reactively off those events using the pure matcher/diff/transition
   core. No Eio, no polling, no systhreads — a mutation IS the event. *)

open Bson

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
  store : (string, doc) Hashtbl.t; (* _id -> full document *)
  mutable order : string list; (* insertion order, stable iteration *)
  mutable observers : (int * observer) list;
  obs_counter : int ref;
  gen_id : unit -> string;
}

let create ?(gen_id = fun () -> Query.Id.random_id ()) () =
  {
    store = Hashtbl.create 64;
    order = [];
    observers = [];
    obs_counter = ref 0;
    gen_id;
  }

type handle = { stop : unit -> unit }

(* raw simulated change stream: subscribe to insert/update/remove events *)
let watch t (f : observer) : handle =
  incr t.obs_counter;
  let i = !(t.obs_counter) in
  t.observers <- (i, f) :: t.observers;
  { stop = (fun () -> t.observers <- List.filter (fun (j, _) -> j <> i) t.observers) }

let notify t change = List.iter (fun (_, obs) -> obs change) (List.rev t.observers)

(* ---- mutations ---- *)

let ensure_id t (d : doc) : string * doc =
  match get d "_id" with
  | Some v -> (Query.Diff.id_to_string v, d)
  | None ->
      let id = t.gen_id () in
      (id, Document (("_id", String id) :: Query.Diff.kvs_of d))

let insert t (d : doc) : string =
  let id, d = ensure_id t d in
  if not (Hashtbl.mem t.store id) then t.order <- t.order @ [ id ];
  Hashtbl.replace t.store id d;
  notify t { op = Insert; id; new_doc = Some d; old_doc = None };
  id

let matching t selector =
  List.filter (fun id -> Query.Matcher.doc_matches selector (Hashtbl.find t.store id)) t.order

(* update by selector; returns number affected. *)
let update t ?(multi = false) ?(upsert = false) (selector : doc) (modifier : doc) :
    int =
  let ids = matching t selector in
  let ids = if multi then ids else match ids with x :: _ -> [ x ] | [] -> [] in
  match ids with
  | [] when upsert ->
      (* seed an insert from the selector's plain equality fields + modifier *)
      let base =
        Document
          (List.filter_map
             (fun (k, v) ->
               if String.length k > 0 && k.[0] = '$' then None
               else match v with Document _ -> None | _ -> Some (k, v))
             (Query.Diff.kvs_of selector))
      in
      ignore (insert t (Query.Modifier.apply ~insert:true base modifier));
      1
  | [] -> 0
  | _ ->
      List.iter
        (fun id ->
          let old = Hashtbl.find t.store id in
          let nw = Query.Modifier.apply old modifier in
          let nw =
            match (get nw "_id", get old "_id") with
            | None, Some idv -> Document (("_id", idv) :: Query.Diff.kvs_of nw)
            | _ -> nw
          in
          Hashtbl.replace t.store id nw;
          notify t { op = Update; id; new_doc = Some nw; old_doc = Some old })
        ids;
      List.length ids

let remove t (selector : doc) : int =
  let ids = matching t selector in
  List.iter
    (fun id ->
      let old = Hashtbl.find t.store id in
      Hashtbl.remove t.store id;
      t.order <- List.filter (fun x -> x <> id) t.order;
      notify t { op = Remove; id; new_doc = None; old_doc = Some old })
    ids;
  List.length ids

(* ---- cursors / queries ---- *)

type cursor = {
  coll : t;
  selector : doc;
  sort : doc;
  skip : int;
  limit : int; (* 0 = unbounded *)
  fields : doc; (* projection spec *)
}

let find t ?(selector = Document []) ?(sort = Document []) ?(skip = 0)
    ?(limit = 0) ?(fields = Document []) () =
  { coll = t; selector; sort; skip; limit; fields }

let all_docs t = List.map (fun id -> Hashtbl.find t.store id) t.order
let matched cur = List.filter (Query.Matcher.doc_matches cur.selector) (all_docs cur.coll)
let ordered cur = Query.Sorter.sort cur.sort (matched cur)

let windowed cur =
  let xs = ordered cur in
  let xs =
    if cur.skip > 0 then
      let rec drop n = function
        | [] -> []
        | _ :: tl when n > 0 -> drop (n - 1) tl
        | l -> l
      in
      drop cur.skip xs
    else xs
  in
  if cur.limit > 0 then
    let rec take n = function
      | x :: tl when n > 0 -> x :: take (n - 1) tl
      | _ -> []
    in
    take cur.limit xs
  else xs

let projection cur = Query.Projection.of_fields cur.fields
let fetch cur = List.map (Query.Projection.apply (projection cur)) (windowed cur)
let count cur = List.length (matched cur)
let for_each cur f = List.iter f (fetch cur)
let map cur f = List.map f (fetch cur)
let find_one cur = match fetch cur with x :: _ -> Some x | [] -> None

(* observeChanges — field-level, unordered membership routing. Honors selector +
   projection on live deltas; skip/limit affect only the initial snapshot. *)
let observe_changes cur ?(added = fun _ _ -> ()) ?(changed = fun _ _ _ -> ())
    ?(removed = fun _ -> ()) () : handle =
  let p = projection cur in
  let cache : (string, doc) Hashtbl.t = Hashtbl.create 64 in
  List.iter
    (fun d ->
      let id = Query.Diff.doc_id d in
      let f = Query.Projection.apply p (Query.Diff.fields_without_id d) in
      Hashtbl.replace cache id f;
      added id f)
    (windowed cur);
  let route (ch : change) =
    let id = ch.id in
    match ch.op with
    | Remove -> if Hashtbl.mem cache id then (Hashtbl.remove cache id; removed id)
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
            let old =
              match Hashtbl.find_opt cache id with Some o -> o | None -> Document []
            in
            let chg, cleared = Query.Diff.diff_fields ~old_doc:old ~new_doc:nw in
            Hashtbl.replace cache id nw;
            if chg <> [] || cleared <> [] then changed id (Document chg) cleared
        | Left ->
            Hashtbl.remove cache id;
            removed id
        | Outside -> ())
  in
  watch cur.coll route

(* observe — document-level. Recomputes the ordered window and diffs, so
   sort/skip/limit are honored and callbacks receive full documents. *)
let observe cur ?(added = fun _ -> ()) ?(changed = fun _ _ -> ())
    ?(removed = fun _ -> ()) () : handle =
  let snap () = List.map (fun d -> (Query.Diff.doc_id d, d)) (windowed cur) in
  let prev = ref (snap ()) in
  List.iter (fun (_, d) -> added d) !prev;
  let recompute (_ : change) =
    let nw = snap () in
    Query.Diff.diff_ordered ~old_list:!prev ~new_list:nw
      ~added_before:(fun id _ _ ->
        match List.assoc_opt id nw with Some d -> added d | None -> ())
      ~changed:(fun id _ _ ->
        match (List.assoc_opt id nw, List.assoc_opt id !prev) with
        | Some d, Some o -> changed d o
        | Some d, None -> changed d d
        | _ -> ())
      ~moved_before:(fun _ _ -> ())
      ~removed:(fun id ->
        match List.assoc_opt id !prev with Some o -> removed o | None -> ());
    prev := nw
  in
  watch cur.coll recompute
