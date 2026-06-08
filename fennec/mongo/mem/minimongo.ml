(* In-memory MongoDB (Minimongo) — the _id-keyed store, mutations, cursors, and the reactive
   observe engine. Mutations synchronously emit change events (the "simulated change stream");
   observe/observeChanges recompute reactively off those events using the pure matcher/diff/
   transition core. No Eio, no polling, no systhreads — a mutation IS the event.

   Insertion order is kept as a REVERSED id list ([rorder], newest first) so insert is O(1); reads
   reverse it. All store lookups are total ([find_opt]) so a re-entrant observer that mutates the
   collection mid-notification can never raise. *)

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
  mutable rorder : string list; (* ids newest-first (reverse insertion order); O(1) insert *)
  mutable observers : (int * observer) list;
  obs_counter : int ref;
  gen_id : unit -> string;
}

let create ?(gen_id = fun () -> Query.Id.random_id ()) () =
  { store = Hashtbl.create 64; rorder = []; observers = []; obs_counter = ref 0; gen_id }

type handle = { stop : unit -> unit }

(* raw simulated change stream: subscribe to insert/update/remove events *)
let watch t (f : observer) : handle =
  incr t.obs_counter;
  let i = !(t.obs_counter) in
  t.observers <- (i, f) :: t.observers;
  { stop = (fun () -> t.observers <- List.filter (fun (j, _) -> j <> i) t.observers) }

let notify t change = List.iter (fun (_, obs) -> obs change) t.observers

(* ids in insertion order *)
let ids t = List.rev t.rorder

(* ---- mutations ---- *)

let ensure_id t (d : doc) : string * doc =
  match get d "_id" with
  | Some v -> (Query.Diff.id_to_string v, d)
  | None ->
      let id = t.gen_id () in
      (id, Document (("_id", String id) :: Query.Diff.kvs_of d))

let insert t (d : doc) : string =
  let id, d = ensure_id t d in
  if not (Hashtbl.mem t.store id) then t.rorder <- id :: t.rorder;
  Hashtbl.replace t.store id d;
  notify t { op = Insert; id; new_doc = Some d; old_doc = None };
  id

let matching t selector =
  List.filter
    (fun id ->
      match Hashtbl.find_opt t.store id with
      | Some d -> Query.Matcher.doc_matches selector d
      | None -> false)
    (ids t)

(* update by selector; returns number affected. *)
let update t ?(multi = false) ?(upsert = false) (selector : doc) (modifier : doc) : int =
  let ms = matching t selector in
  let ms = if multi then ms else match ms with x :: _ -> [ x ] | [] -> [] in
  match ms with
  | [] when upsert ->
      (* seed an insert from the selector's plain equality fields (keep embedded documents; drop
         only operator keys and operator-expression values) + the modifier *)
      let base =
        Document
          (List.filter_map
             (fun (k, v) ->
               if Bson.is_operator_key k then None
               else if Query.Modifier.is_operator_doc v then None
               else Some (k, v))
             (Query.Diff.kvs_of selector))
      in
      ignore (insert t (Query.Modifier.apply ~insert:true base modifier));
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
              notify t { op = Update; id; new_doc = Some nw; old_doc = Some old })
        ms;
      !n

let remove t (selector : doc) : int =
  let ms = matching t selector in
  let dead = Hashtbl.create (List.length ms + 1) in
  List.iter (fun id -> Hashtbl.replace dead id ()) ms;
  List.iter
    (fun id ->
      match Hashtbl.find_opt t.store id with
      | Some old ->
          Hashtbl.remove t.store id;
          notify t { op = Remove; id; new_doc = None; old_doc = Some old }
      | None -> ())
    ms;
  (* single pass over the order list rather than one filter per removed id *)
  t.rorder <- List.filter (fun x -> not (Hashtbl.mem dead x)) t.rorder;
  List.length ms

(* ---- cursors / queries ---- *)

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

let all_docs t = List.filter_map (Hashtbl.find_opt t.store) (ids t)
let matched cur = List.filter (Query.Matcher.doc_matches cur.selector) (all_docs cur.coll)
let ordered cur = Query.Sorter.sort cur.sort (matched cur)

let windowed cur =
  let xs = ordered cur in
  let rec drop n = function [] -> [] | _ :: tl when n > 0 -> drop (n - 1) tl | l -> l in
  let xs = if cur.skip > 0 then drop cur.skip xs else xs in
  if cur.limit > 0 then
    let rec take n acc = function x :: tl when n > 0 -> take (n - 1) (x :: acc) tl | _ -> List.rev acc in
    take cur.limit [] xs
  else xs

let projection cur = Query.Projection.of_fields cur.fields
let fetch cur = List.map (Query.Projection.apply (projection cur)) (windowed cur)

let count cur =
  List.fold_left
    (fun n d -> if Query.Matcher.doc_matches cur.selector d then n + 1 else n)
    0 (all_docs cur.coll)

let is_empty cur = not (List.exists (Query.Matcher.doc_matches cur.selector) (all_docs cur.coll))
let for_each cur f = List.iter f (fetch cur)
let map cur f = List.map f (fetch cur)
let first cur = match fetch cur with x :: _ -> Some x | [] -> None

let find_one t ?(selector = Document []) ?(sort = Document []) ?(skip = 0)
    ?(fields = Document []) () =
  first (find t ~selector ~sort ~skip ~limit:1 ~fields ())

(* observeChanges — field-level, unordered membership routing. Honors selector + projection on live
   deltas; skip/limit affect only the initial snapshot. *)
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
  watch cur.coll route

(* observe — document-level. Recomputes the ordered window and diffs, so sort/skip/limit are
   honored and callbacks receive full documents. *)
let observe cur ?(added = fun _ -> ()) ?(changed = fun _ _ -> ()) ?(removed = fun _ -> ()) () :
    handle =
  let snap () = List.map (fun d -> (Query.Diff.doc_id d, d)) (windowed cur) in
  let prev = ref (snap ()) in
  List.iter (fun (_, d) -> added d) !prev;
  let recompute (_ : change) =
    let nw = snap () in
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
  watch cur.coll recompute
