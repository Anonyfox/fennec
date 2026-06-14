module B = Bson

type observer = {
  oid : int;
  recompute : unit -> (string * B.t) list;
  added : string -> B.t -> unit;
  changed : string -> B.t -> string list -> unit;
  removed : string -> unit;
  cache : (string, B.t) Hashtbl.t; (* id -> last emitted fields *)
}

type t = { mutable next : int; by_coll : (string, observer list ref) Hashtbl.t }

let create () = { next = 0; by_coll = Hashtbl.create 16 }

let observers_of t coll =
  match Hashtbl.find_opt t.by_coll coll with
  | Some r -> r
  | None ->
    let r = ref [] in
    Hashtbl.replace t.by_coll coll r;
    r

(* fields present-and-changed (or new) in [nf] vs [of_], and the names cleared (in [of_], gone in [nf]) *)
let field_diff (old_doc : B.t) (new_doc : B.t) : B.t * string list =
  let of_ = B.fields old_doc and nf = B.fields new_doc in
  let changed = List.filter (fun (k, v) -> match List.assoc_opt k of_ with Some ov -> not (B.equal ov v) | None -> true) nf in
  let cleared = List.filter_map (fun (k, _) -> if List.mem_assoc k nf then None else Some k) of_ in
  (B.Document changed, cleared)

(* recompute the observer's query, diff against its cache, and emit the field-level deltas *)
let run_observer o =
  let cur = o.recompute () in
  let seen = Hashtbl.create 64 in
  List.iter
    (fun (id, fields) ->
      Hashtbl.replace seen id ();
      match Hashtbl.find_opt o.cache id with
      | None ->
        Hashtbl.replace o.cache id fields;
        o.added id fields
      | Some old when B.equal old fields -> ()
      | Some old ->
        let chg, cleared = field_diff old fields in
        Hashtbl.replace o.cache id fields;
        o.changed id chg cleared)
    cur;
  let gone = Hashtbl.fold (fun id _ acc -> if Hashtbl.mem seen id then acc else id :: acc) o.cache [] in
  List.iter
    (fun id ->
      Hashtbl.remove o.cache id;
      o.removed id)
    gone

let add t ~coll ~recompute ~added ~changed ~removed =
  let o = { oid = t.next; recompute; added; changed; removed; cache = Hashtbl.create 64 } in
  t.next <- t.next + 1;
  let r = observers_of t coll in
  r := o :: !r;
  run_observer o (* initial burst: empty cache -> every current doc is [added] *);
  fun () -> r := List.filter (fun o' -> o'.oid <> o.oid) !r

let notify t ~coll =
  match Hashtbl.find_opt t.by_coll coll with None -> () | Some r -> List.iter run_observer !r

let has_observers t ~coll =
  match Hashtbl.find_opt t.by_coll coll with Some r -> !r <> [] | None -> false
