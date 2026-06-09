(* Pure document helpers + the diff/LCS machinery shared by the observe engines. No Eio, no Unix,
   no Yojson — so it cross-compiles to JavaScript unchanged. *)

open Bson

let kvs_of = function Document kvs -> kvs | _ -> []

let id_to_string = function
  | String s -> s
  | Object_id s -> s
  | Int n -> string_of_int n
  | Int64 n -> Int64.to_string n
  | _ -> ""

(* The _id of a document, as a string (minimongo ids are strings by default). *)
let doc_id (d : Bson.t) =
  match get d "_id" with Some v -> id_to_string v | None -> ""

let fields_without_id (d : Bson.t) : Bson.t =
  Document (List.filter (fun (k, _) -> k <> "_id") (kvs_of d))

(* Merge a partial update (changed fields + cleared field names) into a base document. Top-level
   keys only; field order is preserved (existing fields keep their position with the new value; new
   fields are appended once, in [updated] order). *)
let merge_doc (base : Bson.t) ~(updated : (string * Bson.t) list)
    ~(removed : string list) : Bson.t =
  let kept = List.filter (fun (k, _) -> not (List.mem k removed)) (kvs_of base) in
  let kept =
    List.map
      (fun (k, v) -> match List.assoc_opt k updated with Some nv -> (k, nv) | None -> (k, v))
      kept
  in
  let existing = List.map fst kept in
  let added = List.filter (fun (k, _) -> not (List.mem k existing)) updated in
  Document (kept @ added)

(* Fields set/added going old -> new, and the names of fields that vanished. *)
let diff_fields ~(old_doc : Bson.t) ~(new_doc : Bson.t) :
    (string * Bson.t) list * string list =
  let o = kvs_of old_doc and n = kvs_of new_doc in
  let changed =
    List.filter
      (fun (k, v) ->
        k <> "_id"
        && match List.assoc_opt k o with Some ov -> not (Bson.equal ov v) | None -> true)
      n
  in
  let cleared =
    List.filter_map
      (fun (k, _) -> if k <> "_id" && not (List.mem_assoc k n) then Some k else None)
      o
  in
  (changed, cleared)

(* A document's membership transition across one mutation — a closed variant so every case is
   handled exactly once. *)
type transition = Entered | Stayed | Left | Outside

let transition ~was ~now =
  match (was, now) with
  | false, true -> Entered
  | true, true -> Stayed
  | true, false -> Left
  | false, false -> Outside

(* Longest common subsequence of two id lists (for minimal ordered moves). *)
let lcs_ids (a : string list) (b : string list) : string list =
  let na = Array.of_list a and nb = Array.of_list b in
  let la = Array.length na and lb = Array.length nb in
  let dp = Array.make_matrix (la + 1) (lb + 1) 0 in
  for i = la - 1 downto 0 do
    for j = lb - 1 downto 0 do
      dp.(i).(j) <-
        (if na.(i) = nb.(j) then dp.(i + 1).(j + 1) + 1
         else max dp.(i + 1).(j) dp.(i).(j + 1))
    done
  done;
  let rec build i j acc =
    if i >= la || j >= lb then List.rev acc
    else if na.(i) = nb.(j) then build (i + 1) (j + 1) (na.(i) :: acc)
    else if dp.(i + 1).(j) >= dp.(i).(j + 1) then build (i + 1) j acc
    else build i (j + 1) acc
  in
  build 0 0 []

(* Emit observeChanges-ordered operations that transform [old_list] into [new_list] (both id->doc
   associations in order). Lookups are indexed by hashtable; the O(m²) LCS is computed only when the
   surviving ids actually changed order. Processed right-to-left so every `before` reference is
   already placed. *)
let diff_ordered ~(old_list : (string * Bson.t) list)
    ~(new_list : (string * Bson.t) list) ~added_before ~changed ~moved_before
    ~removed =
  let new_tbl = Hashtbl.create (List.length new_list + 1) in
  List.iter (fun (id, d) -> Hashtbl.replace new_tbl id d) new_list;
  let old_tbl = Hashtbl.create (List.length old_list + 1) in
  List.iter (fun (id, d) -> Hashtbl.replace old_tbl id d) old_list;
  let in_new id = Hashtbl.mem new_tbl id in
  let in_old id = Hashtbl.mem old_tbl id in
  List.iter (fun (id, _) -> if not (in_new id) then removed id) old_list;
  let survivors_old =
    List.filter_map (fun (id, _) -> if in_new id then Some id else None) old_list
  in
  let survivors_new =
    List.filter_map (fun (id, _) -> if in_old id then Some id else None) new_list
  in
  (* common case (no reordering): every survivor stays put, so skip the LCS matrix entirely *)
  let keep = if survivors_old = survivors_new then survivors_new else lcs_ids survivors_old survivors_new in
  let keep_set = Hashtbl.create (List.length keep + 1) in
  List.iter (fun id -> Hashtbl.replace keep_set id ()) keep;
  let is_kept id = Hashtbl.mem keep_set id in
  let new_arr = Array.of_list (List.map fst new_list) in
  let n = Array.length new_arr in
  let before_of i = if i + 1 < n then Some new_arr.(i + 1) else None in
  for i = n - 1 downto 0 do
    let id = new_arr.(i) in
    match Hashtbl.find_opt new_tbl id with
    | None -> ()
    | Some nd ->
        if not (in_old id) then added_before id (fields_without_id nd) (before_of i)
        else begin
          (match Hashtbl.find_opt old_tbl id with
          | Some od ->
              let chg, cleared = diff_fields ~old_doc:od ~new_doc:nd in
              if chg <> [] || cleared <> [] then changed id (Document chg) cleared
          | None -> ());
          if not (is_kept id) then moved_before id (before_of i)
        end
  done
