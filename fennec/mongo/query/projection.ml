(* Field projection — true nested (dotted-path) include/exclude, plus the array projection
   operators $slice and $elemMatch. Takes the projection spec as a Bson document (Minimongo's
   `fields`/`projection` option).

   A spec is compiled into a path TREE: [{"a.b": 1}] keeps only [a.b] (the rest of [a] is dropped),
   not all of [a]. Include vs exclude mode is decided by the first plain [0]/[1] field (other than
   [_id]); [_id] is kept unless explicitly excluded. The positional projection operator [$] is not
   supported (it needs the query selector). *)

open Bson

(* a compiled projection node for one field *)
type node =
  | Keep (* include this whole field/subtree *)
  | Drop (* exclude this whole field/subtree *)
  | Descend of (string * node) list (* recurse into a subdocument *)
  | Slice of int option * int (* $slice: optional skip, then count *)
  | Elem of Bson.t (* $elemMatch selector — keep the first matching array element *)

type t =
  | Identity
  | Project of { include_mode : bool; tree : (string * node) list; keep_id : bool }

let truthy = function Int 0 | Bool false -> false | _ -> true

let rec drop n = function [] -> [] | _ :: tl when n > 0 -> drop (n - 1) tl | l -> l

let take n xs =
  let rec go n acc = function x :: tl when n > 0 -> go (n - 1) (x :: acc) tl | _ -> List.rev acc in
  go n [] xs

let slice_array skip lim xs =
  match skip with
  | Some s ->
      let s = if s < 0 then max 0 (List.length xs + s) else s in
      take (max 0 lim) (drop s xs)
  | None ->
      if lim >= 0 then take lim xs
      else
        let n = List.length xs in
        drop (max 0 (n + lim)) xs

(* tree construction: merge a dotted path into the tree *)
let rec insert_path tree segs leaf =
  match segs with
  | [] -> tree
  | [ k ] -> set_node tree k leaf
  | k :: rest ->
      let sub = match List.assoc_opt k tree with Some (Descend t) -> t | _ -> [] in
      set_node tree k (Descend (insert_path sub rest leaf))

and set_node tree k node =
  if List.mem_assoc k tree then List.map (fun (k2, n) -> if k2 = k then (k, node) else (k2, n)) tree
  else tree @ [ (k, node) ]

let leaf_of v =
  match v with
  | Document [ ("$slice", Int n) ] -> Slice (None, n)
  | Document [ ("$slice", Array [ Int sk; Int lim ]) ] -> Slice (Some sk, lim)
  | Document kvs when List.mem_assoc "$elemMatch" kvs -> Elem (List.assoc "$elemMatch" kvs)
  | _ -> if truthy v then Keep else Drop

(* a field whose value participates in include/exclude mode selection (a plain 0/1, not a $slice/
   $elemMatch operator document) *)
let is_plain_flag = function Int _ | Bool _ -> true | _ -> false

let of_fields (spec : Bson.t) : t =
  match spec with
  | Document ((_ :: _) as kvs) ->
      let plain_non_id = List.filter (fun (k, v) -> k <> "_id" && is_plain_flag v) kvs in
      let include_mode = match plain_non_id with (_, v) :: _ -> truthy v | [] -> true in
      let keep_id = match List.assoc_opt "_id" kvs with Some v -> truthy v | None -> true in
      let tree =
        List.fold_left
          (fun acc (k, v) ->
            if k = "_id" then acc else insert_path acc (String.split_on_char '.' k) (leaf_of v))
          [] kvs
      in
      Project { include_mode; tree; keep_id }
  | _ -> Identity

let rec apply_tree include_mode tree (d : Bson.t) : Bson.t =
  match d with
  | Document kvs ->
      Document
        (List.filter_map
           (fun (k, v) ->
             match List.assoc_opt k tree with
             | Some node -> apply_node include_mode node k v
             | None -> if include_mode then None else Some (k, v))
           kvs)
  | other -> other

and apply_node include_mode node k v =
  match node with
  | Keep -> Some (k, v)
  | Drop -> None
  | Descend sub -> Some (k, apply_tree include_mode sub v)
  | Slice (skip, lim) -> (
      match v with Array xs -> Some (k, Array (slice_array skip lim xs)) | _ -> Some (k, v))
  | Elem sel -> (
      match v with
      | Array xs -> (
          match List.find_opt (fun e -> Matcher.doc_matches sel e) xs with
          | Some e -> Some (k, Array [ e ])
          | None -> None)
      | _ -> Some (k, v))

let apply (proj : t) (d : Bson.t) : Bson.t =
  match proj with
  | Identity -> d
  | Project { include_mode; tree; keep_id } -> (
      match d with
      | Document kvs ->
          let projected =
            match apply_tree include_mode tree (Document kvs) with Document k -> k | _ -> []
          in
          let projected =
            if keep_id then
              match List.assoc_opt "_id" kvs with
              | Some idv when include_mode && not (List.mem_assoc "_id" projected) ->
                  ("_id", idv) :: projected
              | _ -> projected
            else List.filter (fun (k, _) -> k <> "_id") projected
          in
          Document projected
      | other -> other)

let seg k = match String.index_opt k '.' with Some i -> String.sub k 0 i | None -> k

let cleared proj (names : string list) : string list =
  match proj with
  | Identity -> names
  | Project { include_mode = true; tree; _ } -> List.filter (fun k -> List.mem_assoc (seg k) tree) names
  | Project { include_mode = false; tree; _ } ->
      List.filter (fun k -> not (List.mem_assoc (seg k) tree)) names
