(* Compile a sort spec ({field: 1|-1, …}) into a document comparator. Reuses the matcher's value
   comparison; missing fields sort before present ones. *)

open Bson

let cmp_vals a b =
  match Matcher.bson_cmp a b with Some c -> c | None -> Stdlib.compare a b

(* Returns a comparator `doc -> doc -> int`. An empty/absent spec keeps input order (the
   comparator returns 0). *)
let of_spec (spec : Bson.t) : Bson.t -> Bson.t -> int =
  let keys = match spec with Document kvs -> kvs | _ -> [] in
  fun d1 d2 ->
    let rec go = function
      | [] -> 0
      | (k, dir) :: rest ->
          let sign =
            match dir with
            | Int n when n < 0 -> -1
            | Float f when f < 0. -> -1
            | _ -> 1
          in
          let c =
            match (Matcher.get_path d1 k, Matcher.get_path d2 k) with
            | Some a, Some b -> cmp_vals a b
            | None, Some _ -> -1
            | Some _, None -> 1
            | None, None -> 0
          in
          if c <> 0 then sign * c else go rest
    in
    go keys

let sort (spec : Bson.t) (docs : Bson.t list) : Bson.t list =
  match spec with Document (_ :: _) -> List.stable_sort (of_spec spec) docs | _ -> docs
