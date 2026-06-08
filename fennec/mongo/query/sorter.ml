(* Compile a sort spec ({field: 1|-1, …}) into a document comparator. Uses {!Bson.compare} — the
   total value order with a defined BSON type precedence — so cross-type sorting is well-defined
   (never the arbitrary constructor-declaration order of [Stdlib.compare]). Missing fields sort
   before present ones; an empty/absent spec keeps input order. Sort keys are expected to be scalar
   values. *)

(* Returns a comparator `doc -> doc -> int`. An empty/absent spec keeps input order (the
   comparator returns 0). *)
let of_spec (spec : Bson.t) : Bson.t -> Bson.t -> int =
  let keys = match spec with Bson.Document kvs -> kvs | _ -> [] in
  fun d1 d2 ->
    let rec go = function
      | [] -> 0
      | (k, dir) :: rest ->
          let sign =
            match dir with
            | Bson.Int n when n < 0 -> -1
            | Bson.Float f when f < 0. -> -1
            | _ -> 1
          in
          let c =
            match (Matcher.get_path d1 k, Matcher.get_path d2 k) with
            | Some a, Some b -> Bson.compare a b
            | None, Some _ -> -1
            | Some _, None -> 1
            | None, None -> 0
          in
          if c <> 0 then sign * c else go rest
    in
    go keys

let sort (spec : Bson.t) (docs : Bson.t list) : Bson.t list =
  match spec with
  | Bson.Document (_ :: _) -> List.stable_sort (of_spec spec) docs
  | _ -> docs
