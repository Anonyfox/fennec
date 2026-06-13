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

(* Schwartzian transform: extract each doc's sort-key vector ONCE (the dotted-path walks are the
   expensive part), sort on the precomputed keys, then strip. Equivalent to [stable_sort (of_spec
   spec)] — same sign rules, same missing-field ordering (absent sorts first), same stability — but it
   walks the paths O(n·fields) times instead of O(n·log n·fields). *)
let sort (spec : Bson.t) (docs : Bson.t list) : Bson.t list =
  match spec with
  | Bson.Document ((_ :: _) as keys) ->
      let fields =
        List.map
          (fun (k, dir) ->
            let sign = match dir with Bson.Int n when n < 0 -> -1 | Bson.Float f when f < 0. -> -1 | _ -> 1 in
            (k, sign))
          keys
      in
      let decorate d = (List.map (fun (k, sign) -> (sign, Matcher.get_path d k)) fields, d) in
      let rec cmp_keys l1 l2 =
        match (l1, l2) with
        | (sign, a) :: r1, (_, b) :: r2 ->
            let c =
              match (a, b) with
              | Some a, Some b -> Bson.compare a b
              | None, Some _ -> -1
              | Some _, None -> 1
              | None, None -> 0
            in
            if c <> 0 then sign * c else cmp_keys r1 r2
        | _ -> 0
      in
      List.map decorate docs |> List.stable_sort (fun (k1, _) (k2, _) -> cmp_keys k1 k2) |> List.map snd
  | _ -> docs
