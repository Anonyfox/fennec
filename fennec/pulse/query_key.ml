(* A stable identity string for a cursor's query — the key the observe multiplexer (RX9) shares ONE
   backend observe under, across every subscription with the same (collection, query).

   The query is canonicalized so a selector's key ORDER is irrelevant ({a;b} keys the same as {b;a}).
   That choice only ever WIDENS sharing: two genuinely different queries always produce different keys
   (the canonical form is injective up to key order), so distinct subscriptions can never collapse onto
   one observe by mistake. Arrays keep their order ($and / $or operands are positional); only Document
   keys are sorted. *)

let rec canon (b : Bson.t) : string =
  match b with
  | Bson.Document kvs ->
      let sorted = List.sort (fun (a, _) (b, _) -> String.compare a b) kvs in
      "{" ^ String.concat "," (List.map (fun (k, v) -> k ^ ":" ^ canon v) sorted) ^ "}"
  | Bson.Array xs -> "[" ^ String.concat "," (List.map canon xs) ^ "]"
  | other -> Bson.to_string other

let of_query ~collection (q : Backend.query) : string =
  String.concat "\x00"
    [ collection; canon q.selector; canon q.sort; string_of_int q.skip; string_of_int q.limit; canon q.fields ]
