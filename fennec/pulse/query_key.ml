(* A stable identity string for a cursor's query — the key the observe multiplexer (RX9) shares ONE
   backend observe under, across every subscription with the same (collection, query).

   The query is canonicalized so a selector's key ORDER is irrelevant ({a;b} keys the same as {b;a}).
   That choice only ever WIDENS sharing: two genuinely different queries always produce different keys
   (the canonical form is injective up to key order), so distinct subscriptions can never collapse onto
   one observe by mistake. Arrays keep their order ($and / $or operands are positional); only Document
   keys are sorted. *)

(* write the canonical form into [buf] — one Buffer instead of a tree of [^]/[concat] allocations;
   byte-identical to the old string form (empty doc → "{}", "{k:v}", "{k1:v1,k2:v2}", …) *)
let rec canon_buf buf (b : Bson.t) =
  match b with
  | Bson.Document kvs ->
      let sorted = List.sort (fun (a, _) (b, _) -> String.compare a b) kvs in
      Buffer.add_char buf '{';
      List.iteri
        (fun i (k, v) ->
          if i > 0 then Buffer.add_char buf ',';
          Buffer.add_string buf k;
          Buffer.add_char buf ':';
          canon_buf buf v)
        sorted;
      Buffer.add_char buf '}'
  | Bson.Array xs ->
      Buffer.add_char buf '[';
      List.iteri (fun i v -> if i > 0 then Buffer.add_char buf ','; canon_buf buf v) xs;
      Buffer.add_char buf ']'
  | other -> Buffer.add_string buf (Bson.to_string other)

let canon (b : Bson.t) : string =
  let buf = Buffer.create 64 in
  canon_buf buf b;
  Buffer.contents buf

let of_query ~collection (q : Backend.query) : string =
  String.concat "\x00"
    [ collection; canon q.selector; canon q.sort; string_of_int q.skip; string_of_int q.limit; canon q.fields ]
