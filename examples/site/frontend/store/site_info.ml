(* A small TYPED isomorphic payload — the model behind the <Site_info> card's Data.model resource. The
   server computes a value, encodes it with THIS codec ([Sift.encode_json]) for both the SSR seed and the
   /api/site-info route; the client reads it back with the SAME codec ([Data.model] -> [Sift.decode_json]),
   so wire shape = model shape with no second serializer and the value is typed end to end (a renamed/
   retyped field is a compile error on both sides). Plain shared model in the store so the server binary
   and the JS bundle agree byte-for-byte. *)

type t = {
  name : string;       [@non_empty]
  tagline : string;
  stars : int;         [@min 0]
}
[@@deriving model]

let empty = { name = "Fennec"; tagline = "…"; stars = 0 }
