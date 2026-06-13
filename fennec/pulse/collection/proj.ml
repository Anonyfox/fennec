(* A PROJECTION: Meteor's [{ fields: { a: 1, b: 1 } }] made type-safe. The [@@fields …] ppx builds
   one of these from a model's [Fields] handles — so it carries, from a SINGLE source:
   - [project_doc]: the Mongo projection document ({a:1, b:1}) the wire/cursor trims by;
   - [decode]: a decoder that builds an OBJECT with exactly the projected methods.
   The result type is the inferred object [< a : ta; b : tb >] — the full record is NEVER constructed
   on this path, so a projected-away field is unmentionable, not [undefined]. A field not on the
   model is an unbound [Fields.x] (compile error) at the projection site. *)

type 'o t = { project_doc : Bson.t; decode : Bson.t -> ('o, Codec.error list) result }

let v ~(fields : (string * int) list) ~(decode : Bson.t -> ('o, Codec.error list) result) : 'o t =
  { project_doc = Bson.Document (List.map (fun (n, i) -> (n, Bson.Int i)) fields); decode }

let project_doc t = t.project_doc
let decode t b = t.decode b
