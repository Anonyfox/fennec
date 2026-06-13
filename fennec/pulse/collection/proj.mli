(** A type-safe projection — Meteor's [{ fields: {…} }], built by the [\[%fields …\]] ppx from a
    model's [Fields] handles. Carries the wire projection document AND a decoder that yields an
    OBJECT of exactly the projected methods (so the full record is never constructed; a
    projected-away field is unmentionable). A field absent from the model is a compile error at the
    projection site (unbound [Fields.x]). 

    {[ (* [%fields] — Meteor's { fields: {…} }, typed; yields an object of exactly those fields *)
       let cards = Task.project [%fields title; author / name] ()   (* < title : …; author : < name : … > > *)
       (* o#title works; o#body is a COMPILE error (not in the projection) *) ]}
*)

type 'o t

(** Used by the ppx; userland writes [\[%fields …\]]. *)
val v : fields:(string * Bson.t) list -> decode:(Bson.t -> ('o, Codec.error list) result) -> 'o t

(** The Mongo projection document ([{a:1, b:1}]) — what the cursor/wire trims by. *)
val project_doc : 'o t -> Bson.t

val decode : 'o t -> Bson.t -> ('o, Codec.error list) result
