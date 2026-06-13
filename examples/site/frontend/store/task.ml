(* The app's task model — ONE form: a plain record with the validation catalog inline as attributes.
   No builder, no Fields module by hand — [@@deriving collection] generates the codec, the
   typed Fields handles, the collection (+ $jsonSchema validator), all from this. Shared verbatim by
   the server binary and the JS bundle. A renamed field is a compile error everywhere it's used. *)

type t = {
  id : string;
  title : string;  [@trim] [@non_empty] [@max_len 200]
  body : string;   [@trim]
}
[@@deriving collection ~name:"tasks"]

(* the client view of the collection — bind once here, then `open Task` and use `Tasks.find …`
   anywhere with no client/handle threading (Meteor's `Tasks`). Reads only; writes are methods. *)
(* declared indexes — co-located with the model, reconciled at boot by T.attach (created in the
   backend, fennec-named so a future removal auto-drops the orphan) *)
let () = Def.index collection Index.[ asc Fields.title ]

module Tasks = Pulse.Collection (struct
  type doc = t
  let collection = collection
end)
