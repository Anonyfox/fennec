(* The app's task model — ONE form: a plain record with the validation catalog inline as attributes.
   No builder, no Fields module by hand — [@@deriving fennec_collection] generates the codec, the
   typed Fields handles, the collection (+ $jsonSchema validator), all from this. Shared verbatim by
   the server binary and the JS bundle. A renamed field is a compile error everywhere it's used. *)

type t = {
  id : string;
  title : string;  [@trim] [@non_empty] [@max_len 200]
  body : string;   [@trim]
}
[@@deriving fennec_collection ~name:"tasks"]
