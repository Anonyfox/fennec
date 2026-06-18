(* Proves [@@deriving model] works through the standalone sift.ppx driver, linking ONLY
   fennec.pulse.sift — NO collection, NO Mongo, NO fur. A plain record → a working codec + Fields. *)

type t = { id : string; title : string; tags : string list; note : string option }
[@@deriving model]

let () =
  let v = { id = "507f1f77bcf86cd799439011"; title = "hi"; tags = [ "a"; "b" ]; note = Some "n" } in
  let ok =
    (* the generated codec round-trips through BSON and native JSON *)
    (match Sift.decode codec (Sift.to_bson codec v) with Ok v' -> Sift.equal codec v v' | _ -> false)
    && (match Sift.decode_json codec (Sift.encode_json codec v) with Ok v' -> Sift.equal codec v v' | _ -> false)
    (* the generated Fields handles carry the wire name + requiredness *)
    && String.equal (Sift.field_name Fields.title) "title"
    && Sift.field_required Fields.id (* a field named id ⇒ doc_id (required) *)
    && not (Sift.field_required Fields.note) (* an option field ⇒ opt (not required) *)
  in
  if ok then print_endline "deriving model: standalone codec + Fields OK"
  else (
    prerr_endline "deriving model: FAILED";
    exit 1)
