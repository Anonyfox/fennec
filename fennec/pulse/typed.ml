(* The typed collection runtime, server side: a functor over any reactive instance. Every verb
   DELEGATES to the dynamic substrate (one implementation of insert/find/update/remove in the whole
   stack — this layer adds only the boundary translation):

   - writes VALIDATE: [insert] runs the shape's full check battery (encode_checked) and raises
     [Invalid] with the collected, path-tagged errors — an invalid value cannot reach the database
     through the typed layer (a method handler letting it escape yields a 422-style error, not a
     silent bad write);
   - reads DECODE with the skip policy: [find]/[find_one] silently skip documents that no longer
     match the declared shape (foreign garbage, legacy docs after a tightened rule) — the UI never
     crashes on them; [find_results] exposes every decode verdict for code that must care;
   - [cursor] yields the substrate's reactive cursor (sorted/windowed/projected) so typed
     collections plug into [publish] unchanged. *)

module Make (R : Reactive.REACTIVE) = struct
  exception Invalid of Codec.error list

  type 'a t = { def : 'a Def.t; coll : R.Collection.t }

  (* bind the pure declaration to this instance (boot-time, next to publish) *)
  let attach (def : 'a Def.t) backend : 'a t = { def; coll = R.Collection.create ~name:(Def.name def) backend }

  let collection t = t.coll (* the dynamic escape hatch *)
  let def t = t.def
  let validate t v = Codec.validate (Def.codec t.def) v

  let insert t v =
    match Codec.encode_checked (Def.codec t.def) v with
    | Ok b ->
        (* an EMPTY [_id] means "mint one for me" (the [{ id = ""; … }] convention) — strip it so
           the substrate's id_generation does its job; a non-empty id passes through *)
        let b =
          match b with
          | Bson.Document kvs ->
              Bson.Document (List.filter (function "_id", Bson.String "" -> false | _ -> true) kvs)
          | x -> x
        in
        R.Collection.insert t.coll b
    | Error es -> raise (Invalid es)

  (* [~where] is a LIST of clauses — Q.[ eq a 1; gt b 2 ] reads as AND (Q.all) *)
let sel where = match Q.all where with [] -> None | q -> Some (Q.to_bson q)

  let cursor t ?(where = []) ?sort ?skip ?limit () =
    R.Collection.find t.coll ?selector:(sel where) ?sort ?skip ?limit ()

  let find t ?(where = []) ?sort ?skip ?limit () : 'a list =
    R.Collection.fetch (cursor t ~where ?sort ?skip ?limit ())
    |> List.filter_map (fun d -> match Codec.decode (Def.codec t.def) d with Ok v -> Some v | Error _ -> None)

  let find_results t ?(where = []) ?sort ?skip ?limit () =
    R.Collection.fetch (cursor t ~where ?sort ?skip ?limit ()) |> List.map (Codec.decode (Def.codec t.def))

  let find_one t ?(where = []) ?sort () : 'a option =
    match R.Collection.find_one t.coll ?selector:(sel where) ?sort () with
    | Some d -> ( match Codec.decode (Def.codec t.def) d with Ok v -> Some v | Error _ -> None)
    | None -> None

  let count t ?(where = []) () = R.Collection.count t.coll ?selector:(sel where) ()

  let update t ?(multi = true) ~where m =
    R.Collection.update t.coll ~multi (Q.to_bson (Q.all where)) (M.to_bson m)

  let remove t ~where = R.Collection.remove t.coll (Q.to_bson (Q.all where))
end
