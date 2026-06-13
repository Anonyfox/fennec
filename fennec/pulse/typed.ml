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

  (* RECONCILE the declared indexes against what the backend actually has — the lifecycle keystone
     (Meteor never closed this loop): create the missing, DROP the fennec-named orphans (an index we
     created for a since-removed declaration), never touch _id_ or a hand-made index. Idempotent;
     graceful — a failed build (e.g. unique over duplicate data) is logged, not fatal, unless
     [~strict]. Names encode the spec, so a changed declaration auto-migrates (old dropped, new made). *)
  let reconcile_indexes ?(strict = false) (coll : R.Collection.t) (declared : Index.t list) =
    let want = List.map (fun ix -> (Index.name ix, ix)) declared in
    let have = R.Collection.index_names coll in
    (* create the missing *)
    List.iter
      (fun (name, ix) ->
        if not (List.mem name have) then
          try R.Collection.ensure_index coll ~name ~keys:(Index.keys_bson ix) ~unique:(Index.is_unique ix)
          with e ->
            let msg = Printf.sprintf "fennec/index: could not build %s — %s" name (Printexc.to_string e) in
            if strict then failwith msg else prerr_endline msg)
      want;
    (* drop the fennec-named orphans (declared elsewhere, removed here) *)
    List.iter
      (fun name -> if Index.is_fennec_name name && not (List.mem_assoc name want) then
          try R.Collection.drop_index coll ~name with _ -> ())
      have

  (* bind the pure declaration to this instance (boot-time, next to publish) + reconcile its indexes *)
  let attach ?strict (def : 'a Def.t) backend : 'a t =
    let coll = R.Collection.create ~name:(Def.name def) backend in
    reconcile_indexes ?strict coll (Def.all_indexes def);
    { def; coll }

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
  let srt = Option.map Sort.to_bson

  let cursor t ?(where = []) ?sort ?skip ?limit ?project () =
    let fields = Option.map Proj.project_doc project in
    R.Collection.find t.coll ?selector:(sel where) ?sort:(srt sort) ?skip ?limit ?fields ()

  (* a PROJECTED read: only the projection's fields cross the boundary, decoded into its object
     type; malformed rows skipped (the same policy as [find]) *)
  let find_p t (p : 'o Proj.t) ?(where = []) ?sort ?skip ?limit () : 'o list =
    R.Collection.fetch (cursor t ~where ?sort ?skip ?limit ~project:p ())
    |> List.filter_map (fun d -> match Proj.decode p d with Ok v -> Some v | Error _ -> None)

  let find t ?(where = []) ?sort ?skip ?limit () : 'a list =
    R.Collection.fetch (cursor t ~where ?sort ?skip ?limit ())
    |> List.filter_map (fun d -> match Codec.decode (Def.codec t.def) d with Ok v -> Some v | Error _ -> None)

  let find_results t ?(where = []) ?sort ?skip ?limit () =
    R.Collection.fetch (cursor t ~where ?sort ?skip ?limit ()) |> List.map (Codec.decode (Def.codec t.def))

  let find_one t ?(where = []) ?sort () : 'a option =
    match R.Collection.find_one t.coll ?selector:(sel where) ?sort:(srt sort) () with
    | Some d -> ( match Codec.decode (Def.codec t.def) d with Ok v -> Some v | Error _ -> None)
    | None -> None

  let count t ?(where = []) () = R.Collection.count t.coll ?selector:(sel where) ()

  let update t ?(multi = true) ~where m =
    R.Collection.update t.coll ~multi (Q.to_bson (Q.all where)) (M.to_bson m)

  (* typed upsert: the modifier runs whether it matched or inserted ($setOnInsert covers
     insert-only fields). Returns the engine's affected count + any newly-minted id. *)
  let upsert t ?(multi = false) ~where m =
    let r = R.Collection.upsert t.coll ~multi (Q.to_bson (Q.all where)) (M.to_bson m) in
    (r.R.Collection.number_affected, r.R.Collection.inserted_id)

  let remove t ~where = R.Collection.remove t.coll (Q.to_bson (Q.all where))

  (* distinct values of one field across the matching docs, decoded to the field's type (values
     that don't decode are skipped — the read policy) *)
  let distinct t (f : 'b Codec.field) ?(where = []) () : 'b list =
    R.Collection.distinct t.coll ~key:(Codec.field_name f) ?selector:(sel where) ()
    |> List.filter_map (fun v -> match Codec.field_get f (Bson.doc [ (Codec.field_name f, v) ]) with Ok x -> Some x | Error _ -> None)
end
