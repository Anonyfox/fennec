(* The stub's write surface — Method.sim_writes over the client merge store. A method stub runs
   against THIS, never the server collections: inserts/updates land as the sim sub's field writes
   (top precedence — instant UI), removes tombstone via sim_hide, and the whole simulation rolls
   back to server truth with one sub_stopped when the method's [updated] arrives.

   Insert ids mint from the call's seed, per-collection ([Method.Seed.stream]), so the server's
   seeded handler mints the SAME ids and the optimistic doc converges with the real one (no
   duplicate-then-vanish). Caveat: convergence covers STRING id collections (the default); a MONGO
   (ObjectId) collection's server id differs, so its optimistic insert swaps once at reveal. Updates
   run the real Modifier engine over the current merged docs — the same semantics the server applies,
   to the extent the client has the docs. Pure (no browser dependency), so simulations are unit
   tested natively. *)

module MS = Merge_store

(* the TYPED optimistic insert: the stub validates with the SAME battery the server enforces —
   instant offline form errors with zero duplicated logic; an invalid value raises (caught by the
   stub-failure containment: logged, simulation skipped, the server still decides) *)
let insert_t (w : Method.sim_writes) (def : 'a Def.t) (v : 'a) : string =
  match Sift.encode_checked (Def.codec def) v with
  | Ok (Bson.Document kvs) ->
      let kvs = List.filter (function "_id", Bson.String "" -> false | _ -> true) kvs in
      w.Method.insert (Def.name def) (Bson.Document kvs)
  | Ok _ -> invalid_arg "Sim.insert_t: codec must encode a document"
  | Error es -> failwith ("invalid document: " ^ Sift.errors_to_string es)

let _doc_id where kvs =
  match List.assoc_opt "_id" kvs with
  | Some (Bson.String s) | Some (Bson.Object_id s) -> s
  | _ -> failwith (where ^ ": the value has no _id (read it from a query before saving/deleting)")

(* TYPED optimistic save: predict a full-document update by [_id] against the cache (the client mirror of
   the server's $set-by-_id). Validates with the same battery. No-op until the client has the doc. *)
let save_t (w : Method.sim_writes) (def : 'a Def.t) (v : 'a) : unit =
  match Sift.encode_checked (Def.codec def) v with
  | Ok (Bson.Document kvs) ->
      let id = _doc_id "Sim.save_t" kvs in
      let fields = List.filter (fun (k, _) -> k <> "_id") kvs in
      let sel = Bson.Document [ ("_id", Bson.String id) ] in
      let modifier = Bson.Document [ ("$set", Bson.Document fields) ] in
      ignore (w.Method.update (Def.name def) sel modifier)
  | Ok _ -> invalid_arg "Sim.save_t: codec must encode a document"
  | Error es -> failwith ("invalid document: " ^ Sift.errors_to_string es)

(* TYPED optimistic delete: tombstone the doc by [_id] for the simulation (server truth restores on
   reveal). Takes the value (so the verb mirrors the server's [delete]); only its [_id] is used. *)
let remove_t (w : Method.sim_writes) (def : 'a Def.t) (v : 'a) : unit =
  match Sift.encode_checked (Def.codec def) v with
  | Ok (Bson.Document kvs) ->
      ignore (w.Method.remove (Def.name def) (Bson.Document [ ("_id", Bson.String (_doc_id "Sim.remove_t" kvs)) ]))
  | Ok _ -> invalid_arg "Sim.remove_t: codec must encode a document"
  | Error es -> failwith ("invalid document: " ^ Sift.errors_to_string es)

let writes (box : MS.t) ~sim ~seed : Method.sim_writes =
  MS.begin_sim box sim;
  let streams : (string, int -> int) Hashtbl.t = Hashtbl.create 4 in
  let rng coll =
    match Hashtbl.find_opt streams coll with
    | Some r -> r
    | None ->
        let r = Method.Seed.stream ~seed ~scope:coll in
        Hashtbl.replace streams coll r;
        r
  in
  let insert coll d =
    let kvs = Query.Diff.kvs_of d in
    let id =
      match List.assoc_opt "_id" kvs with
      | Some (Bson.String s) | Some (Bson.Object_id s) -> s
      | _ -> Query.Id.random_id ~rng:(rng coll) ()
    in
    let fields = List.filter (fun (k, _) -> k <> "_id") kvs in
    MS.added box ~sub:sim ~collection:coll ~id ~fields;
    id
  in
  let update coll sel modifier =
    let docs = MS.fetch box coll ~selector:sel () in
    Array.iter
      (fun old ->
        let id = Query.Diff.doc_id old in
        let nw = Query.Modifier.apply old modifier in
        let chg, cleared =
          Query.Diff.diff_fields
            ~old_doc:(Query.Diff.fields_without_id old)
            ~new_doc:(Query.Diff.fields_without_id nw)
        in
        if chg <> [] || cleared <> [] then
          MS.changed box ~sub:sim ~collection:coll ~id ~fields:chg ~cleared)
      docs;
    Array.length docs
  in
  let remove coll sel =
    let docs = MS.fetch box coll ~selector:sel () in
    Array.iter (fun d -> MS.sim_hide box ~sub:sim ~collection:coll ~id:(Query.Diff.doc_id d)) docs;
    Array.length docs
  in
  { Method.insert; update; remove }
