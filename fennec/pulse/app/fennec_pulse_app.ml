(* The server data facade. Wraps the Reactive/server/Typed functors over the production Dynamic
   backend (mem-or-mongo, chosen by the global Mongo env) into ONE ambient module. A server file
   does: Pulse.start → declare collections' publications/methods → done. No functor aliases, no
   per-collection backend threading, no double-declared SSR publication. *)

module D = Fennec_pulse_mongo.Dynamic
module R = Fennec_pulse.Reactive.Make (D)
module RT = Fennec_pulse_server.Make (R)
module T = Fennec_pulse.Typed.Make (R)

(* the ambient connection config (Eio switch + db), set once at boot by [start] *)
let _cfg : (Eio.Switch.t * string) option ref = ref None
let start ~sw ~db () = _cfg := Some (sw, db)
let cfg () = match !_cfg with Some x -> x | None -> failwith "Fennec_pulse_app: call start before using collections"

(* one reactive collection per name (stable mux uid; indexes reconciled once on creation), re-wrapped
   per call as a cheap typed handle *)
let _reactives : (string, R.Collection.t) Hashtbl.t = Hashtbl.create 16

let collection (def : 'a Def.t) : 'a T.t =
  let name = Def.name def in
  let coll =
    match Hashtbl.find_opt _reactives name with
    | Some c -> c
    | None ->
        let sw, db = cfg () in
        let c = R.Collection.create ~name (D.from_env ~sw ~db ~name ()) in
        Hashtbl.replace _reactives name c;
        T.reconcile c (Def.all_indexes def);
        c
  in
  T.of_reactive def coll

(* server writes (used inside method handlers): validating, ambient *)
let insert def v = T.insert (collection def) v
let seed def vs = List.iter (fun v -> ignore (insert def v)) vs
let update def ?multi ~where m = T.update (collection def) ?multi ~where m
let upsert def ?multi ~where m = T.upsert (collection def) ?multi ~where m
let remove def ~where = T.remove (collection def) ~where

(* ONE publish call: the live DDP publication AND the SSR seed, both keyed by the collection's name.
   [~where] (params → typed clauses) filters; default publishes the whole collection. *)
let publish ?(where = fun _ -> []) (def : 'a Def.t) =
  let name = Def.name def in
  let h = collection def in
  R.publish name (fun (pub : R.publication) -> R.Cursor (T.cursor h ~where:(where pub.params) ()));
  Ddp_client.publish ~name (fun params ->
      let selector = Filter.to_bson (Filter.all (where params)) in
      [ (name, R.Collection.fetch (R.Collection.find ~selector (T.collection h) ())) ])

(* register a typed method handler (the one client write path) *)
let method_ m handler = R.handle m handler

(* the DDP websocket paw for the endpoint pipeline *)
let serve_ddp ?path () = RT.paw ?path ()
