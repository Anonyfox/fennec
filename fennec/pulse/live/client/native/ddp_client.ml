(* Native / SSR side of the DDP client. There is no WebSocket on the server, so this is the SSR
   reactive: [publish] registers a publication's initial-document fetcher (server-side); during the
   SSR render a component's [subscribe]/[use_subscribe] runs it, seeds the per-render store (so
   [find] renders the real data, not []) AND embeds the docs via Fur's seed <script> — so the
   browser hydrates flicker-free, then its live subscription re-confirms + streams. A fetch that
   can't run synchronously (e.g. a real-mongo query, which needs Eio) degrades to a loading client
   (ready stays false; the browser fills in after hydration). *)

module MS = Fennec_pulse_live.Merge_store
module Live = Fennec_pulse_live.Live
module Subkey = Fennec_pulse_live.Subkey
module BJ = Fennec_mongo_bson_json.Bson_json

type t = { live : Live.t }
type subscription = { ready : bool Fur.signal; stop : unit -> unit }

(* SSR publication registry: name → its current documents. Populated on the server by [publish],
   read during the SSR render by [subscribe]. *)
let _pubs : (string, Bson.t list -> Bson.t list) Hashtbl.t = Hashtbl.create 8
let publish ~name f = Hashtbl.replace _pubs name f
let seed_key name params = "ddp:" ^ Subkey.key name params

let connect ?path () =
  ignore path;
  { live = Live.create () }

let subscribe t ~name ?(params = []) () : subscription =
  let ready =
    match Hashtbl.find_opt _pubs name with
    | None -> false (* no SSR publication registered — the browser fills in after hydration *)
    | Some f -> (
      match (try Some (f params) with _ -> None) with
      | None -> false (* the fetch couldn't run synchronously (e.g. real mongo) — degrade to loading *)
      | Some docs ->
        let collection = Subkey.collection_of_name name in
        MS.seed (Live.store t.live) ~sub:(Subkey.key name params) ~collection docs;
        (* embed for the browser's flicker-free hydration — rides Fur's seed <script> *)
        Fur.Data.put_seed (seed_key name params) (BJ.to_string (Bson.Array docs));
        true)
  in
  { ready = Fur.signal ready; stop = (fun () -> ()) }

let use_subscribe t ~name ?(params = []) () = (subscribe t ~name ~params ()).ready
let call _ ~name ?(params = []) () = ignore (name, params)
let find t = Live.find t.live
