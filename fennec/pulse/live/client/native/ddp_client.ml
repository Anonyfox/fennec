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
module Seed = Fennec_pulse_live.Seed

type t = { live : Live.t }
type subscription = { ready : bool Fur.signal; stop : unit -> unit }

(* SSR publication registry: name → its current documents. Populated on the server by [publish],
   read during the SSR render by [subscribe]. *)
let _pubs : (string, Bson.t list -> (string * Bson.t list) list) Hashtbl.t = Hashtbl.create 8
let publish ~name f = Hashtbl.replace _pubs name f
let seed_key name params = "ddp:" ^ Subkey.key name params

(* the ambient default — on the server (SSR) the "connection" is a seeded Live store; the per-model
   Collection views read through it exactly as the browser does, so component source is identical *)
let _default : t option ref = ref None
let default () = match !_default with Some t -> t | None -> failwith "Ddp_client: call connect before querying"

let connect ?path ?persist ?chrome () =
  ignore (path, persist, chrome);
  let t = { live = Live.create () } in
  _default := Some t;
  t

(* SSR has no socket / reconnect loop, so tearing down is a no-op *)
let close (_ : t) = ()

(* persistence is a browser concern; nothing to purge server-side *)
let purge_storage (_ : t) = ()

(* SSR assumes connectivity for the first paint (no offline banner flash server-side) *)
let status (_ : t) : [ `Connected | `Connecting | `Waiting ] Fur.signal = Fur.signal `Connected
let pending_writes (_ : t) : int Fur.signal = Fur.signal 0

let subscribe t ~name ?(params = []) () : subscription =
  let ready =
    match Hashtbl.find_opt _pubs name with
    | None -> false (* no SSR publication registered — the browser fills in after hydration *)
    | Some f -> (
      match (try Some (f params) with _ -> None) with
      | None -> false (* the fetch couldn't run synchronously (e.g. real mongo) — degrade to loading *)
      | Some groups ->
        let sub = Subkey.key name params in
        List.iter (fun (collection, docs) -> MS.seed (Live.store t.live) ~sub ~collection docs) groups;
        (* embed for the browser's flicker-free hydration — each collection's docs ride in the Seed
           payload as a {c;d} group, so the client installs each under its real collection *)
        Fur.Data.put_seed (seed_key name params) (Seed.encode groups);
        true)
  in
  { ready = Fur.signal ready; stop = (fun () -> ()) }

let use_subscribe t ~name ?(params = []) () = (subscribe t ~name ~params ()).ready
let call _ ~name ?(params = []) () = ignore (name, params)

(* SSR has no socket, so a method never resolves here — the signal stays [None] (pending) *)
let call_result _ ~name ?(params = []) () : (Bson.t, string * string) result option Fur.signal =
  ignore (name, params);
  Fur.signal None

(* typed twin: same SSR no-op — sends nothing, stays pending *)
let call_m _ (m : ('a, 'r) Method.t) (a : 'a) :
    ('r, string * string) result option Fur.signal =
  ignore (m, a);
  Fur.signal None
let find t = Live.find t.live
let find_c t = Live.find_c t.live
let find_p t def = Live.find_p t.live (Def.name def)
let aggregate t = Live.aggregate t.live

module Collection (M : sig
  type doc
  val collection : doc Def.t
end) = struct
  let find ?where ?sort ?skip ?limit () = find_c (default ()) M.collection ?where ?sort ?skip ?limit ()
  let project p ?where ?sort ?skip ?limit () = find_p (default ()) M.collection p ?where ?sort ?skip ?limit ()
end
