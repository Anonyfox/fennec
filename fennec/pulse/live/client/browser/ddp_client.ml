(* The browser DDP client. Opens a WebSocket to the server's /websocket, sends [connect], and on
   each frame feeds the sub-tagged data deltas into a live merge store ({!Fennec_pulse_live}). [subscribe]
   dedups + refcounts by (name, params) and tracks readiness; [find] is the reactive Fur query over
   the merged collection; [call] invokes a server method. The data a method changes returns through
   the open subscription as a normal delta — no request/response plumbing. Browser-only. *)

open Js_of_ocaml
module Msg = Fennec_ddp.Message
module MS = Fennec_pulse_live.Merge_store
module Live = Fennec_pulse_live.Live
module Subkey = Fennec_pulse_live.Subkey
module Seed = Fennec_pulse_live.Seed

(* the key the SSR embedded this subscription's hydration docs under (Fur's seed table) *)
let seed_key name params = "ddp:" ^ Subkey.key name params

(* one deduped subscription: identical (name, params) share this state (and its [ready] signal),
   refcounted so the server sub is torn down only when the last holder stops *)
type sub_state = { id : string; mutable refcount : int; ready_sig : bool Fur.signal }
type subscription = { ready : bool Fur.signal; stop : unit -> unit }

type t = {
  live : Live.t;
  send : string -> unit;
  subs : (string, sub_state) Hashtbl.t; (* Subkey.key (name,params) → state *)
  by_id : (string, sub_state) Hashtbl.t; (* sub id → state, for ready/nosub *)
  mutable subc : int;
  mutable methodc : int;
}

let mark_ready t id = match Hashtbl.find_opt t.by_id id with Some st -> Fur.set st.ready_sig true | None -> ()

(* route one decoded message: data deltas → merge store; ready/nosub → flip readiness; ping → pong *)
let handle t raw =
  match try Some (Msg.decode raw) with _ -> None with
  | None -> ()
  | Some m ->
      let box = Live.store t.live in
      (* data deltas (incl. the ordered addedBefore/movedBefore) route through the shared, native-
         tested Wire_route; control frames are this client's concern (they touch subscription state) *)
      if not (Fennec_pulse_live.Wire_route.apply_delta box m) then
        (match m with
        | Msg.Ready { subs } -> List.iter (mark_ready t) subs
        | Msg.Nosub { id; _ } -> mark_ready t id (* the sub ended/failed — stop "loading" rather than hang *)
        | Msg.Ping { id } -> t.send (Msg.encode (Msg.Pong { id }))
        | _ -> ())

let connect ?(path = "/websocket") () : t =
  let loc = Js.Unsafe.get Dom_html.window (Js.string "location") in
  let protocol = Js.to_string (Js.Unsafe.get loc (Js.string "protocol")) in
  let host = Js.to_string (Js.Unsafe.get loc (Js.string "host")) in
  let scheme = if protocol = "https:" then "wss://" else "ws://" in
  let url = scheme ^ host ^ path in
  let ws = Js.Unsafe.new_obj (Js.Unsafe.pure_js_expr "WebSocket") [| Js.Unsafe.inject (Js.string url) |] in
  let raw str = ignore (Js.Unsafe.meth_call ws "send" [| Js.Unsafe.inject (Js.string str) |]) in
  (* queue sends until the socket is open — subscribe/call can fire (component setup) before [onopen] *)
  let is_open = ref false in
  let pending = Queue.create () in
  let send str = if !is_open then raw str else Queue.add str pending in
  let t = { live = Live.create (); send; subs = Hashtbl.create 16; by_id = Hashtbl.create 16; subc = 0; methodc = 0 } in
  Js.Unsafe.set ws (Js.string "onopen")
    (Dom.handler (fun _ ->
         is_open := true;
         raw (Msg.encode (Msg.Connect { session = None; version = "1"; support = [ "1" ] }));
         Queue.iter raw pending;
         Queue.clear pending;
         Js._true));
  Js.Unsafe.set ws (Js.string "onmessage")
    (Dom.handler (fun ev ->
         handle t (Js.to_string (Js.Unsafe.get ev (Js.string "data")));
         Js._true));
  t

let subscribe t ~name ?(params = []) () : subscription =
  let key = Subkey.key name params in
  let st =
    match Hashtbl.find_opt t.subs key with
    | Some st -> st.refcount <- st.refcount + 1; st
    | None ->
        t.subc <- t.subc + 1;
        let st = { id = "s" ^ string_of_int t.subc; refcount = 1; ready_sig = Fur.signal false } in
        Hashtbl.replace t.subs key st;
        Hashtbl.replace t.by_id st.id st;
        (* flicker-free hydration: if the SSR embedded this sub's docs (Fur's seed table), install
           them under this sub id and mark ready BEFORE sending the live Sub — so the first paint
           matches the server HTML; the live Sub then re-confirms + streams deltas under the same id. *)
        (match Hashtbl.find_opt Fur.Data.seed (seed_key name params) with
        | Some payload ->
          (* install under the collection the SERVER declared (it rides in the payload) — not a
             client-side re-derivation, so a publication whose name ≠ collection still hydrates right *)
          (match Seed.decode payload with
          | Some (collection, docs) -> (try MS.seed (Live.store t.live) ~sub:st.id ~collection docs with _ -> ())
          | None -> ());
          Fur.set st.ready_sig true
        | None -> ());
        t.send (Msg.encode (Msg.Sub { id = st.id; name; params }));
        st
  in
  let stopped = ref false in
  let stop () =
    if not !stopped then begin
      stopped := true;
      st.refcount <- st.refcount - 1;
      if st.refcount <= 0 then begin
        Hashtbl.remove t.subs key;
        Hashtbl.remove t.by_id st.id;
        t.send (Msg.encode (Msg.Unsub { id = st.id }));
        MS.sub_stopped (Live.store t.live) st.id
      end
    end
  in
  { ready = st.ready_sig; stop }

let use_subscribe t ~name ?(params = []) () : bool Fur.signal =
  let sub = subscribe t ~name ~params () in
  Fur.on_cleanup sub.stop;
  sub.ready

let call t ~name ?(params = []) () =
  t.methodc <- t.methodc + 1;
  t.send (Msg.encode (Msg.Method { method_ = name; params; id = "m" ^ string_of_int t.methodc; random_seed = None }))

(* SSR-only concept: the browser receives data over the live socket, not a publication registry *)
let publish ~name ?collection (_ : Bson.t list -> Bson.t list) = ignore (name, collection)

let find t = Live.find t.live
let aggregate t = Live.aggregate t.live
