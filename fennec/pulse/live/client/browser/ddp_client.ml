(* The browser DDP client. Opens a WebSocket to the server's /websocket, runs the DDP handshake, and
   feeds sub-tagged data deltas into a live merge store ({!Fennec_pulse_live}). [subscribe] dedups +
   refcounts by (name, params) and tracks readiness; [find]/[aggregate] are the reactive Fur queries
   over the merged cache; [call] invokes a server method (its data change returns via the open
   subscription as a normal delta). RESILIENT: on a dropped socket it reconnects with exponential
   backoff, replays the handshake (with the saved session id), resubscribes every live subscription,
   and RESYNCS the cache — the resubscription's fresh snapshot re-confirms surviving docs and the
   [ready] quiescence drops whatever the server stopped sending during the outage. Browser-only. *)

open Js_of_ocaml
module Msg = Fennec_ddp.Message
module MS = Fennec_pulse_live.Merge_store
module Live = Fennec_pulse_live.Live
module Subkey = Fennec_pulse_live.Subkey
module Seed = Fennec_pulse_live.Seed

(* the key the SSR embedded this subscription's hydration docs under (Fur's seed table) *)
let seed_key name params = "ddp:" ^ Subkey.key name params

(* one deduped subscription: identical (name, params) share this state (and its [ready] signal),
   refcounted so the server sub is torn down only when the last holder stops. [name]/[params] are
   kept so the subscription can be replayed verbatim on reconnect. *)
type sub_state = {
  id : string;
  name : string;
  params : Bson.t list;
  mutable refcount : int;
  ready_sig : bool Fur.signal;
}

type subscription = { ready : bool Fur.signal; stop : unit -> unit }

type t = {
  live : Live.t;
  mutable send : string -> unit; (* rebound to the live socket on each (re)connect *)
  subs : (string, sub_state) Hashtbl.t; (* Subkey.key (name,params) → state *)
  by_id : (string, sub_state) Hashtbl.t; (* sub id → state, for ready/nosub *)
  mutable subc : int;
  mutable methodc : int;
  mutable session_id : string option; (* captured from Connected; replayed for DDP session resume *)
  (* in-flight method calls awaiting their Result: id → a signal that resolves to Ok value /
     Error (code, reason). [None] = still pending. *)
  methods_pending : (string, (Bson.t, string * string) result option Fur.signal) Hashtbl.t;
}

let mark_ready t id = match Hashtbl.find_opt t.by_id id with Some st -> Fur.set st.ready_sig true | None -> ()

(* route one decoded message: data deltas → merge store (via the shared Wire_route); control frames
   are this client's concern (they touch session/subscription state) *)
let handle t raw =
  match try Some (Msg.decode raw) with _ -> None with
  | None -> ()
  | Some m ->
      let box = Live.store t.live in
      if not (Fennec_pulse_live.Wire_route.apply_delta box m) then
        match m with
        | Msg.Connected { session } -> t.session_id <- Some session (* for session resume on reconnect *)
        | Msg.Ready { subs } ->
            (* quiescence: drop seeded/stale docs the live snapshot didn't re-confirm, then mark ready *)
            List.iter (fun id -> MS.quiesce box id; mark_ready t id) subs
        | Msg.Nosub { id; error } ->
            (* a FAILED sub (error=Some) — GC the docs it contributed so they don't linger; an
               error=None nosub is the server's Unsub ack (already cleaned on stop). Either way, stop
               "loading" rather than hang. *)
            (match error with Some _ -> MS.sub_stopped box id | None -> ());
            mark_ready t id
        | Msg.Ping { id } -> t.send (Msg.encode (Msg.Pong { id }))
        | Msg.Result { id; error; result } ->
            (* resolve the call's result signal (if a caller is awaiting it via [call_result]) *)
            (match Hashtbl.find_opt t.methods_pending id with
            | Some sig_ ->
                Fur.set sig_
                  (Some
                     (match error with
                     | Some e -> Error (e.Msg.code, Option.value e.Msg.reason ~default:"")
                     | None -> Ok (Option.value result ~default:Bson.Null)));
                Hashtbl.remove t.methods_pending id
            | None -> ())
        | Msg.Updated _ -> () (* the server's write-fence; the data change already flows via the subscription *)
        | Msg.Failed _ -> () (* DDP version mismatch — nothing to renegotiate; proceed best-effort *)
        | _ -> ()

let connect ?(path = "/websocket") () : t =
  let loc = Js.Unsafe.get Dom_html.window (Js.string "location") in
  let protocol = Js.to_string (Js.Unsafe.get loc (Js.string "protocol")) in
  let host = Js.to_string (Js.Unsafe.get loc (Js.string "host")) in
  let scheme = if protocol = "https:" then "wss://" else "ws://" in
  let url = scheme ^ host ^ path in
  let t =
    { live = Live.create (); send = (fun _ -> ()); subs = Hashtbl.create 16; by_id = Hashtbl.create 16;
      subc = 0; methodc = 0; session_id = None; methods_pending = Hashtbl.create 16 }
  in
  (* sends before the socket is open (component setup fires subscribe/call before onopen) queue up *)
  let is_open = ref false in
  let pending = Queue.create () in
  let backoff_ms = ref 500 in (* exponential, capped at 30s; reset on a clean open *)
  let set_timeout cb ms =
    ignore
      (Js.Unsafe.fun_call (Js.Unsafe.pure_js_expr "setTimeout")
         [| Js.Unsafe.inject (Js.wrap_callback cb); Js.Unsafe.inject ms |])
  in
  let rec open_socket () =
    is_open := false;
    let ws = Js.Unsafe.new_obj (Js.Unsafe.pure_js_expr "WebSocket") [| Js.Unsafe.inject (Js.string url) |] in
    let raw str = ignore (Js.Unsafe.meth_call ws "send" [| Js.Unsafe.inject (Js.string str) |]) in
    t.send <- (fun str -> if !is_open then raw str else Queue.add str pending);
    Js.Unsafe.set ws (Js.string "onopen")
      (Dom.handler (fun _ ->
           is_open := true;
           backoff_ms := 500; (* connected — reset the backoff *)
           raw (Msg.encode (Msg.Connect { session = t.session_id; version = "1"; support = [ "1" ] }));
           (* resubscribe + resync every live subscription: re-mark its docs tentative so the fresh
              snapshot re-confirms survivors and the [ready] quiesce drops what the server dropped *)
           Hashtbl.iter
             (fun _ (st : sub_state) ->
               MS.resync_begin (Live.store t.live) st.id;
               raw (Msg.encode (Msg.Sub { id = st.id; name = st.name; params = st.params })))
             t.subs;
           Queue.iter raw pending;
           Queue.clear pending;
           Js._true));
    Js.Unsafe.set ws (Js.string "onmessage")
      (Dom.handler (fun ev ->
           handle t (Js.to_string (Js.Unsafe.get ev (Js.string "data")));
           Js._true));
    (* a dropped socket → reconnect with backoff (the browser fires onclose after an error too) *)
    Js.Unsafe.set ws (Js.string "onclose")
      (Dom.handler (fun _ ->
           is_open := false;
           let delay = !backoff_ms in
           backoff_ms := min (delay * 2) 30_000;
           set_timeout open_socket delay;
           Js._true))
  in
  open_socket ();
  t

let subscribe t ~name ?(params = []) () : subscription =
  let key = Subkey.key name params in
  let st =
    match Hashtbl.find_opt t.subs key with
    | Some st ->
        st.refcount <- st.refcount + 1;
        st
    | None ->
        t.subc <- t.subc + 1;
        let st = { id = "s" ^ string_of_int t.subc; name; params; refcount = 1; ready_sig = Fur.signal false } in
        Hashtbl.replace t.subs key st;
        Hashtbl.replace t.by_id st.id st;
        (* flicker-free hydration: if the SSR embedded this sub's docs (Fur's seed table), install
           them under this sub id and mark ready BEFORE sending the live Sub — so the first paint
           matches the server HTML; the live Sub then re-confirms + streams deltas under the same id. *)
        (match Hashtbl.find_opt (Fur.Data.seed_table ()) (seed_key name params) with
        | Some payload ->
          (* install each collection's docs under the collection the SERVER declared (they ride in the
             payload as {c;d} groups) — handles a publication that feeds one OR several collections *)
          List.iter
            (fun (collection, docs) -> try MS.seed (Live.store t.live) ~sub:st.id ~collection docs with _ -> ())
            (Seed.decode payload);
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

(* [call_result] invokes a method and returns a signal that resolves to its outcome — [None] while
   in flight, then [Some (Ok value)] or [Some (Error (code, reason))] (e.g. an allow/deny 403). *)
let call_result t ~name ?(params = []) () : (Bson.t, string * string) result option Fur.signal =
  t.methodc <- t.methodc + 1;
  let id = "m" ^ string_of_int t.methodc in
  let sig_ = Fur.signal None in
  Hashtbl.replace t.methods_pending id sig_;
  t.send (Msg.encode (Msg.Method { method_ = name; params; id; random_seed = None }));
  sig_

(* fire-and-forget: the data a method changes returns via the open subscription as a normal delta *)
let call t ~name ?(params = []) () = ignore (call_result t ~name ~params ())

(* SSR-only concept: the browser receives data over the live socket, not a publication registry *)
let publish ~name (_ : Bson.t list -> (string * Bson.t list) list) = ignore name

let find t = Live.find t.live
let aggregate t = Live.aggregate t.live
