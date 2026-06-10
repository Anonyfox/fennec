(* The browser DDP client. Opens a WebSocket to the server's /websocket, runs the DDP handshake, and
   feeds sub-tagged data deltas into a live merge store ({!Fennec_pulse_live}). [subscribe] dedups +
   refcounts by (name, params) and tracks readiness; [find]/[aggregate] are the reactive Fur queries
   over the merged cache; [call] invokes a server method (its data change returns via the open
   subscription as a normal delta).

   OFFLINE BY DEFAULT: a network drop degrades gracefully with no userland code. [find] keeps
   rendering the cache; optimistic stubs keep applying instantly; method calls buffer (the unacked
   list) in order. NOTHING ELSE queues — the server session dies with the socket, so the reconnect
   handshake REBUILDS everything from client state: Connect (session resume) → resubscribe every
   live subscription (with resync_begin, so the fresh snapshots + [ready] quiescence heal the cache)
   → re-send the buffered methods verbatim, oldest first (at-least-once). A HEARTBEAT (DDP ping every
   15s, 10s pong deadline) detects silent network death (wifi gone, no FIN) and force-closes the
   socket so the reconnect loop takes over; [status]/[pending_writes] are Fur signals for offline
   affordances. With [?persist] (the PWA tiers) the cache AND the write outbox survive reloads:
   snapshots restore as seeds (tentative → quiesced by the next live ready), the outbox re-issues
   with fresh ids + original seeds, and stubs replay byte-identically. Browser-only. *)

open Js_of_ocaml
module Msg = Fennec_ddp.Message
module MS = Fennec_pulse_live.Merge_store
module Live = Fennec_pulse_live.Live
module Subkey = Fennec_pulse_live.Subkey
module Seed = Fennec_pulse_live.Seed
module Outbox = Fennec_pulse_live.Outbox

(* the key the SSR embedded this subscription's hydration docs under (Fur's seed table) *)
let seed_key name params = "ddp:" ^ Subkey.key name params

let set_timeout cb ms =
  ignore
    (Js.Unsafe.fun_call (Js.Unsafe.pure_js_expr "setTimeout")
       [| Js.Unsafe.inject (Js.wrap_callback cb); Js.Unsafe.inject ms |])

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
  subs : (string, sub_state) Hashtbl.t; (* Subkey.key (name,params) → state *)
  by_id : (string, sub_state) Hashtbl.t; (* sub id → state, for ready/nosub *)
  mutable subc : int;
  mutable methodc : int;
  mutable session_id : string option; (* captured from Connected; replayed for DDP session resume *)
  (* in-flight method calls awaiting their Result: id → the resolver invoked with Ok value /
     Error (code, reason) (the typed path decodes inside its resolver) *)
  methods_pending : (string, (Bson.t, string * string) result -> unit) Hashtbl.t;
  (* UNACKNOWLEDGED method frames (newest first), re-sent verbatim after a reconnect — Meteor's
     at-least-once semantics: a method lost to a dropped socket re-executes rather than dangling
     forever (same id, same seed; write idempotency is the app's concern, exactly as in Meteor).
     Method frames bypass the pre-open queue and ride ONLY this list, so they can't double-send. *)
  mutable unacked : (string * string) list;
  (* THE one send primitive: raw socket send when open, silently dropped otherwise — every frame
     category is rebuilt by the reconnect handshake (subs from t.subs, methods from unacked), so
     queueing frames offline would only produce duplicates *)
  mutable send_if_open : string -> unit;
  (* running optimistic simulations: method id → its sim sub; dropped (server truth revealed) when
     the method's [updated] arrives *)
  sims : (string, string) Hashtbl.t;
  status_sig : [ `Connected | `Connecting | `Waiting ] Fur.signal; (* for offline affordances *)
  pending_sig : int Fur.signal; (* buffered/unacknowledged method count ("saving… N pending") *)
  mutable awaiting_pong : bool; (* heartbeat: set on ping, cleared by ANY incoming frame *)
  (* PWA persistence (tier 2+3): the storage namespace, None = off. Sub snapshots persist (debounced
     + on ready) under "sub:<key>"; the write outbox under "outbox" (mid-paired in memory, codec'd
     without mids — fresh ids remint on restore). *)
  persist : string option;
  mutable outbox : (string * Outbox.entry) list; (* (mid, entry), newest first; mirrors unacked *)
  mutable persist_scheduled : bool; (* the snapshot debounce flag *)
  mutable closed : bool; (* set by [close]: stops the reconnect loop (and lets a fired timer bail) *)
  mutable ws : Js.Unsafe.any option; (* the live socket, so [close] can shut it *)
}

(* the outbox storage key is TAB-scoped (a sessionStorage-persisted suffix): two tabs sharing one
   persist namespace must not clobber or double-execute each other's pending writes — sessionStorage
   is per-tab and survives that tab's reloads, exactly the right lifetime *)
let outbox_key () =
  let ss k = try
      Js.Optdef.case (Dom_html.window##.sessionStorage) (fun () -> None)
        (fun st -> Js.Opt.case (st##getItem (Js.string k)) (fun () -> None) (fun v -> Some (Js.to_string v)))
    with _ -> None
  in
  match ss "fennec:tab" with
  | Some id -> "outbox:" ^ id
  | None ->
      let id = string_of_int (int_of_float (Js.to_float (Js.Unsafe.eval_string "Math.random()*1e9"))) in
      (try Js.Optdef.iter (Dom_html.window##.sessionStorage) (fun st ->
           st##setItem (Js.string "fennec:tab") (Js.string id))
       with _ -> ());
      "outbox:" ^ id

(* persist the outbox NOW (writes are precious; no debounce) *)
let persist_outbox t =
  match t.persist with
  | None -> ()
  | Some ns -> Kv.put ~ns (outbox_key ()) (Outbox.encode (List.rev_map snd t.outbox))

(* persist every live sub's snapshot (the SEED format — restores via the normal seed path) *)
let persist_subs t =
  match t.persist with
  | None -> ()
  | Some ns ->
      Hashtbl.iter
        (fun key (st : sub_state) ->
          let groups = MS.snapshot_sub (Live.store t.live) ~sub:st.id in
          Kv.put ~ns ("sub:" ^ key) (Seed.encode groups))
        t.subs

(* debounced: data deltas arrive in bursts; one snapshot a second is plenty *)
let schedule_persist t =
  if t.persist <> None && not t.persist_scheduled then begin
    t.persist_scheduled <- true;
    set_timeout
      (fun () ->
        t.persist_scheduled <- false;
        if not t.closed then persist_subs t)
      1_000
  end

let mark_ready t id = match Hashtbl.find_opt t.by_id id with Some st -> Fur.set st.ready_sig true | None -> ()

(* route one decoded message: data deltas → merge store (via the shared Wire_route); control frames
   are this client's concern (they touch session/subscription state) *)
let handle t raw =
  t.awaiting_pong <- false;
  (* any inbound traffic proves the link is alive *)
  match try Some (Msg.decode raw) with _ -> None with
  | None -> Firebug.console##warn (Js.string ("fennec/ddp: dropped an undecodable frame: " ^ raw))
  | Some m ->
      let box = Live.store t.live in
      if Fennec_pulse_live.Wire_route.apply_delta box m then schedule_persist t
      else
        match m with
        | Msg.Connected { session } -> t.session_id <- Some session (* for session resume on reconnect *)
        | Msg.Ready { subs } ->
            (* quiescence: drop seeded/stale docs the live snapshot didn't re-confirm, then mark ready *)
            List.iter (fun id -> MS.quiesce box id; mark_ready t id) subs;
            persist_subs t (* a clean truth point: snapshot immediately, not debounced *)
        | Msg.Nosub { id; error } ->
            (* a FAILED sub (error=Some) — GC the docs it contributed so they don't linger; an
               error=None nosub is the server's Unsub ack (already cleaned on stop). Either way, stop
               "loading" rather than hang. *)
            (match error with Some _ -> MS.sub_stopped box id | None -> ());
            mark_ready t id
        | Msg.Ping { id } -> t.send_if_open (Msg.encode (Msg.Pong { id }))
        | Msg.Result { id; error; result } ->
            (* acknowledged: never re-sent, and resolve the awaiting caller (call_result/call_m) *)
            t.unacked <- List.filter (fun (i, _) -> i <> id) t.unacked;
            t.outbox <- List.filter (fun (i, _) -> i <> id) t.outbox;
            persist_outbox t;
            Fur.set t.pending_sig (List.length t.unacked);
            (match Hashtbl.find_opt t.methods_pending id with
            | Some resolve ->
                Hashtbl.remove t.methods_pending id;
                resolve
                  (match error with
                  | Some e -> Error (e.Msg.code, Option.value e.Msg.reason ~default:"")
                  | None -> Ok (Option.value result ~default:Bson.Null))
            | None -> ())
        | Msg.Updated { methods } ->
            (* the server's write fence has passed: its data deltas for these methods have ARRIVED,
               so reveal server truth — dropping each method's simulation is one sub_stopped (the
               merge store's precedence fallthrough IS the rollback) *)
            List.iter
              (fun mid ->
                match Hashtbl.find_opt t.sims mid with
                | Some simid ->
                    MS.sub_stopped box simid;
                    Hashtbl.remove t.sims mid
                | None -> ())
              methods
        | Msg.Failed _ -> () (* DDP version mismatch — nothing to renegotiate; proceed best-effort *)
        | _ -> ()

(* the one method-send path: mint the id, register the resolver, record the frame as unacknowledged
   (the reconnect path re-sends it verbatim), and put it on the wire if the socket is open — methods
   never ride the pre-open queue, so a frame can't go out twice *)
let send_method t ~name ~params ~random_seed (resolve : (Bson.t, string * string) result -> unit) :
    string =
  t.methodc <- t.methodc + 1;
  let id = "m" ^ string_of_int t.methodc in
  let frame = Msg.encode (Msg.Method { method_ = name; params; id; random_seed }) in
  Hashtbl.replace t.methods_pending id resolve;
  t.unacked <- (id, frame) :: t.unacked;
  (* the persistent outbox (PWA): writes survive a reload as (name, params, seed) *)
  (if t.persist <> None then begin
     let seed = match random_seed with Some (Bson.String s) -> Some s | _ -> None in
     t.outbox <- (id, { Outbox.name; params; seed }) :: t.outbox;
     persist_outbox t
   end);
  Fur.set t.pending_sig (List.length t.unacked);
  t.send_if_open frame;
  id

let connect ?(path = "/websocket") ?persist () : t =
  let loc = Js.Unsafe.get Dom_html.window (Js.string "location") in
  let protocol = Js.to_string (Js.Unsafe.get loc (Js.string "protocol")) in
  let host = Js.to_string (Js.Unsafe.get loc (Js.string "host")) in
  let scheme = if protocol = "https:" then "wss://" else "ws://" in
  let url = scheme ^ host ^ path in
  let t =
    { live = Live.create (); subs = Hashtbl.create 16; by_id = Hashtbl.create 16;
      subc = 0; methodc = 0; session_id = None; methods_pending = Hashtbl.create 16; unacked = [];
      send_if_open = (fun _ -> ()); sims = Hashtbl.create 8; status_sig = Fur.signal `Connecting;
      pending_sig = Fur.signal 0; awaiting_pong = false; persist; outbox = []; persist_scheduled = false;
      closed = false; ws = None }
  in
  (* PWA outbox restore (tier 3): every write that survived the reload re-issues with a FRESH id
     and its ORIGINAL seed, and its stub re-runs (Method.stub_replay) — the deterministic seed
     streams remint identical optimistic ids, so the rows reappear exactly as they were, BEFORE the
     socket even opens. Results are fire-and-forget post-reload (the resolvers died with the page). *)
  (match persist with
  | None -> ()
  | Some ns ->
      let entries = Outbox.decode (Option.value (Kv.get ~ns (outbox_key ())) ~default:"") in
      Kv.del ~ns (outbox_key ());
      List.iter
        (fun (e : Outbox.entry) ->
          let mid =
            send_method t ~name:e.Outbox.name ~params:e.Outbox.params
              ~random_seed:(Option.map (fun x -> Bson.String x) e.Outbox.seed)
              (fun _ -> ())
          in
          match e.Outbox.seed with
          | Some seed -> (
              match Method.stub_replay e.Outbox.name with
              | Some replay ->
                  let simid = "sim:" ^ mid in
                  (try replay e.Outbox.params (Fennec_pulse_live.Sim.writes (Live.store t.live) ~sim:simid ~seed)
                   with _ -> ());
                  Hashtbl.replace t.sims mid simid
              | None -> ())
          | None -> ())
        entries);
  (* NO offline frame queue, deliberately: the server session dies with the socket, so every frame
     category is rebuilt from client state on reconnect (subs from t.subs, methods from unacked) —
     queueing raw frames would only double-send. [send_if_open] silently drops when not open. *)
  let is_open = ref false in
  let backoff_ms = ref 500 in (* exponential, capped at 30s; reset on a clean open *)
  let set_timeout cb ms =
    ignore
      (Js.Unsafe.fun_call (Js.Unsafe.pure_js_expr "setTimeout")
         [| Js.Unsafe.inject (Js.wrap_callback cb); Js.Unsafe.inject ms |])
  in
  let force_close () =
    match t.ws with Some ws -> (try ignore (Js.Unsafe.meth_call ws "close" [||]) with _ -> ()) | None -> ()
  in
  let rec open_socket () =
    is_open := false;
    Fur.set t.status_sig `Connecting;
    let ws = Js.Unsafe.new_obj (Js.Unsafe.pure_js_expr "WebSocket") [| Js.Unsafe.inject (Js.string url) |] in
    t.ws <- Some (Js.Unsafe.inject ws);
    let raw str = ignore (Js.Unsafe.meth_call ws "send" [| Js.Unsafe.inject (Js.string str) |]) in
    t.send_if_open <- (fun str -> if !is_open then raw str);
    Js.Unsafe.set ws (Js.string "onopen")
      (Dom.handler (fun _ ->
           is_open := true;
           backoff_ms := 500; (* connected — reset the backoff *)
           Fur.set t.status_sig `Connected;
           raw (Msg.encode (Msg.Connect { session = t.session_id; version = "1"; support = [ "1" ] }));
           (* resubscribe + resync every live subscription: re-mark its docs tentative so the fresh
              snapshot re-confirms survivors and the [ready] quiesce drops what the server dropped *)
           Hashtbl.iter
             (fun _ (st : sub_state) ->
               MS.resync_begin (Live.store t.live) st.id;
               raw (Msg.encode (Msg.Sub { id = st.id; name = st.name; params = st.params })))
             t.subs;
           (* flush the offline buffer: every unacknowledged method, oldest first, verbatim *)
           List.iter (fun (_, frame) -> raw frame) (List.rev t.unacked);
           Js._true));
    Js.Unsafe.set ws (Js.string "onmessage")
      (Dom.handler (fun ev ->
           handle t (Js.to_string (Js.Unsafe.get ev (Js.string "data")));
           Js._true));
    (* a dropped socket → reconnect with backoff (the browser fires onclose after an error too) *)
    Js.Unsafe.set ws (Js.string "onclose")
      (Dom.handler (fun _ ->
           is_open := false;
           Fur.set t.status_sig `Waiting;
           (* don't reconnect once [close] was called; a timer scheduled just before [close] also
              re-checks the flag when it fires, so a finished client never reopens a socket *)
           if not t.closed then begin
             let delay = !backoff_ms in
             backoff_ms := min (delay * 2) 30_000;
             set_timeout (fun () -> if not t.closed then open_socket ()) delay
           end;
           Js._true))
  in
  open_socket ();
  (* HEARTBEAT: silent network death (wifi gone — no FIN, no onclose) would leave the client
     believing it is online for minutes. A DDP ping every 15s with a 10s deadline detects it: any
     inbound frame counts as life ([handle] clears the flag); a missed deadline force-closes the
     socket, and the normal reconnect loop (with its resubscribe + buffered-method flush) heals. *)
  let rec heartbeat () =
    if not t.closed then begin
      (if !is_open then begin
         t.awaiting_pong <- true;
         t.send_if_open (Msg.encode (Msg.Ping { id = None }));
         set_timeout
           (fun () -> if (not t.closed) && !is_open && t.awaiting_pong then force_close ())
           10_000
       end);
      set_timeout heartbeat 15_000
    end
  in
  set_timeout heartbeat 15_000;
  t

(* tear the client down: stop reconnecting, drop any running simulations (their methods can never
   resolve now), and shut the live socket. After this the client is inert. Idempotent. *)
let close t =
  t.closed <- true;
  Hashtbl.iter (fun _ simid -> MS.sub_stopped (Live.store t.live) simid) t.sims;
  Hashtbl.reset t.sims;
  match t.ws with Some ws -> (try ignore (Js.Unsafe.meth_call ws "close" [||]) with _ -> ()) | None -> ()

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
        | None -> (
            (* PWA warm boot (tier 2): restore the last persisted snapshot as a seed — tentative
               until the next live [ready] re-confirms; quiescence prunes what died while away *)
            match t.persist with
            | None -> ()
            | Some ns -> (
                match Kv.get ~ns ("sub:" ^ key) with
                | None -> ()
                | Some payload ->
                    List.iter
                      (fun (collection, docs) ->
                        try MS.seed (Live.store t.live) ~sub:st.id ~collection docs with _ -> ())
                      (Seed.decode payload);
                    Fur.set st.ready_sig true)));
        (* offline: dropped silently — the onopen handshake re-sends every live sub from t.subs *)
        t.send_if_open (Msg.encode (Msg.Sub { id = st.id; name; params }));
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
        (match t.persist with Some ns -> Kv.del ~ns ("sub:" ^ key) | None -> ());
        (* offline: nothing to unsub — the server session died with the socket, and the onopen
           handshake only re-sends subs still in t.subs (this one just left) *)
        t.send_if_open (Msg.encode (Msg.Unsub { id = st.id }));
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
   in flight, then [Some (Ok value)] or [Some (Error (code, reason))] (e.g. a method's 403). *)
let call_result t ~name ?(params = []) () : (Bson.t, string * string) result option Fur.signal =
  let sig_ = Fur.signal None in
  ignore (send_method t ~name ~params ~random_seed:None (fun r -> Fur.set sig_ (Some r)));
  sig_

(* fire-and-forget: the data a method changes returns via the open subscription as a normal delta *)
let call t ~name ?(params = []) () = ignore (call_result t ~name ~params ())

(* the TYPED call: arguments encode through the shared method value's codec; the outcome decodes
   before it reaches the signal (a result the codec rejects surfaces as a client-decode error).
   A declared [?stub] runs IMMEDIATELY as an optimistic simulation against the client cache — the
   UI updates now; the server's [updated] reveals truth (Updated handler above). The call carries a
   randomSeed so the server's seeded handler mints the SAME insert ids as the stub (convergence). *)
let call_m t (m : ('a, 'r) Method.t) (a : 'a) : ('r, string * string) result option Fur.signal =
  let sig_ = Fur.signal None in
  let resolve r =
    Fur.set sig_
      (Some
         (match r with
         | Error e -> Error e
         | Ok v -> (
             match (Method.result m).Codec.dec v with
             | Ok r -> Ok r
             | Error e -> Error ("client-decode", e))))
  in
  let params = (Method.args m).Codec.enc_args a in
  (match Method.stub m with
  | None -> ignore (send_method t ~name:(Method.name m) ~params ~random_seed:None resolve)
  | Some stub ->
      let seed = Query.Id.random_id () in
      let mid =
        send_method t ~name:(Method.name m) ~params ~random_seed:(Some (Bson.String seed)) resolve
      in
      let simid = "sim:" ^ mid in
      (* a throwing stub is logged and skipped — the call still went to the server (truth) *)
      (try stub (Fennec_pulse_live.Sim.writes (Live.store t.live) ~sim:simid ~seed) a
       with e ->
         Firebug.console##warn
           (Js.string ("fennec/method: stub failed (simulation skipped): " ^ Printexc.to_string e)));
      Hashtbl.replace t.sims mid simid);
  sig_

(* SSR-only concept: the browser receives data over the live socket, not a publication registry *)
let publish ~name (_ : Bson.t list -> (string * Bson.t list) list) = ignore name

let find t = Live.find t.live
let aggregate t = Live.aggregate t.live

(* offline affordances: the connection state and the buffered-write count, as Fur signals *)
let status t = t.status_sig
let pending_writes t = t.pending_sig

(* wipe this client's persisted namespace (snapshots + outbox) — the identity-change hook: call it
   on logout/user-switch so one user's cache never leaks to the next *)
let purge_storage t = match t.persist with Some ns -> Kv.purge ~ns | None -> ()
