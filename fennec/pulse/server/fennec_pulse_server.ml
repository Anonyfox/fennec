(* DDP-over-WebSocket server wiring. [Make (R)] over a reactive instance produces the glue that runs
   a DDP session on a live websocket channel: it bridges the session's push [sink] straight to
   [R.run_publication]'s observe deltas (no merge box, no polling), and routes methods through
   [R.call] (translating a reactive [Error] into the DDP error payload). *)

module Rx = Fennec_pulse.Reactive
module Msg = Fennec_ddp.Message
module Session = Fennec_ddp.Session
module Sockjs = Fennec_ddp.Sockjs
module Ws = Fennec_core.Ws_channel

module Make (R : Fennec_pulse.Reactive.REACTIVE) = struct
  (* a publication in the session's sink-fed form, backed by the delta-driven run_publication *)
  let publication_of name : Session.publication =
   fun ~params sink ->
    let h =
      R.run_publication name ~params ~on:(function
        | Rx.Added { collection; id; fields } -> sink.Session.added ~collection ~id ~fields
        | Rx.Changed { collection; id; fields; cleared } ->
            sink.Session.changed ~collection ~id ~fields ~cleared
        | Rx.Removed { collection; id } -> sink.Session.removed ~collection ~id)
    in
    (* observe_changes replayed existing docs as [added] during run_publication; signal ready now *)
    sink.Session.ready ();
    { Session.stop = h.Rx.stop }

  (* ---- seeded id minting (latency compensation) ------------------------------------------------
     The client's randomSeed must drive the handler's insert ids so they CONVERGE with its stub's.
     The seam is fiber-local (Eio's equivalent of AsyncLocalStorage): [with_seed] binds the call's
     per-collection streams for the handler's dynamic extent — concurrent methods on other fibers and
     domains each see their own. Outside an Eio run (unit tests) the fiber effect is unhandled, so a
     plain global stands in (single-fiber there — the DD7 pattern). *)
  module Mth = Fennec_pulse_method

  type _seed_streams = string * (string, int -> int) Hashtbl.t (* seed + per-collection streams *)

  let _seed_key : _seed_streams Eio.Fiber.key = Eio.Fiber.create_key ()
  let _seed_fallback : _seed_streams option ref = ref None

  let _current_streams () =
    match (try Eio.Fiber.get _seed_key with Stdlib.Effect.Unhandled _ -> None) with
    | Some v -> Some v
    | None -> !_seed_fallback

  let () =
    R.set_seeded_id_provider (fun coll ->
        match _current_streams () with
        | None -> None
        | Some (seed, tbl) ->
            Some
              (match Hashtbl.find_opt tbl coll with
              | Some r -> r
              | None ->
                  let r = Mth.Method.Seed.stream ~seed ~scope:coll in
                  Hashtbl.replace tbl coll r;
                  r))

  let with_seed seed f =
    let v = (seed, Hashtbl.create 4) in
    (* probe whether a fiber context exists BEFORE running f, so f executes exactly once *)
    match try `Fiber (ignore (Eio.Fiber.get _seed_key : _seed_streams option)) with Stdlib.Effect.Unhandled _ -> `Plain with
    | `Fiber () -> Eio.Fiber.with_binding _seed_key v f
    | `Plain ->
        _seed_fallback := Some v;
        Fun.protect f ~finally:(fun () -> _seed_fallback := None)

  (* a method, translating a reactive Error into the session's Method_error so its code/reason reach
     the client instead of being collapsed to a generic 500. The session's per-call context threads
     through: the connection's user reaches the handler's invocation, a login method's set_user_id
     rebinds the connection, and a randomSeed binds the seeded id streams for the handler's extent. *)
  let method_of name : Session.method_fn =
   fun ctx params ->
    let run () =
      try R.apply ~user_id:ctx.Session.user_id ~set_user_id:ctx.Session.set_user_id name params
      with R.Error { code; reason } -> raise (Session.Method_error { code; reason })
    in
    match ctx.Session.random_seed with
    | Some (Bson.String seed) when seed <> "" -> with_seed seed run
    | _ -> run ()

  (* the publication/method registries for a session — the names are fixed at registration time *)
  (* a per-session snapshot of the publication/method names (wrapped into session sinks). Rebuilt per
     connection — cheap (O(pubs+methods), all reads of the global tables) and CORRECT even if
     publications are registered after the first session connects; memoizing it would capture a stale
     snapshot and silently miss later registrations. The session's own mutable state is just its subs. *)
  let registries () =
    let pubs = Hashtbl.create 16 and methods = Hashtbl.create 16 in
    List.iter (fun n -> Hashtbl.replace pubs n (publication_of n)) (R.publications ());
    List.iter (fun n -> Hashtbl.replace methods n (method_of n)) (R.method_names ());
    (pubs, methods)

  let new_session ~session_id ~emit =
    let pubs, methods = registries () in
    (* the write fence: a method's [updated] is emitted only after R.fence reports every committed
       delta delivered — the client may then safely reveal server truth over its simulation *)
    Session.create ~fence:R.fence ~session_id ~emit ~pubs ~methods ()

  let gen_session_id = function Some s -> s | None -> R.ObjectID.make ()

  (* raw /websocket: exactly one DDP JSON message per text frame *)
  let serve ?session_id (ch : Ws.t) : unit =
    let session = new_session ~session_id:(gen_session_id session_id) ~emit:(fun m -> ch.Ws.send (Msg.encode m)) in
    (* the DECODE is broadly guarded — a malformed frame is dropped, the connection kept. DISPATCH is
       NOT guarded here: it already handles app-level failures internally (a method exception → a 500
       Result, a failing publication → Nosub), so anything that still escapes it is a genuine
       transport/logic bug and should surface (close the connection) rather than vanish silently. *)
    ch.Ws.on_text <- (fun raw -> match (try Some (Msg.decode raw) with _ -> None) with None -> () | Some m -> Session.dispatch session m);
    ch.Ws.on_close <- (fun () -> Session.close session)

  (* /sockjs: DDP messages are wrapped in SockJS array frames (for the stock Meteor browser client) *)
  let serve_sockjs ?session_id (ch : Ws.t) : unit =
    let session =
      new_session ~session_id:(gen_session_id session_id) ~emit:(fun m -> ch.Ws.send (Sockjs.wrap [ Msg.encode m ]))
    in
    ch.Ws.send Sockjs.open_frame;
    ch.Ws.on_text <-
      (fun frame ->
        List.iter
          (fun raw -> match (try Some (Msg.decode raw) with _ -> None) with None -> () | Some m -> Session.dispatch session m)
          (Sockjs.unwrap frame));
    ch.Ws.on_close <- (fun () -> Session.close session)

  let paw ?(path = "/websocket") () = Fennec_server.Websocket.make path (fun ch -> serve ch)
end
