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

  (* a method, translating a reactive Error into the session's Method_error so its code/reason reach
     the client instead of being collapsed to a generic 500 *)
  let method_of name : Session.method_fn =
   fun params ->
    try R.call name params with R.Error { code; reason } -> raise (Session.Method_error { code; reason })

  (* the publication/method registries for a session — the names are fixed at registration time *)
  let registries () =
    let pubs = Hashtbl.create 16 and methods = Hashtbl.create 16 in
    List.iter (fun n -> Hashtbl.replace pubs n (publication_of n)) (R.publications ());
    List.iter (fun n -> Hashtbl.replace methods n (method_of n)) (R.method_names ());
    (pubs, methods)

  let new_session ~session_id ~emit =
    let pubs, methods = registries () in
    Session.create ~session_id ~emit ~pubs ~methods

  let gen_session_id = function Some s -> s | None -> R.ObjectID.make ()

  (* raw /websocket: exactly one DDP JSON message per text frame *)
  let serve ?session_id (ch : Ws.t) : unit =
    let session = new_session ~session_id:(gen_session_id session_id) ~emit:(fun m -> ch.Ws.send (Msg.encode m)) in
    ch.Ws.on_text <- (fun raw -> match Msg.decode raw with m -> Session.dispatch session m | exception _ -> ());
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
          (fun raw -> match Msg.decode raw with m -> Session.dispatch session m | exception _ -> ())
          (Sockjs.unwrap frame));
    ch.Ws.on_close <- (fun () -> Session.close session)

  let paw ?(path = "/websocket") () = Fennec_server.Websocket.make path (fun ch -> serve ch)
end
