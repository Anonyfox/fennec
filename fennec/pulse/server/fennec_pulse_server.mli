(** DDP-over-WebSocket server wiring: bridges a {!Fennec_pulse.Reactive} instance to a
    {!Fennec_ddp.Session} and serves it over a live {!Fennec_core.Ws_channel.t}. The bridge is
    delta-driven — publications feed the session's sink straight from [observe_changes] (no merge
    box, no polling) — and methods route through the reactive instance's [call], with a reactive
    [Error] mapped to the DDP error payload.

    Most apps reach this through the {!Fennec_pulse_app} facade ([serve_ddp]) rather than applying the
    functor by hand; the direct form is one [Make]-then-[paw]:

    {[ module Ddp = Fennec_pulse_server.Make (R)   (* R : Fennec_pulse.Reactive.REACTIVE *)

       (* the websocket paw, dropped into an endpoint's pipeline; seeds user id from native Accounts *)
       let ddp = Ddp.paw ~path:"/ddp" () ]} *)

(** [Make (R)] wires the reactive instance [R] to a DDP transport. *)
module Make (R : Fennec_pulse.Reactive.REACTIVE) : sig
  (** [serve ?user_id ?session_id ch] runs a DDP session on a raw [/websocket] channel: one DDP JSON
      message per text frame, decoded into the session and emitted back. Tears the session down on
      close. [session_id] defaults to a fresh ObjectId. [user_id] seeds the connection identity for
      tests/custom transports; the normal framework websocket paw derives it from native Accounts. *)
  val serve : ?user_id:string -> ?session_id:string -> Fennec_core.Ws_channel.t -> unit

  (** [serve_sockjs ?user_id ?session_id ch] is {!serve} for a SockJS channel: it sends the open
      frame, then unwraps/wraps DDP messages in SockJS array frames (for the unmodified Meteor
      browser client). *)
  val serve_sockjs : ?user_id:string -> ?session_id:string -> Fennec_core.Ws_channel.t -> unit

  (** [paw ?path ()] is the websocket paw serving DDP at [path] (default [/websocket]). It
      automatically seeds the DDP session user id from native Accounts and installs the built-in
      Accounts methods ([login], [logout], [currentUser], ...). [?user_id] is only for custom/test
      transports that intentionally override native Accounts. *)
  val paw : ?path:string -> ?user_id:(Fennec_paw.Conn.t -> string option) -> unit -> Fennec_paw.Paw.t
end
