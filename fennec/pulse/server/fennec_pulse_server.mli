(** DDP-over-WebSocket server wiring: bridges a {!Fennec_pulse.Reactive} instance to a
    {!Fennec_ddp.Session} and serves it over a live {!Fennec_core.Ws_channel.t}. The bridge is
    delta-driven — publications feed the session's sink straight from [observe_changes] (no merge
    box, no polling) — and methods route through the reactive instance's [call], with a reactive
    [Error] mapped to the DDP error payload. *)

(** [Make (R)] wires the reactive instance [R] to a DDP transport. *)
module Make (R : Fennec_pulse.Reactive.REACTIVE) : sig
  (** [serve ?user_id ?session_id ch] runs a DDP session on a raw [/websocket] channel: one DDP JSON
      message per text frame, decoded into the session and emitted back. Tears the session down on
      close. [session_id] defaults to a fresh ObjectId. [user_id] seeds the connection identity from
      an already-verified Accounts cookie. *)
  val serve : ?user_id:string -> ?session_id:string -> Fennec_core.Ws_channel.t -> unit

  (** [serve_sockjs ?user_id ?session_id ch] is {!serve} for a SockJS channel: it sends the open
      frame, then unwraps/wraps DDP messages in SockJS array frames (for the unmodified Meteor
      browser client). *)
  val serve_sockjs : ?user_id:string -> ?session_id:string -> Fennec_core.Ws_channel.t -> unit

  (** [paw ?path ()] is the websocket paw serving DDP at [path] (default [/websocket]) — mount it in
      an app to expose the realtime endpoint. *)
  val paw : ?path:string -> ?user_id:(Fennec_paw.Conn.t -> string option) -> unit -> Fennec_paw.Paw.t
end
