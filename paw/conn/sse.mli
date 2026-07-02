(* Server-Sent Events — a long-lived [text/event-stream] response for the browser's [EventSource]:
   live updates without websocket ceremony (notifications, progress, a tailing log). A thin, correct
   layer over [Conn.send_chunked]; see the .ml header for the wire rules. *)

(** A data event (optionally named / id'd / carrying a reconnect hint) or a comment-only keepalive. *)
type event =
  | Data of { data : string; event : string option; id : string option; retry : int option }
  | Comment of string

val data : ?event:string -> ?id:string -> ?retry:int -> string -> event
val comment : string -> event

val to_wire : event -> string
(** The SSE wire form: one [data:] line per payload line (CRLF-safe), optional [event]/[id]/[retry]
    fields, the blank line that dispatches. *)

val stream : Conn.t -> (push:(event -> unit) -> unit) -> Conn.t
(** Answer with an event stream: sets [content-type: text/event-stream] + [cache-control: no-cache];
    the producer pushes events until it returns (it typically loops, blocking on its source). *)
