(** The browser DDP client (Js_of_ocaml): dials the server's [/websocket], runs the DDP handshake,
    and feeds sub-tagged deltas into a live merge store. {!find} is the reactive Fur query over the
    merged data; {!call} invokes a server method (the data it changes flows back through the open
    subscription as a normal delta). Browser-only — there is no server-side DDP client. *)

(** A live connection to the DDP server. *)
type t

(** [connect ?path ()] opens a WebSocket to [path] (default [/websocket]) on the current origin and
    sends [connect]. Returns immediately; data arrives asynchronously into {!find}. *)
val connect : ?path:string -> unit -> t

(** [subscribe t ~name ?params ()] starts the named publication; its documents stream into the merge
    store and become visible through {!find}. *)
val subscribe : t -> name:string -> ?params:Bson.t list -> unit -> unit

(** [call t ~name ?params ()] invokes a server method (fire-and-forget; any data it changes returns
    via an open subscription). *)
val call : t -> name:string -> ?params:Bson.t list -> unit -> unit

(** [find t name ?selector ?sort ?skip ?limit ?fields ()] is a Fur signal of the matching documents
    that recomputes as the server pushes changes. Read it with {!Fur.get} inside a component. *)
val find :
  t ->
  string ->
  ?selector:Bson.t ->
  ?sort:Bson.t ->
  ?skip:int ->
  ?limit:int ->
  ?fields:Bson.t ->
  unit ->
  Bson.t array Fur.signal
