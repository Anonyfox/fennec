(** Change streams — the reactive primitive over a real MongoDB. A blocking watch cursor runs in an
    Eio systhread; {!next} returns [None] on a server-side timeout (no deadlock) and [Some event] on
    a real change. {!Live} builds the Meteor-style livequery fan-out on top. Requires a replica set
    (even a single node) — see {!Server}.

    {[
      let cs = watch coll ~full_document:true ~max_await_ms:200 () in
      let rec loop () =
        match next cs with                 (* None on a server-side timeout — just retry *)
        | Some ev -> handle ev.op ev.full_document; loop ()
        | None -> loop ()
      in
      Fun.protect ~finally:(fun () -> close cs) loop
    ]} *)

(** An open change stream (a libmongoc watch cursor). *)
type t = Fennec_mongo_ffi.Mongo_ffi.change_stream

(** The kind of change, parsed from the wire's [operationType] into a closed variant so downstream
    code matches exhaustively instead of comparing strings. *)
type operation =
  | Insert
  | Update
  | Replace
  | Delete
  | Invalidate
  | Drop
  | Drop_database
  | Rename
  | Other of string

(** Parse an [operationType] string. *)
val operation_of_string : string -> operation

(** The inverse of {!operation_of_string}. *)
val string_of_operation : operation -> string

(** One change event. [full_document] is present for inserts, or for updates/replaces when the
    stream was opened with [~full_document:true] ([fullDocument:updateLookup]). [resume_token] is the
    change's [_id] — persist it to resume later. *)
type event = {
  op : operation;
  ns : string;  (** ["db.collection"] *)
  document_key : Bson.t;  (** [{ _id: … }] *)
  full_document : Bson.t option;
  resume_token : Bson.t;
  raw : Bson.t;  (** the whole change event *)
}

(** [watch col ?pipeline ?full_document ?max_await_ms ?resume_after ()] opens a change stream on
    [col]. [~full_document:true] requests the post-image on updates ([updateLookup]); [max_await_ms]
    bounds how long a single {!next} blocks server-side (so cancellation is honoured within a cycle);
    [resume_after] resumes from a prior {!event.resume_token}. *)
val watch :
  Collection.t ->
  ?pipeline:Bson.t ->
  ?full_document:bool ->
  ?max_await_ms:int ->
  ?resume_after:Bson.t option ->
  unit ->
  t

(** Parse a raw change-event JSON string into an {!event}. *)
val parse : string -> event

(** [next t] blocks (off the scheduler) for the next event; [None] on a server-side timeout — call
    again. @raise Failure if the stream errored or was closed. *)
val next : t -> event option

(** Close the stream and release its pooled connection. *)
val close : t -> unit
