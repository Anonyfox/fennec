(** MongoDB wire-protocol message framing — pure bytes <-> structured message. Handles OP_MSG (2013,
    everything after the handshake, including kind-1 document sequences) and OP_QUERY (2004, the legacy
    handshake), reassembling the logical command document either way. Bounds-safe: malformed input
    raises {!Protocol_error}, which the server turns into a clean connection error. *)

(** A frame that could not be parsed (short header, bad section, truncated field, unsupported opcode). *)
exception Protocol_error of string

(** Which framing a reply must use — mirrors the request. *)
type reply_kind = Op_msg | Op_reply

(** A parsed request. [command] is the reassembled command document: the kind-0 body with every kind-1
    document sequence folded back in as an array field, and (for OP_QUERY) "$db" injected from the
    namespace. [more_to_come] is set when the client wants no reply (a fire-and-forget write). *)
type incoming = {
  request_id : int;
  command : Bson.t;
  reply_kind : reply_kind;
  more_to_come : bool;
}

val parse : string -> incoming
(** [parse frame] parses a complete message (16-byte header + body). Raises {!Protocol_error}. *)

val message_length : string -> int
(** [message_length header] is the total message length declared in the first 4 bytes — the server uses
    it to know how many more bytes to read (after capping it against the max message size). *)

val reply : response_to:int -> reply_kind -> Bson.t -> string
(** [reply ~response_to kind doc] serialises [doc] as a complete reply frame whose responseTo is
    [response_to] (the request's [request_id]). *)

val command_name : Bson.t -> string option
(** The command name — the first field of the command document (Mongo convention). *)

val db : Bson.t -> string option
(** The target database — the "$db" field of the command document. *)
