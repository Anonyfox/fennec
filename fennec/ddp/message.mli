(** The DDP message model and its JSON codec — one variant per [msg] type from the spec
    (DATAFLOW.md §2). Absent optional fields are omitted on encode and tolerated on decode; EJSON
    fields/params go through {!Ejson}. Pure — native and JavaScript. *)

(** A DDP error payload (in [nosub] / [result]). [code] is a string in DDP v1, but real Meteor
    sends a number — {!of_json} coerces it. (On the wire the field is named ["error"].) *)
type error = {
  code : string;
  reason : string option;
  message : string option;
  error_type : string;
}

(** A DDP message. The [sub] tag on [Added]/[Changed]/[Removed] is the contributing subscription id
    in our extended mode ([None] in standard DDP — Meteor ignores the unknown field). *)
type t =
  | Connect of { session : string option; version : string; support : string list }
  | Connected of { session : string }
  | User of { id : string option }
      (** v2 extension: the connection's authenticated user, pushed on connect and whenever a method
          rebinds it ([set_user_id]) — the client purges its persisted cache on identity change.
          Stock DDP clients ignore unknown messages. *)
  | Failed of { version : string }
  | Ping of { id : string option }
  | Pong of { id : string option }
  | Sub of { id : string; name : string; params : Bson.t list }
  | Unsub of { id : string }
  | Nosub of { id : string; error : error option }
  | Added of { collection : string; id : string; fields : (string * Bson.t) list; sub : string option }
  | Changed of {
      collection : string;
      id : string;
      fields : (string * Bson.t) list;
      cleared : string list;
      sub : string option;
    }
  | Removed of { collection : string; id : string; sub : string option }
  | Ready of { subs : string list }
  | Added_before of {
      collection : string;
      id : string;
      fields : (string * Bson.t) list;
      before : string option;
    }
  | Moved_before of { collection : string; id : string; before : string option }
  | Method of { method_ : string; params : Bson.t list; id : string; random_seed : Bson.t option }
  | Result of { id : string; error : error option; result : Bson.t option }
  | Updated of { methods : string list }
  | Error_msg of { reason : string; offending : Json.t option }

(** Raised by {!of_json}/{!decode} on a missing or unknown [msg]. *)
exception Bad_message of string

(** Encode a message to its wire JSON. *)
val to_json : t -> Json.t

(** Decode wire JSON to a message. @raise Bad_message on a missing/unknown [msg]. *)
val of_json : Json.t -> t

(** [encode m] is the wire JSON string of [m]. *)
val encode : t -> string

(** [decode s] parses a wire JSON string to a message. @raise Bad_message / {!Json.Parse_error}. *)
val decode : string -> t
