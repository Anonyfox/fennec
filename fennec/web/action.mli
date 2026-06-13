(** Typed request extraction — the input half of an action. JSON request bodies decode straight
    through the {!Codec} model (the API-input counterpart of {!Form.parse}); path/query scalars
    coerce with the stdlib. HTML form bodies go through {!Form.parse}.

    {[ let create conn =
         match Action.json Post.codec conn with     (* API: JSON in *)
         | Ok post -> save post; Respond.model ~status:201 conn Post.codec post
         | Error errs -> Respond.errors conn errs ]} *)

module Conn = Fennec_paw.Conn

(** The raw request body. *)
val body : Conn.t -> string

(** Decode a JSON request body into a codec's type — validation/refinements apply exactly as for
    forms; a malformed body or a failed check returns per-field errors. *)
val json : 'a Codec.t -> Conn.t -> ('a, Codec.error list) result

(** A path parameter (e.g. [:id]). *)
val path : Conn.t -> string -> string option

(** A query-string parameter. *)
val query : Conn.t -> string -> string option

val path_int : Conn.t -> string -> int option
val query_int : Conn.t -> string -> int option

(** A query scalar with a fallback (e.g. pagination [?page=]). *)
val query_default : default:string -> Conn.t -> string -> string

val query_int_default : default:int -> Conn.t -> string -> int
