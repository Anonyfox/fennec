(** Content negotiation + JSON output: one resource action serves a browser (HTML) and an API client
    (JSON) off the same handler. JSON is encoded straight from the {!Codec} model, so the wire shape
    matches the DB + form shape — no separate serializer.

    {[ let show conn post =
         Respond.negotiate conn
           ~html:(fun c -> View.document c (Posts_view.show post))
           ~json:(fun c -> Respond.model c Post.codec post) ]} *)

module Conn = Fennec_paw.Conn

(** Whether the client prefers JSON (walks the Accept header in order; HTML is the default — browsers
    and a missing Accept header). *)
val prefers_json : Conn.t -> bool

(** [negotiate conn ~html ~json] runs [json] when the client prefers JSON, else [html]. *)
val negotiate : Conn.t -> html:(Conn.t -> Conn.t) -> json:(Conn.t -> Conn.t) -> Conn.t

(** Answer with a raw BSON value as JSON. *)
val bson : ?status:int -> Conn.t -> Bson.t -> Conn.t

(** Answer with ONE model value, encoded through its codec. *)
val model : ?status:int -> Conn.t -> 'a Codec.t -> 'a -> Conn.t

(** Answer with a LIST of model values. *)
val models : ?status:int -> Conn.t -> 'a Codec.t -> 'a list -> Conn.t

(** The JSON error envelope — the canonical {!Form.summary} shape:
    [{ form_errors: [..], field_errors: { field: [..] } }] (zod's [flatten] shape). *)
val error_envelope : Codec.error list -> Bson.t

(** Answer with the validation-error envelope ([422] by default). *)
val errors : ?status:int -> Conn.t -> Codec.error list -> Conn.t
