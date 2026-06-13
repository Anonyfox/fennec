(** Content negotiation + JSON output: one resource action serves a browser (HTML) and an API client
    (JSON) off the same handler. JSON is encoded straight from the {!Codec} model, so the wire shape
    matches the DB + form shape — no separate serializer.

    {[ let show conn post =
         Respond.negotiate conn
           ~html:(fun c -> View.document c (Posts_view.show post))
           ~json:(fun c -> Respond.model c Post.codec post) ]} *)

module Conn = Fennec_paw.Conn

(** Whether the client prefers JSON (walks the Accept header in order; HTML is the default). A
    JSON-API handler can branch on this; HTML and JSON are otherwise kept as separate code paths. *)
val prefers_json : Conn.t -> bool

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

(** A typed handler outcome (ASP.NET [TypedResults] / a return-type union) — a JSON-API handler
    returns one of these and {!api} renders it with the right status, instead of hand-building the Conn.

    {[ let show conn = Respond.api conn Post.codec (match find (Action.path conn "id") with
                                                    | Some p -> Ok_ p | None -> Not_found) ]} *)
type 'a outcome =
  | Ok_ of 'a  (** 200 + the value *)
  | Created of 'a  (** 201 + the value *)
  | No_content  (** 204 *)
  | Not_found  (** 404 *)
  | Invalid of Codec.error list  (** 422 + the error envelope *)
  | Redirect of string  (** 303 to a location *)

(** Render a typed {!outcome} to a JSON response with the matching status code. *)
val api : Conn.t -> 'a Codec.t -> 'a outcome -> Conn.t
