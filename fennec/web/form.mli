(** Typed form INPUT over the SAME {!Codec} model that powers collections and DDP methods — the input
    half of a handler. {!read} coerces the stringly request a form (or query) submits into the shape's
    BSON and runs the codec's full decode + validation, so the handler gets a typed value with the
    model's refinements enforced and per-field errors for re-rendering. No HTML building — forms are
    authored as plain [.mlx] markup; this module only reads the request.

    {[ let post conn =
         match Form.read Message.codec conn with
         | Ok m -> ignore (Messages.create m); Handler.redirect conn "/thanks" ~flash:"Sent!"
         | Error errs -> Handler.html ~status:422 conn (Contact_page.view ~errors:errs ()) ]} *)

module Conn = Fennec_paw.Conn

(** Where {!read} takes the pairs from: the request [Body] (a POSTed form — the default) or the
    [Query] string (a GET filter/search form). *)
type source = Body | Query

(** [read ?from ?inject ?ignore codec conn] coerces + validates the submitted pairs into [codec]'s
    type. Repeated keys feed a list field; dotted keys ([author.name]) feed an embedded record; an
    absent checkbox reads [false]; an absent non-bool field defers to the codec's
    required/optional/default rule; a coercion failure is a per-field error (its redundant "missing"
    decode error suppressed). [~inject] supplies server-controlled fields that OVERRIDE any
    client-submitted value (mass-assignment safety); [~ignore] drops named keys from user input. *)
val read :
  ?from:source ->
  ?inject:(string * string) list ->
  ?ignore:string list ->
  'a Codec.t ->
  Conn.t ->
  ('a, Codec.error list) result

(** {!read} over an explicit assoc list — the testable core (no {!Conn}). *)
val parse_assoc : 'a Codec.t -> (string * string) list -> ('a, Codec.error list) result

(** The error messages attached to one field (by wire name) — inline feedback when re-rendering.
    Pass a field handle from the model's generated [Fields] module. *)
val field_errors : _ Codec.field -> Codec.error list -> string list

(** The raw value the user just submitted for [name] — repopulate an input when re-rendering. *)
val submitted : Conn.t -> string -> string

(** The canonical validation-error summary (zod's [flatten] / Ecto's [traverse_errors] shape):
    top-level (path-less) messages separated from per-field messages keyed by wire name. *)
type error_summary = { form_errors : string list; field_errors : (string * string list) list }

val summary : Codec.error list -> error_summary
