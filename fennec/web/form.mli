(** Typed HTML forms over the SAME {!Codec} model that powers collections and DDP methods — one
    declaration, no parallel validation story. An HTML form (or a GET filter form) submits
    stringly-typed [(key, value)] pairs; {!parse} coerces them into the shape's BSON (driven by
    {!Codec.view}) and runs the codec's full decode + validation, so the model's refinements
    ([@non_empty], [@max_len], [@email], [@min], …) apply for free and failures come back as
    per-field errors keyed by wire name — exactly what re-rendering a form with inline feedback wants.

    {[ (* the post handler: parse, persist, redirect — or re-render with field errors (PRG) *)
       let create conn =
         match Form.parse Post.codec conn with
         | Ok post -> ignore (save post); Conn.redirect conn "/posts"
         | Error errs ->
             let title_errs = Form.field_errors Post.Fields.title errs in
             Conn.html ~status:422 conn (render_form ~conn ~errors:errs) ]} *)

module Conn = Fennec_paw.Conn

(** Where {!parse} reads the submitted pairs from: the request [Body] (a POSTed form, urlencoded or
    multipart — the default) or the [Query] string (a GET filter/search form). *)
type source = Body | Query

(** [parse ?from codec conn] coerces the submitted pairs into [codec]'s type and validates them.
    Repeated keys feed a list field; an absent checkbox reads as [false]; an absent non-bool field
    defers to the codec's own required/optional/default rule. A coercion failure (e.g. ["abc"] for an
    int) is reported per-field and its redundant "missing" decode error is suppressed. *)
val parse : ?from:source -> 'a Codec.t -> Conn.t -> ('a, Codec.error list) result

(** {!parse} over an explicit assoc list — the testable core (no {!Conn}). *)
val parse_assoc : 'a Codec.t -> (string * string) list -> ('a, Codec.error list) result

(** The error messages attached to one field (matched by its wire name) — drives inline feedback.
    Pass a field handle from the model's generated [Fields] module. *)
val field_errors : _ Codec.field -> Codec.error list -> string list

(** The raw value the user just submitted for [name] — repopulate an input when re-rendering. *)
val submitted : Conn.t -> string -> string
