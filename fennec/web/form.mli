(** Typed form INPUT over the SAME {!Sift} model that powers collections and DDP methods — the input
    half of a handler. {!read} coerces the stringly request a form (or query) submits into the shape's
    BSON and runs the codec's full decode + validation, so the handler gets a typed value with the
    model's refinements enforced and per-field errors for re-rendering. No HTML building — forms are
    authored as plain [.mlx] markup; this module only reads the request.

    {[ let post conn =
         match Form.read Message.codec conn with
         | Ok m -> ignore (Messages.create m); Handler.redirect conn "/thanks" ~flash:"Sent!"
         | Error errs -> Handler.html ~status:422 conn (Contact_page.view ~errors:errs ()) ]} *)

module Conn = Paw.Conn

(** {1 One Form, both surfaces — the client primitive + codec-driven attrs, re-exported}

    The CLIENT form primitive (the top-level [Form] module of [fennec.fur.form]) re-exported here, so a
    server-rendered handler and a browser SPA component reach the SAME [Form.signal] / [Form.input_attrs]
    under one name, validating the SAME ['a Sift.t] with the SAME {!Sift.error} shape. The full reactive
    surface ([value]/[valid]/[field_error]/[submit]…) lives on [fennec.fur.form] for client components;
    here we surface the constructor and the codec-driven HTML5 attrs a server-rendered [<input>] uses. *)

(** A reactive client form over a codec — see [fennec.fur.form]'s [Form.t]. ([Fur_form] is that module
    under a collision-free name, since this unit is itself named [Form].) *)
type 'a live = 'a Fur_form.t

(** [signal codec init] — a fresh reactive client form (see [fennec.fur.form]'s [Form.signal]). *)
val signal : 'a Sift.t -> 'a -> 'a live

(** [input_attrs field] — the HTML5 constraint attrs ([required]/[maxlength]/[type=email]/…) for one
    typed field handle, straight off the codec. Drop onto a server-rendered [<input>] so the browser
    enforces the model's refinements natively, with no hand-duplication. *)
val input_attrs : _ Sift.field -> Fur.attr list

(** [field_attrs view required] — the pure core {!input_attrs} builds on. *)
val field_attrs : Sift.view -> bool -> Fur.attr list

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
  'a Sift.t ->
  Conn.t ->
  ('a, Sift.error list) result

(** {!read} over an explicit assoc list — the testable core (no {!Conn}). *)
val parse_assoc : 'a Sift.t -> (string * string) list -> ('a, Sift.error list) result

(** The error messages attached to one field (by wire name) — inline feedback when re-rendering.
    Pass a field handle from the model's generated [Fields] module. *)
val field_errors : _ Sift.field -> Sift.error list -> string list

(** The raw value the user just submitted for [name] — repopulate an input when re-rendering. *)
val submitted : Conn.t -> string -> string

(** The canonical validation-error summary (zod's [flatten] / Ecto's [traverse_errors] shape):
    top-level (path-less) messages separated from per-field messages keyed by wire name. *)
type error_summary = { form_errors : string list; field_errors : (string * string list) list }

val summary : Sift.error list -> error_summary

(** {2 Form-handler view-state + outcome}

    A [frontend/handlers/<name>.mlx] FORM handler is [view : ctx -> vnode] + [submit : conn -> t -> outcome]
    (the fur ppx generates GET/POST/serve). The [view] reads this {!ctx}; [submit] returns an {!outcome}. *)

(** The per-render context a form handler's [view] receives: the conn (for {!csrf} + {!value}), the
    flash from a prior {!redirect}, and the validation errors from a failed {!read}. The ppx builds it;
    [view] only reads it (a plain value — concurrency-safe). *)
type ctx

val ctx : conn:Conn.t -> flash:string option -> errors:Sift.error list -> ctx

(** The flash banner (empty without a message). *)
val flash : ctx -> Fur.vnode

(** This form's CSRF hidden field (= {!Handler.csrf_field}; empty without the Csrf paw). *)
val csrf : ctx -> Fur.vnode

(** Repopulate an input with the value the user just submitted for [name] (empty on the first GET). *)
val value : ctx -> string -> string

(** The inline validation error(s) for one field, by wire name (empty when it validated). *)
val error : ctx -> string -> Fur.vnode

(** What a form handler's [submit] returns — the framework renders it, so re-rendering HTML (error
    states, awaiting corrections) is first-class: re-show the form with errors ([Again] — input
    preserved; codec-invalid input auto-does this), [Redirect] (post-redirect-get), or an arbitrary
    [Page]. Build with {!again}/{!redirect}/{!page}. *)
type outcome = Again of Sift.error list | Redirect of string * string option | Page of Fur.vnode

(** Re-render the form with these per-field errors (wire-name, message), 422. *)
val again : (string * string) list -> outcome

(** Post-redirect-get, optionally flashing a message onto the next page. *)
val redirect : ?flash:string -> string -> outcome

(** Render an arbitrary result page (200). *)
val page : Fur.vnode -> outcome
