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

(** [parse ?from ?inject ?ignore codec conn] coerces the submitted pairs into [codec]'s type and
    validates them. Repeated keys feed a list field; an absent checkbox reads as [false]; an absent
    non-bool field defers to the codec's own required/optional/default rule. A coercion failure (e.g.
    ["abc"] for an int) is reported per-field and its redundant "missing" decode error is suppressed.

    [~inject] supplies server-controlled fields (id, owner, timestamps) the form must not carry — they
    OVERRIDE any client-submitted value for the same key (mass-assignment safety). [~ignore] drops
    named keys from the user input entirely. *)
val parse :
  ?from:source ->
  ?inject:(string * string) list ->
  ?ignore:string list ->
  'a Codec.t ->
  Conn.t ->
  ('a, Codec.error list) result

(** {!parse} over an explicit assoc list — the testable core (no {!Conn}). *)
val parse_assoc : 'a Codec.t -> (string * string) list -> ('a, Codec.error list) result

(** The error messages attached to one field (matched by its wire name) — drives inline feedback.
    Pass a field handle from the model's generated [Fields] module. *)
val field_errors : _ Codec.field -> Codec.error list -> string list

(** The raw value the user just submitted for [name] — repopulate an input when re-rendering. *)
val submitted : Conn.t -> string -> string

(** {1 Rendering}

    Inputs bind to the SAME field handles {!parse} reads, so a renamed model field is a compile error
    in BOTH directions — the form and its parser cannot drift. All produce {!Fur} vnodes (attribute
    values + text are escaped), renderable as a dead view or composed into a live one.

    {[ View.document conn (View.page ~title:"New post"
         [ Form.form ~action:"/posts" ~csrf:(Csrf.token ~secret conn)
             [ Form.field Post.Fields.title ~label:"Title"
                 ~value:(Form.submitted conn "title") ~errors:(Form.field_errors Post.Fields.title errs);
               Form.field Post.Fields.body ~label:"Body" ~kind:`Textarea;
               Form.submit "Publish" ] ]) ]} *)

(** A hidden input (carried value, CSRF token, method override). *)
val hidden : name:string -> value:string -> Fur.vnode

(** A [<label for=…>] bound to a field's wire name. *)
val label_for : _ Codec.field -> string -> Fur.vnode

(** A text-style [<input>] bound to a field ([type_] defaults to ["text"]). *)
val input : ?type_:string -> ?value:string -> ?attrs:(string * string) list -> _ Codec.field -> Fur.vnode

val textarea : ?value:string -> ?attrs:(string * string) list -> _ Codec.field -> Fur.vnode
val checkbox : ?checked:bool -> ?attrs:(string * string) list -> _ Codec.field -> Fur.vnode

(** A [<select>] bound to a field; [options] is [(value, label)] pairs, [selected] marks the current. *)
val select :
  ?selected:string -> ?attrs:(string * string) list -> options:(string * string) list -> _ Codec.field -> Fur.vnode

val submit : ?attrs:(string * string) list -> string -> Fur.vnode

(** A labeled field row — [<label>] + control + inline error list — in one call. [kind] picks the
    control; [value]/[errors] repopulate + annotate after a failed submit (feed {!submitted} /
    {!field_errors}); [attrs] adds extra input attributes. *)
val field :
  ?label:string ->
  ?kind:[ `Text | `Email | `Password | `Number | `Date | `Textarea | `Checkbox ] ->
  ?value:string ->
  ?errors:string list ->
  ?attrs:(string * string) list ->
  _ Codec.field ->
  Fur.vnode

(** {1 The bound form ([to_form])}

    A {!ctx} carries the request (for value repopulation), the errors to show, and the form chrome
    (action/method/CSRF/override). {!bound} then renders ONE field with everything filled in
    automatically — value, per-field errors, inferred input type, and HTML5 constraint attributes
    derived from the model's refinements — so the view never threads value/error/type by hand.

    {[ (* new + edit + re-render-after-error, all the same code *)
       let f = Form.start conn ~secret ~action:"/posts" ~errors
                 ?values:(if editing then Some (Form.values_of Post.codec post) else None) () in
       View.document conn (View.page ~title:"Post"
         (Form.render f
            [ Form.bound f Post.Fields.title ~label:"Title";       (* type/required/maxlength inferred *)
              Form.bound f Post.Fields.body ~label:"Body" ~kind:`Textarea;
              Form.bound f Post.Fields.priority ~label:"Priority"; (* number + min/max from refinements *)
              Form.bound f Post.Fields.published ~label:"Published"; (* checkbox inferred *)
              Form.submit "Save" ])) ]} *)

(** Encode a model value as form-input strings — prefill an edit form via {!start}'s [~values]. *)
val values_of : 'a Codec.t -> 'a -> (string * string) list

(** The HTML5 constraint attributes a field's refinements imply ([required]/[minlength]/[maxlength]/
    [min]/[max]/[pattern]) — client-side validation from the same model the server validates. *)
val constraints : _ Codec.field -> (string * string) list

(** A bound form. *)
type ctx

(** Open a bound form. [~errors] show inline; [~values] prefill a fresh render (see {!values_of});
    [~secret] auto-mints a CSRF token (or pass [~csrf]); [~override] simulates PUT/PATCH/DELETE.
    After a failed submit the request body repopulates every field automatically. *)
val start :
  ?method_:[ `GET | `POST ] ->
  ?errors:Codec.error list ->
  ?values:(string * string) list ->
  ?override:string ->
  ?csrf:string ->
  ?secret:string ->
  action:string ->
  Conn.t ->
  ctx

(** Render ONE field of a bound form — value, errors, input type, and constraints all filled in.
    Pass [~kind] only to override the inferred control (e.g. [`Textarea], [`Email], [`Password]). *)
val bound :
  ctx ->
  ?label:string ->
  ?kind:[ `Text | `Email | `Password | `Number | `Date | `Textarea | `Checkbox ] ->
  _ Codec.field ->
  Fur.vnode

(** Wrap a bound form's fields in the [<form>] (action/method/CSRF/override all from the ctx). *)
val render : ctx -> Fur.vnode list -> Fur.vnode

(** The [<form>] wrapper. [method_] is the HTML method; [override] simulates PUT/PATCH/DELETE via the
    [_method] field (pair with {!Fennec_server.Method_override}); [csrf] embeds the token in
    [_csrf_token] (pair with the CSRF paw — mint with [Csrf.token] in the handler). *)
val form :
  ?method_:[ `GET | `POST ] ->
  action:string ->
  ?csrf:string ->
  ?override:string ->
  ?attrs:(string * string) list ->
  Fur.vnode list ->
  Fur.vnode
