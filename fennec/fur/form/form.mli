(** The CLIENT form primitive — a reactive form bound to a {!Sift} codec.

    A browser SPA form gets the SAME refinement validation the server enforces
    ([@trim] [@non_empty] [@max_len] [@email] [@min]…), recomputed reactively, offline, with no
    round-trip: type a character and {!errors} updates synchronously. The draft lives in ONE
    {!Fur.signal}; {!errors} is a derived {!Fur.memo} that re-runs {!Sift.validate} on every change.
    Per-field feedback is keyed by the model's generated typed field handle ([Model.Fields.x]) — a
    renamed field is a compile error, the same discipline as {!Filter} / [%q].

    This UNIFIES the form stories on one codec and one {!Sift.error} shape. The server's
    {!Fennec_web.Form.read} (coerce + decode + validate a POST) and this {!signal} both produce the
    SAME {!Sift.error} codes and paths, because they validate the SAME ['a Sift.t]. The HTML5
    constraint attributes ({!input_attrs}) come off that same codec — browser, client form, and
    server all enforce ONE declaration. Pure ({!Fur} signals + {!Sift}), so it runs native (SSR /
    tests) and in the js_of_ocaml bundle unchanged.

    {[ (* a .mlx component — the model's codec is shared verbatim with the server [Form.read] *)
       let f = Form.signal Signup.codec (Sift.default Signup.codec)
       let view () =
         <form>
           <input attrs=(Form.input_attrs Signup.Fields.email)          (* required, type=email, off the codec *)
                  name="email" value=((Form.value f).email)
                  onInput=(Form.update f (fun d -> { d with email = target_value () })) />
           (match Form.field_error f Signup.Fields.email with
            | Some msg -> <p className="err">{msg}</p> | None -> <span/>)
           <button disabled=(not (Form.valid f))>Sign up</button>
         </form> ]} *)

(** A reactive form over a codec ['a Sift.t]: the typed draft plus its live error list. *)
type 'a t

(** [signal codec init] — a fresh reactive form over [codec], seeded with [init]. Use
    [Sift.default codec] for an empty form. The error list is validated immediately and on every
    {!set}/{!update}. *)
val signal : 'a Sift.t -> 'a -> 'a t

(** The current typed draft — REACTIVE (subscribes the caller, so a render re-runs on every edit). *)
val value : 'a t -> 'a

(** The current draft WITHOUT subscribing — read it inside an event handler that writes the same form
    (so the handler does not re-run on its own write). *)
val peek : 'a t -> 'a

(** Replace the whole draft (re-validates on the next {!errors}/{!valid}/{!field_error} read). *)
val set : 'a t -> 'a -> unit

(** [update f g] — transform the draft with [g] (a record update for an edited field). *)
val update : 'a t -> ('a -> 'a) -> unit

(** The live validation errors of the current draft — REACTIVE; [[]] when valid. The SAME
    {!Sift.error} list (codes + paths) the server's {!Fennec_web.Form.read} yields for this codec. *)
val errors : 'a t -> Sift.error list

(** REACTIVE: the draft satisfies every refinement ([errors = []]) — drive a submit button's
    [disabled]. *)
val valid : 'a t -> bool

(** [field_error f field] — the FIRST error message for one field, by its typed handle
    ([Model.Fields.x]). REACTIVE inline feedback. Matches on the error path head (the field's wire
    name), exactly like {!Fennec_web.Form.error}, so client and server show the same per-field message. *)
val field_error : 'a t -> _ Sift.field -> string option

(** Every error message for one field (a field can break several refinements at once) — REACTIVE. *)
val field_errors : 'a t -> _ Sift.field -> string list

(** The submit gate — the typed value when the draft is valid, else its errors. A NON-reactive peek
    (call it from an [onClick]); the [Ok] value is exactly what a DDP method / {!Sift.encode_json}
    would send, the same boundary the server re-validates. *)
val submit : 'a t -> ('a, Sift.error list) result

(** {1 Codec-driven HTML5 constraint attributes}

    Walk a field's {!Sift.field_view} + {!Sift.field_required} (the same reflection [json_schema.ml]
    renders to [$jsonSchema]) and emit the browser's native form attributes from the ONE codec — so
    the browser enforces the constraints before any JS runs, with zero hand-duplication. *)

(** [input_attrs field] — the HTML5 constraint attrs for one typed field handle ([Model.Fields.x]):
    a [@non_empty] field becomes [required], [@max_len 40] becomes [maxlength="40"], [@min_len] →
    [minlength], a numeric field [type="number"] with [min]/[max] bounds, an [@email]/[@url] field the
    semantic [type="email"]/[type="url"] input, any other [pattern] a [pattern=]. Drop the list straight
    onto an [<input>] (the caller still writes [name=]). *)
val input_attrs : _ Sift.field -> Fur.attr list

(** [field_attrs view required] — the pure core {!input_attrs} builds on: the attrs for a field of
    structural [view] that is [required] or not. Use directly when you hold a {!Sift.view} rather than a
    field handle. *)
val field_attrs : Sift.view -> bool -> Fur.attr list
