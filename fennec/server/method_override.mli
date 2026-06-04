(** Method override — lets an HTML form POST act as PUT/PATCH/DELETE, via a [_method] form
    field or the X-HTTP-Method-Override header. Only POST requests are rewritten. *)

(** Build the method-override paw. [field] (default ["_method"]) is the form field consulted
    when the [header] (default ["x-http-method-override"]) is absent. *)
val make : ?field:string -> ?header:string -> unit -> Fennec_paw.Paw.t
