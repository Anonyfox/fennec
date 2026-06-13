(** Method override — lets an HTML form POST act as PUT/PATCH/DELETE, via a [_method] form
    field or the [X-HTTP-Method-Override] header. Only POST requests are rewritten.

    Place it early in an endpoint pipeline so later route matching sees the effective method:
    {[
      let pipeline = [ Method_override.make (); (* … routes … *) ]
    ]}
    Then a form posting [_method=PUT] is routed as a PUT. *)

(** Build the method-override paw. [field] (default ["_method"]) is the form field consulted
    when the [header] (default ["x-http-method-override"]) is absent. *)
val make : ?field:string -> ?header:string -> unit -> Fennec_paw.Paw.t
