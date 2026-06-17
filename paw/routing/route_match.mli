(** Pure path + method matching rules, shared by {!Pipeline.on} (the runtime route-as-paw) and
    {!Route_table} (the compiled dispatch) so both apply identical semantics. Depends only on
    {!Http} — no Conn, no Pipeline. *)

(** [meth_matches want got] — does a request method [got] satisfy a route declared for [want]?
    A [GET] route also answers [HEAD] (the body is stripped downstream). *)
val meth_matches : Http.meth -> Http.meth -> bool

(** [segments path] splits a path into its non-empty segments (leading/trailing/double slashes ignored). *)
val segments : string -> string list

(** [match_segments pattern_segs path_segs []] matches pre-split pattern segments against pre-split
    path segments, capturing [:name] (one segment) and a trailing [*name] (the remaining path joined
    by ["/"]). Returns the captures in order, or [None] if they don't match. *)
val match_segments : string list -> string list -> (string * string) list -> (string * string) list option

(** [has_params pattern] — whether the pattern carries a [:param] or [*splat] (so it needs segment
    matching rather than an exact string compare). *)
val has_params : string -> bool
