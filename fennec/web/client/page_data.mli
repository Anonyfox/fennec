(** The client/isomorphic read of a Page's cross-stage payload — the SAME {!Codec} the server encoded
    with decodes it back (server: from the SSR seed; client: from [window.__FUR_DATA__]), so the view
    hydrates byte-for-byte. Declare it INSIDE the view (per render) so it reads the current seed. *)

val resource : 'a Codec.t -> key:string -> fallback:'a -> 'a Fur.Data.t
