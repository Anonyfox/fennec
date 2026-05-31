(** Filename → MIME content-type, and which types are worth compressing. Pure. *)

(** Content-type for a path, by file extension; unknown → ["application/octet-stream"].
    Text types carry a charset. *)
val of_path : string -> string

(** Whether a content-type is worth gzip'ing (text-ish / json / wasm / svg) vs
    already-compressed media and fonts. Ignores any [;charset] parameter. *)
val compressible : string -> bool
