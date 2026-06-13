(** Filename → MIME content-type, and which types are worth compressing. Pure.

    {[
      let ct = Mime.of_path "/assets/app.css" in   (* "text/css; charset=utf-8" *)
      if Mime.compressible ct then gzip body else body
    ]} *)

(** Content-type for a path, by file extension; unknown → ["application/octet-stream"].
    Text types carry a charset. *)
val of_path : string -> string

(** Whether a content-type is worth gzip'ing (text-ish / json / wasm / svg) vs
    already-compressed media and fonts. Ignores any [;charset] parameter. *)
val compressible : string -> bool
