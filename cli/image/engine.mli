(** Run an image operation through the vendored native engine (the Rust [image] crate + libwebp, via
    {!Fennec_buildkit.Image}). This is the one typed boundary: {!Format.t} + {!Op.t} in, encoded bytes
    out, failures as [result] rather than exceptions. *)

(** [process ~input ~format ~op] decodes [input] (any common format, detected by content), applies [op]
    (resize / fit), and encodes to [format]. [Error msg] on a decode/encode failure — corrupt input, an
    ICO frame over 256px, etc. *)
val process : input:bytes -> format:Format.t -> op:Op.t -> (bytes, string) result

(** Read a whole file as bytes, or [Error] on an I/O error. *)
val read_file : string -> (bytes, string) result

(** Write bytes to [path], creating any missing parent directories, or [Error] on an I/O error. *)
val write_file : string -> bytes -> (unit, string) result

(** [process_file ~input ~output ~format ~op] = read [input] → {!process} → write [output]. *)
val process_file : input:string -> output:string -> format:Format.t -> op:Op.t -> (unit, string) result
