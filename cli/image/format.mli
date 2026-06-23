(** The output image formats [fennec image] can encode to — a small closed variant, so the encode path
    is exhaustive (the compiler checks every format is handled) and an unknown format is rejected once,
    at the CLI boundary, never deep in the pipeline. AVIF is intentionally absent for now. *)
type t =
  | Jpeg
  | Png
  | Gif
  | Webp
  | Ico

(** All formats, in a stable order — for help text and enumeration. *)
val all : t list

(** The canonical lowercase extension, without the dot ([Jpeg -> "jpg"], [Webp -> "webp"], …). *)
val extension : t -> string

(** The human / CLI name (same as {!extension}). *)
val to_string : t -> string

(** Parse a format name — [jpg]/[jpeg]/[png]/[gif]/[webp]/[ico], case-insensitive — or [None]. *)
val of_string : string -> t option

(** Infer the format from a path's extension, or [None] if it has no extension or an unknown one. *)
val of_extension : string -> t option

(** The token the native engine expects (mirrors the Rust [encode] match; note [Jpeg -> "jpeg"], not
    its [.jpg] extension). *)
val to_engine : t -> string
