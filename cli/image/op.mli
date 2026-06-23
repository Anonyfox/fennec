(** The transform options for one image operation — a typed record the pipeline is a pure function of.
    Serialised to the engine's [k=v;…] wire string at the FFI boundary (the only place strings appear). *)

(** How a both-dimensions resize fills the target box. *)
type fit =
  | Contain  (** fit inside the box, preserving aspect ratio (no crop) — the default *)
  | Cover  (** fill the box exactly, center-cropping the overflow *)

type t = {
  resize : Geometry.t option;  (** [None] keeps the source dimensions *)
  fit : fit;
  quality : int option;  (** 1–100 for lossy formats (jpeg/webp); [None] uses the per-format default *)
  strip : bool;  (** drop metadata (EXIF/XMP/ICC) — re-encoding already emits none, this makes it explicit *)
}

(** No resize, [Contain], default quality, no explicit strip. *)
val default : t

(** The engine wire string, e.g. ["w=800;h=600;fit=cover;q=75"] — only the set keys, in a stable order. *)
val to_opts : t -> string
