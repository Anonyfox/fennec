(** A resize request, parsed from the [-r]/[--resize] geometry — a closed variant so resize handling is
    total. One way to ask for a resize; the [--fit] flag decides how a both-dimensions box is filled. *)
type t =
  | Width of int  (** [-r 800] or [-r 800x] — fix the width, scale the height to keep aspect ratio *)
  | Height of int  (** [-r x600] — fix the height, scale the width *)
  | Box of int * int  (** [-r 800x600] — fit within (or, with [--fit cover], fill) this box *)

(** Parse [W] / [WxH] / [Wx] / [xH] into a request, or [Error msg] on a malformed / non-positive value. *)
val of_string : string -> (t, string) result
