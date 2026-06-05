(* A Host-header pattern, parsed into a total 3-way shape so matching never re-parses and
   overlapping patterns order deterministically. See host_pattern.ml. *)

type t =
  | Exact of string  (** a full normalized host, e.g. ["acme.com"] *)
  | Suffix of string  (** the dot-prefixed tail of ["*.x"], e.g. [".acme.com"]; matches >=1 leading label *)
  | Any  (** ["*"] — matches every host (at most one per router) *)

(** Normalize a request Host: lowercased, with any [":port"] suffix and a trailing ["."] removed. *)
val normalize : string -> string

(** Parse + classify a pattern. [Error msg] (human-readable) on empty, whitespace, or an illegal
    ['*'] (anything other than ["*"] alone or a single leading ["*."]). *)
val of_string : string -> (t, string) result

(** Render a pattern back to its source form (["*.acme.com"], ["*"], …). *)
val to_string : t -> string

(** Does [host] match this pattern? [host] is normalized internally. Total. *)
val matches : t -> host:string -> bool

(** Specificity rank — higher is more specific: any [Exact] > a longer [Suffix] > a shorter
    [Suffix] > [Any]. Lets a router resolve overlapping patterns by precision. *)
val specificity : t -> int
