(** Case-insensitive operations over HTTP headers, kept as a plain [(name, value)]
    assoc list (small counts → a list is cache-friendly and beats a hashtable).
    Name comparison is case-insensitive without allocating per lookup.

    {[
      let h : Headers.t = [ ("Content-Type", "text/html") ] in
      let ct = Headers.get h "content-type" in          (* Some "text/html" *)
      let h = Headers.put h "Cache-Control" "no-store" in
      let h = Headers.add h "Set-Cookie" "sid=abc" in    (* repeatable field *)
      let cookies = Headers.get_all h "set-cookie"
    ]} *)

(** An HTTP header list — [(name, value)] pairs in wire order. Names are compared
    case-insensitively; the list representation is cache-friendly for typical small header counts. *)
type t = (string * string) list

(** Allocation-free case-insensitive string equality (exposed: header names use it). *)
val ci_equal : string -> string -> bool

(** The first value bound to a name (case-insensitive), if any. *)
val get : t -> string -> string option

(** Every value bound to a name, in order — for repeatable fields like [Set-Cookie]. *)
val get_all : t -> string -> string list

(** Whether a name is present. *)
val mem : t -> string -> bool

(** Remove every binding for a name. *)
val delete : t -> string -> t

(** Set a name to a single value, replacing any existing binding(s). *)
val put : t -> string -> string -> t

(** Append a binding, keeping existing ones (for repeatable fields). *)
val add : t -> string -> string -> t
