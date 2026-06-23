(** Small filesystem helpers shared across the release pipeline, so {!Lean}, {!Verify} and {!Stage}
    don't each re-roll the same primitives. (The asset bundler in [cli/webroot.ml] has equivalents but
    lives in the [fennec] {e executable}, not a library, so it can't be imported here — see README.md.) *)

(** [read_file path] reads the whole file at [path] as a binary string. *)
val read_file : string -> string

(** [mkdir_p dir] creates [dir] and any missing parents; a no-op if it already exists. *)
val mkdir_p : string -> unit

(** [count_files dir] is the number of regular files under [dir], recursively. Best-effort: an
    unreadable directory contributes [0]. *)
val count_files : string -> int
