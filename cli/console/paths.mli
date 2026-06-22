(** Deterministic console paths, shared by the side that LISTENS (the in-server engine, via the env it
    is handed) and the side that CONNECTS (the [fennec console] command, and a future attach). Deriving
    the socket path from the project root means a console started for a project always finds the same
    one — the basis for "attach to the running one if it exists". *)

(** [socket ~root] — the unix-socket path the console engine listens on for [root]. A short name under
    the temp dir (socket paths are length-capped), keyed by a digest of the absolute root. *)
val socket : root:string -> string
