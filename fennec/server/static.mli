(** Static file serving for a web root — disk (dev) or an embedded byte map
    (prod). Path-traversal-safe, with MIME, strong ETag + conditional 304, Range
    (206/416), Cache-Control, and a symlink-escape (realpath) guard for disk mode. *)

module H = Fennec_core.Http

(** A web root.
    - [Dir path]: read from disk; results cached by mtime+size; symlinks that
      resolve outside [path] are refused.
    - [Embedded lookup]: a path → bytes function baked into the binary. *)
type source = Dir of string | Embedded of (string -> string option)

(** Build a response for [req] against [src]. [None] means "no such asset"
    (caller falls through to pages / 404). A 403 is returned for an unsafe path. *)
val respond : ?cache_control:string -> source -> H.request -> H.response option

(** A request → optional-response function to wrap in [App.fallthrough]. *)
val handler : ?cache_control:string -> source -> H.request -> H.response option
