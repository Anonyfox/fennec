(** Process-wide Mongo runtime conventions shared by Fennec subsystems.

    Fennec has one data-location knob: [MONGO_URL]. A real MongoDB URI means "use Mongo"; the
    sentinel [":memory:"] means "use the in-process Mongo-shaped backend". An absent or blank
    [MONGO_URL] is a first-class [Missing] state: the process may boot, but database operations
    should fail with a clear configuration error. Test/dev orchestration opts into memory or real
    Mongo explicitly by setting this one variable before spawning an app. *)

(** Environment variable that carries the application's Mongo URL. *)
val mongo_url_env : string

(** Sentinel URL for the in-process Mongo-shaped backend. *)
val memory_url : string

(** Default Mongo database used by framework-owned collections. *)
val default_db : string

(** Resolved process state for framework-owned Mongo consumers. *)
type state =
  | Missing
  | Memory
  | Mongo of { uri : string; db : string }

(** [url ()] returns the trimmed [MONGO_URL], or [None] when it is absent/blank. *)
val url : unit -> string option

(** [db ()] returns [FENNEC_DB] when set/nonblank, otherwise ["fennec"]. *)
val db : unit -> string

(** [is_memory_url url] is [true] only for the [":memory:"] sentinel. *)
val is_memory_url : string -> bool

(** [state ()] classifies the current environment without inventing a fallback. *)
val state : unit -> state

(** Clear operation error for database-backed features when [state () = Missing]. *)
val unavailable_message : unit -> string

(** Print the missing-Mongo startup warning at most once per process. *)
val warn_if_missing : unit -> unit
