(** The one database knob: [MONGO_URL], parsed once into a structured backend choice. Three schemes,
    mirroring the SQLite -> server progression:
    - [:memory:] — the in-process Mongo-shaped store (minimongo); ephemeral (unit tests).
    - [burrow://\[user:pass@\]\[host\[:port\]\]/<abs-path>\[?tls&readonly\]] — the embedded on-disk engine.
      It is the mirror image of [mongodb://]: the authority is the coordinates the {e hosted} embedded
      server listens on (absent => storage only, no wire endpoint), and the path's trailing segment is
      the database name {e and} its on-disk directory ([mongosh use other] => a sibling dir under the
      same base). Paths are absolute.
    - [mongodb://…] / [mongodb+srv://…] — a real MongoDB server (native driver).

    An absent/blank [MONGO_URL] is a first-class [Missing] state. Pure string parsing — no IO, no driver.

    {[ match backend () with
       | Memory -> in_memory ()
       | Burrow { base; db; expose } -> open_embedded ~dir:(Filename.concat base db) ?expose ()
       | Mongo { uri; db } -> connect ~uri ~db
       | Missing -> failwith (unavailable_message ()) ]} *)

val mongo_url_env : string
(** The environment variable that carries the application's database URL: ["MONGO_URL"]. *)

val memory_url : string
(** The [:memory:] sentinel for the in-process backend. *)

val default_db : string
(** The default database name (["fennec"]) when neither the URL nor [FENNEC_DB] provides one. *)

val default_wire_port : int
(** The default mongosh wire-endpoint port (27017, the official MongoDB port). *)

(** The optional mongosh wire endpoint requested by a [burrow://] authority. *)
type expose = {
  host : string;  (** bind address (default 127.0.0.1) *)
  port : int;  (** wire port (default 27017) *)
  user : string option;  (** SCRAM username — creds present => auth required *)
  pass : string option;
  tls : bool;  (** [?tls=true] — reuse the app's TLS certificate *)
  read_only : bool;  (** [?readonly=true] *)
}

(** The resolved backend choice. For [Burrow], the engine directory is [Filename.concat base db]. *)
type backend =
  | Missing
  | Memory
  | Burrow of { base : string; db : string; expose : expose option }
  | Mongo of { uri : string; db : string }

val url : unit -> string option
(** The trimmed [MONGO_URL], or [None] when absent/blank. *)

val db : unit -> string
(** [FENNEC_DB] when set/nonblank, else {!default_db}. The fallback db for [:memory:] and for a
    [mongodb://] URI with no database component. *)

val is_memory_url : string -> bool
(** [true] only for the [:memory:] sentinel. *)

val backend : unit -> backend
(** Classify [MONGO_URL] into the backend choice, without inventing a fallback. *)

val unavailable_message : unit -> string
(** The hard error a database-backed feature raises when [backend () = Missing]. No soft degradation. *)
