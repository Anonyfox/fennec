(* An endpoint: an app's identity (name + host patterns) and behavior (a paw pipeline). Ports are
   not here — the runtime routes by Host in prod and assigns localhost ports in dev. See endpoint.ml. *)

module Paw = Fennec_paw.Paw

type t

(** [make ~name ?hosts ()] — a named endpoint answering the given host PATTERNS (validated later by
    {!Host_router}). [hosts] defaults to [["*"]], the catch-all/default app. [name] is mandatory:
    a stable handle for the dev banner, tests, and tooling. *)
val make : name:string -> ?hosts:string list -> unit -> t

(** Append a reusable pipeline (a paw list). *)
val pipe : Paw.t list -> t -> t

(** Append a single paw (e.g. a prebuilt battery). *)
val use : Paw.t -> t -> t

(** Prepend a paw so it runs before the rest (e.g. the dev livereload injector). *)
val prepend : Paw.t -> t -> t

val get : string -> Paw.t -> t -> t
val post : string -> Paw.t -> t -> t
val put : string -> Paw.t -> t -> t
val delete : string -> Paw.t -> t -> t
val patch : string -> Paw.t -> t -> t

(** Mount an SSR app: [render : path -> html option] answers with a document when it matches (under
    optional prefix [at], default ["/"]), else declines so static/404 follow. *)
val app : ?at:string -> (string -> string option) -> t -> t

(** The composed handler paw for this endpoint. *)
val handler : t -> Paw.t

(** The endpoint's name. *)
val name : t -> string

(** The endpoint's raw host patterns (as declared). *)
val hosts : t -> string list
