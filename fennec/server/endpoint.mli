(** An endpoint: an app's identity (name + host patterns) and its two-phase paw pipeline.
    Ports are not part of the endpoint — the runtime routes by Host in prod and assigns localhost
    ports in dev.

    {b Always-phase} paws ({!pipe}, {!use}, {!get}, {!post}, {!app}, …) run on every request.
    {b Matched-phase} paws ({!pipe_matched}, {!use_matched}) run ONLY after an always-phase paw
    answered (a route matched) — this prevents the "404 becomes 401" class of bugs.

    For protected pages or APIs, define routes/mounts first and put auth, rate limiting, or
    business middleware in the matched phase. Use the always phase for infrastructure that really
    must see every request, such as logging, request ids, static files, and method override.

    For simple apps (no matched-phase paws) behaviour is identical to a flat pipeline. *)

(** Re-exports {!Fennec_paw.Paw} for convenience in endpoint definitions. *)
module Paw = Fennec_paw.Paw

(** A named, host-scoped application with a two-phase paw pipeline. *)
type t

(** [make ~name ?hosts ()] — a named endpoint answering the given host PATTERNS (validated later by
    {!Host_router}). [hosts] defaults to [["*"]], the catch-all/default app. [name] is mandatory:
    a stable handle for the dev banner, tests, and tooling. *)
val make : name:string -> ?hosts:string list -> unit -> t

(** {1 Always-phase — runs on every request, matched or not} *)

(** Append a reusable always-phase pipeline (a paw list). It runs even when no route matches. *)
val pipe : Paw.t list -> t -> t

(** Append a single always-phase paw. It runs even when no route matches. *)
val use : Paw.t -> t -> t

(** Prepend a paw so it runs before the rest (e.g. the dev livereload injector). *)
val prepend : Paw.t -> t -> t

(** Route shorthand: add a GET handler for [pattern] to the always-phase pipeline. *)
val get : string -> Paw.t -> t -> t

(** Route shorthand: add a POST handler for [pattern] to the always-phase pipeline. *)
val post : string -> Paw.t -> t -> t

(** Route shorthand: add a PUT handler for [pattern] to the always-phase pipeline. *)
val put : string -> Paw.t -> t -> t

(** Route shorthand: add a DELETE handler for [pattern] to the always-phase pipeline. *)
val delete : string -> Paw.t -> t -> t

(** Route shorthand: add a PATCH handler for [pattern] to the always-phase pipeline. *)
val patch : string -> Paw.t -> t -> t

(** Mount an SSR app. *)
val app : ?at:string -> (string -> string option) -> t -> t

(** {1 Matched-phase — runs only after a route in the always-phase answered} *)

(** Append a paw that runs only after a route matched. Auth, rate limiting, and business
    middleware belong here when protecting route handlers, SSR apps, or APIs — they should never
    fire on an unmatched request. *)
val use_matched : Paw.t -> t -> t

(** Append a pipeline that runs only after a route matched. Prefer this for a small stack such as
    session loading, login checks, authorization, and response decoration around matched routes. *)
val pipe_matched : Paw.t list -> t -> t

(** {1 Introspection} *)

(** The composed handler paw (always-phase → [if answered] matched-phase). *)
val handler : t -> Paw.t

(** The endpoint's stable name (as given to {!make}). *)
val name : t -> string

(** The host patterns this endpoint answers (as given to {!make}). *)
val hosts : t -> string list
