(* An endpoint: an app's identity (name + host patterns) and behavior (a two-phase paw pipeline).
   Ports are not here — the runtime routes by Host in prod and assigns localhost ports in dev.

   {b Always-phase} paws ({!pipe}, {!use}, {!get}, {!post}, {!app}, …) run on every request.
   {b Matched-phase} paws ({!pipe_matched}, {!use_matched}) run ONLY after an always-phase paw
   answered the conn (a route matched). This prevents the "404 becomes 401" bug class: auth
   middleware in the matched phase never fires on a request that didn't match any route.

   For simple apps (no matched-phase paws), behavior is identical to a flat pipeline. *)

module Paw = Fennec_paw.Paw

type t

(** [make ~name ?hosts ()] — a named endpoint answering the given host PATTERNS (validated later by
    {!Host_router}). [hosts] defaults to [["*"]], the catch-all/default app. [name] is mandatory:
    a stable handle for the dev banner, tests, and tooling. *)
val make : name:string -> ?hosts:string list -> unit -> t

(** {1 Always-phase — runs on every request, matched or not} *)

(** Append a reusable pipeline (a paw list). *)
val pipe : Paw.t list -> t -> t

(** Append a single paw. *)
val use : Paw.t -> t -> t

(** Prepend a paw so it runs before the rest (e.g. the dev livereload injector). *)
val prepend : Paw.t -> t -> t

val get : string -> Paw.t -> t -> t
val post : string -> Paw.t -> t -> t
val put : string -> Paw.t -> t -> t
val delete : string -> Paw.t -> t -> t
val patch : string -> Paw.t -> t -> t

(** Mount an SSR app. *)
val app : ?at:string -> (string -> string option) -> t -> t

(** {1 Matched-phase — runs only after a route in the always-phase answered} *)

(** Append a paw that runs only after a route matched. Auth, rate limiting, and business
    middleware belong here — they should never fire on an unmatched request. *)
val use_matched : Paw.t -> t -> t

(** Append a pipeline that runs only after a route matched. *)
val pipe_matched : Paw.t list -> t -> t

(** {1 Introspection} *)

(** The composed handler paw (always-phase → [if answered] matched-phase). *)
val handler : t -> Paw.t

val name : t -> string
val hosts : t -> string list
