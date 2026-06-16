(** The validated Host→endpoint routing table. The only constructor is {!build}, which enforces the
    structural invariants — so a table that reaches the server is well-formed. Polymorphic over the
    endpoint payload so it's pure and testable in isolation. {!build} reports ALL errors at once so
    the developer fixes everything in one pass.

    {b Overlap is first-class.} Several endpoints may match the same host — same exact name, same
    wildcard, even several catch-alls (a downstream app split a large host into separate pipelines).
    {!route_all} returns every match, most-specific-first then declaration order, so the server can
    try them in turn (first to {e answer} wins; a non-matching one falls through).

    {[
      match Host_router.build
              [ ("web", [ "*" ], web_ep); ("admin", [ "admin.acme.com" ], admin_ep) ]
      with
      | Error errs -> failwith (Host_router.describe_errors errs)
      | Ok t -> Host_router.route_all t ~host:"admin.acme.com" (* [ admin_ep; web_ep ] *)
    ]} *)

(** One entry in the routing table: the endpoint's name, its parsed host patterns, and its payload. *)
type 'ep entry = { name : string; patterns : Host_pattern.t list; ep : 'ep }

(** A validated, immutable host-routing table. *)
type 'ep t

(** A routing configuration error detected by {!build}. *)
type error =
  | Bad_name of string  (** empty, or contains whitespace / ['='] *)
  | Duplicate_name of string  (** two endpoints share a name (names are identity) *)
  | No_patterns of string  (** an endpoint declared zero host patterns *)
  | Bad_pattern of string * string  (** endpoint name, the {!Host_pattern.of_string} error *)

(** Build a routing table from [(name, raw host patterns, payload)] entries in declaration order.
    Parses every pattern, then validates names (non-empty/clean/unique) and that each endpoint
    declares >=1 pattern. Overlapping patterns are allowed (see {!route_all}). Reports ALL errors at
    once (not just the first) so the developer fixes them in one pass. *)
val build : (string * string list * 'ep) list -> ('ep t, error list) result

(** Every endpoint matching the request Host, ordered most-specific-first (exact > longer wildcard
    suffix > catch-all ["*"]) and declaration order within a tier. The list the server walks: it runs
    each endpoint and the first to {e answer} wins; an endpoint that matches no route falls through to
    the next. Empty when nothing matches and there is no catch-all (the caller 404s). *)
val route_all : 'ep t -> host:string -> 'ep list

(** The single most-specific match — the head of {!route_all} (or [None]). *)
val route : 'ep t -> host:string -> 'ep option

(** The endpoints as declared (declaration order) — for dev port allocation and the banner. *)
val entries : 'ep t -> 'ep entry list

(** A human-readable explanation of a single {!build} error. *)
val describe_error : error -> string

(** Join all errors from a failed {!build} into one human-readable message (one per line). *)
val describe_errors : error list -> string
