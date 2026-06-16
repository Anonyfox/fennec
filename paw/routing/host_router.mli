(** The validated Host→endpoint routing table. The only constructor is {!build}, which enforces
    every routing invariant — so a table that reaches the server is provably well-formed and {!route}
    is total. Polymorphic over the endpoint payload so it's pure and testable in isolation. {!build}
    reports ALL errors at once so the developer fixes everything in one pass.

    {[
      match Host_router.build
              [ ("web", [ "*" ], web_ep); ("admin", [ "admin.acme.com" ], admin_ep) ]
      with
      | Error errs -> failwith (Host_router.describe_errors errs)
      | Ok t -> Host_router.route t ~host:"admin.acme.com" (* Some admin_ep *)
    ]} *)

(** One entry in the routing table: the endpoint's name, its parsed host patterns, and its payload. *)
type 'ep entry = { name : string; patterns : Host_pattern.t list; ep : 'ep }

(** A validated, immutable host-routing table. *)
type 'ep t

(** A routing configuration error detected by {!build}. *)
type error =
  | Bad_name of string  (** empty, or contains whitespace / ['='] *)
  | Duplicate_name of string
  | No_patterns of string  (** an endpoint declared zero host patterns *)
  | Bad_pattern of string * string  (** endpoint name, the {!Host_pattern.of_string} error *)
  | Multiple_catch_all of string list  (** more than one endpoint declared ["*"] *)
  | Conflicting_pattern of string * string * string  (** the pattern, and the two endpoint names claiming it *)

(** Build a routing table from [(name, raw host patterns, payload)] entries in declaration order.
    Parses every pattern, then validates: names non-empty/clean/unique, each endpoint has >=1
    pattern, at most one catch-all ["*"], and no two endpoints claim the same pattern. Reports ALL
    errors at once (not just the first) so the developer fixes them in one pass. *)
val build : (string * string list * 'ep) list -> ('ep t, error list) result

(** Resolve a request Host to an endpoint payload: the most specific matching pattern wins, else the
    single catch-all default, else [None] (unknown host with no default → the caller should 404). *)
val route : 'ep t -> host:string -> 'ep option

(** The endpoints as declared (declaration order) — for dev port allocation and the banner. *)
val entries : 'ep t -> 'ep entry list

(** A human-readable explanation of a single {!build} error. *)
val describe_error : error -> string

(** Join all errors from a failed {!build} into one human-readable message (one per line). *)
val describe_errors : error list -> string
