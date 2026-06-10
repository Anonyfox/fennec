open Discover_model

(** A ranked public API candidate. [coverage] is the fraction of normalized query
    terms matched by the API text or its linked evidence. *)
type api_result = {
  item : public_item;
  score : float;
  coverage : float;
}

(** A ranked source-backed proof item: example, test, doctest, or generated route
    fact. [coverage] is the fraction of normalized query terms matched by this
    evidence item. *)
type evidence_result = {
  ev : evidence;
  score : float;
  coverage : float;
}

val apis : snapshot -> string list -> api_result list
(** Rank public APIs using fielded lexical search, character n-gram similarity,
    and source-evidence graph propagation. *)

val find_api : snapshot -> string -> public_item option
(** Resolve a discover API id or public path through the snapshot's runtime id table. *)

val evidence : snapshot -> string list -> api_result list -> evidence_result list
(** Rank examples, tests, doctests, and generated route facts for the current
    query and selected API candidates. *)
