open Discover_model

(** Pick the APIs that should be shown in a discover card.

    Retrieval can return many correct-but-noisy candidates. Selection is the
    presentation stage: it prefers public facade APIs, avoids near-duplicate
    helpers, keeps distinct families visible, and lets strongly linked evidence
    pull exact action APIs such as [set_cookie] or [signal] into the card. *)
val plan_uses :
  terms:string list ->
  more:bool ->
  api_results:Retrieve.api_result list ->
  evidence_seed_items:public_item list ->
  public_items:public_item list ->
  public_item list

(** Pick the two representatives for an explicit comparison query. *)
val compare_pair :
  task:string ->
  terms:string list ->
  uses:public_item list ->
  public_items:public_item list ->
  (public_item * public_item) option
