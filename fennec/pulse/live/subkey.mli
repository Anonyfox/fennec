(** The deterministic subscription key (name + params) — shared so an SSR hydration payload lines up
    with what the client looks up. Dependency-free string key; pure → native and browser. *)

(** [bkey b] — a stable string key for a BSON value (the building block of {!key}). *)
val bkey : Bson.t -> string

(** [key name params] — the subscription key for publication [name] with [params]. *)
val key : string -> Bson.t list -> string

(** The collection a publication feeds, by convention [name → name] (override at the find call site
    when a publication feeds a differently-named collection). *)
val collection_of_name : string -> string
