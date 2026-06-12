(** $jsonSchema rendered from a codec's {!Codec.view} — the structural half of validation pushed
    into the DATABASE: install it and mongod rejects foreign writes violating the declared shape
    (minimongo enforces the identical rule in-engine). Renderable refinements translate via their
    hints; arbitrary checks are app-side-only by design. Objects deliberately leave
    [additionalProperties] unset (legacy fields keep docs writable — evolution tolerance); an [opt]
    field is simply absent from [required]. Pure rendering — installation rides the backend boot
    path next to index-ensure. *)

(** The schema document for a reflected shape. *)
val json_schema : Codec.view -> Bson.t

(** The [{ "$jsonSchema": … }] validator document mongod's create/collMod expects. *)
val validator : 'a Codec.t -> Bson.t
