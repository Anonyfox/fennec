(** The stub's write surface — {!Method.sim_writes} over the client merge store.
    A method stub's inserts/updates land as the sim sub's top-precedence field writes (instant UI),
    removes tombstone via {!Merge_store.sim_hide}, and the whole simulation rolls back to server
    truth with one [sub_stopped] when the method's [updated] arrives. Insert ids mint from the
    call's [seed] per collection, matching the server's seeded minting (STRING-id collections
    converge; a MONGO ObjectId insert swaps once at reveal). Pure — simulations unit-test natively. *)

(** Typed optimistic insert for stubs: validates with the SAME checks the server enforces (instant
    offline form errors, zero duplicated logic); an invalid value raises — contained by the
    stub-failure machinery (logged, simulation skipped, server decides). Empty [_id] = mint. *)
val insert_t : Method.sim_writes -> 'a Def.t -> 'a -> string

(** [writes box ~sim ~seed] registers [sim] (via {!Merge_store.begin_sim}) and returns the write
    surface bound to it. *)
val writes : Merge_store.t -> sim:string -> seed:string -> Method.sim_writes
