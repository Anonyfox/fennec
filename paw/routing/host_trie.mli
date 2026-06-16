(** A reversed-label trie for host-pattern matching — O(1) exact, O(label-depth) suffix. Built
    once at startup from a pre-validated pattern list; lookup is pure. See [host_trie.ml] for the
    algorithm. Used internally by {!Host_router}; the router's API does not expose the trie.

    {[
      (* Host_router.build feeds the validated, Any-stripped patterns in: *)
      let trie = Host_trie.build [ (Host_pattern.Exact "admin.acme.com", admin_ep) ] in
      Host_trie.lookup trie ~host:"admin.acme.com" (* Some admin_ep *)
    ]} *)

(** An immutable reversed-label trie mapping host patterns to endpoint payloads. *)
type 'ep t

(** Build a trie from [(pattern, payload)] pairs, in declaration order. [Any] patterns are silently
    skipped (the caller holds the default separately). Overlap is allowed — a node may carry several
    payloads (exact and/or wildcard), kept in declaration order. *)
val build : (Host_pattern.t * 'ep) list -> 'ep t

(** [lookup_all t ~host] normalizes [host] (strips port, lowercases) and returns EVERY matching
    payload, most-specific-first: the exact matches at the host's node, then the wildcard matches
    from the deepest node up to the shallowest, each level in declaration order. [\[\]] if nothing
    matches. *)
val lookup_all : 'ep t -> host:string -> 'ep list

(** [lookup t ~host] is the single most-specific match — the head of {!lookup_all} (or [None]). *)
val lookup : 'ep t -> host:string -> 'ep option
