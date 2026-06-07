(** A reversed-label trie for host-pattern matching — O(1) exact, O(label-depth) suffix. Built
    once at startup from a pre-validated pattern list; lookup is pure. See [host_trie.ml] for the
    algorithm. Used internally by {!Host_router}; the router's API does not expose the trie. *)

(** An immutable reversed-label trie mapping host patterns to endpoint payloads. *)
type 'ep t

(** Build a trie from [(pattern, payload)] pairs. [Any] patterns are silently skipped (the caller
    holds the default separately). The list must be pre-validated (no conflicting patterns). *)
val build : (Host_pattern.t * 'ep) list -> 'ep t

(** [lookup t ~host] normalizes [host] (strips port, lowercases) and walks the trie. Returns the
    most specific match: an exact match beats any wildcard; among wildcards the deepest (most
    labels) wins. [None] if nothing matches (the caller falls to the default). *)
val lookup : 'ep t -> host:string -> 'ep option
