(** [fennec agent selftest]: exercise the emit→hook loop for every registered adapter against a
    throwaway journal, so a harness's wiring (its injection shape especially) can't silently rot.
    Returns [(harness id, passed)] per adapter. Offline — no dev server, no real harness, no global
    config touched. *)

(** Run the offline round trip for every adapter and return [(harness id, passed)] for each. *)
val run : unit -> (string * bool) list
