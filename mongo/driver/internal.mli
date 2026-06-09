(** The bridge between Eio's scheduler and the blocking C driver. *)

(** [run f] runs the blocking [f] in an Eio systhread, so a libmongoc call (which releases the OCaml
    runtime lock) yields to the scheduler instead of freezing it — the reason a blocking
    change-stream read does not stall the whole application. *)
val run : (unit -> 'a) -> 'a
