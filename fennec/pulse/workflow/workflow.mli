(** The workflow runtime — the machinery behind the [@workflow] ppx. Userland writes ordinary functions
    tagged [@workflow] (transactional) and reactions tagged [@after f] / [@before f]; the ppx lowers
    each workflow to {!exec} plus two typed registration lists, so the function reads like a normal
    function and the transaction + reactions stay transparent. You rarely call this directly. *)

(** Raised when a workflow is re-entered within its own reaction cascade — the runtime half of the
    circuit-breaker (the compile-time half rejects cyclic [@after]/[@before] graphs). Contained by the
    after-hook isolation, so it breaks the loop rather than crashing. The argument is the workflow's
    name. *)
exception Cyclic_reaction of string

(** [exec ~name ~befores ~afters arg body] is what a [@workflow] function compiles to: run [befores arg]
    then [body ()] in ONE transparent transaction (commit on return, roll back on raise); on commit fire
    [afters result] — deferred to the outermost commit, isolated. Re-entrancy raises {!Cyclic_reaction}. *)
val exec :
  name:string -> befores:('a -> unit) list -> afters:('r -> unit) list -> 'a -> (unit -> 'r) -> 'r
