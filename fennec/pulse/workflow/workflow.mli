(** Workflows — ordinary functions that run inside the transparent transaction and carry their
    reactions. A [('a, 'r) t] is a named function ['a -> 'r] plus [before] guards (run in the
    transaction; a raise vetoes the whole call and rolls it back) and [after] hooks (run post-commit,
    isolated). Reactions are stored on the value, so [@after Orders.place] is a normal type-checked
    cross-module reference — there is no stringly event registry. The reactions ppx desugars
    [@before f] / [@after f] into {!before} / {!after} [f]. *)

type ('a, 'r) t

(** [make name fn] wraps [fn] as a workflow. [name] is used in diagnostics and the wiring manifest. *)
val make : string -> ('a -> 'r) -> ('a, 'r) t

(** [call t x] runs the workflow: its [before] guards then its body, all in ONE transaction (commit
    on return, roll back on raise). On commit its [after] hooks fire — deferred to the outermost
    commit (so a nested workflow's reactions fire only once the whole transaction is durable) and
    isolated from one another. *)
val call : ('a, 'r) t -> 'a -> 'r

(** The workflow's name. *)
val name : ('a, 'r) t -> string

(** The bare underlying function — runs the body WITHOUT the transaction bracket or reactions. For
    composing a workflow's logic into another without re-entering its hooks; rarely needed (prefer
    {!call}, which nests/joins the enclosing transaction). *)
val fn : ('a, 'r) t -> ('a -> 'r)

(** [before t guard] registers an in-transaction pre-condition: [guard x] runs before the body on
    every [call t x] and may [raise] to veto it (the transaction rolls back; no after-hook fires). *)
val before : ('a, 'r) t -> ('a -> unit) -> unit

(** [after t hook] registers a post-commit reaction: [hook result] runs after [call t] commits. *)
val after : ('a, 'r) t -> ('r -> unit) -> unit
