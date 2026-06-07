(** Property-based testing — the [let%prop] surface (a thin, lean layer over QCheck2).

    A property test asserts an invariant holds for {e many automatically-generated} inputs, and
    on failure {e shrinks} the counterexample to its minimal form. Where a unit test checks one
    example, a property checks a hundred and hands you the smallest input that breaks it.

    {2 The [let%prop] ppx}

    The headline form is {b type-driven}: annotate the arguments and the generator {e and} the
    counterexample printer are both derived from the types, so a property reads like a spec —
    no generator boilerplate, and failures print the offending value for free:

    {@ocaml skip[
      let%prop "reversing a list twice is the identity" = fun (l : int list) ->
        List.rev (List.rev l) = l

      let%prop "append lengths add" = fun (a : string list) (b : string list) ->
        List.length (a @ b) = List.length a + List.length b
    ]}

    Supported argument types — and any [list] / [array] / [option] / tuple nesting of them:
    [int], [bool], [char], [string], [float]. Up to four arguments (they are tupled for you).
    The body returns [bool]; use {!assume} for a precondition. A [let%prop] is registered and run
    exactly like a [let%test] — swept by [fennec test], re-run by the dev loop's inline lane, and
    stripped to nothing in a production build (so [qcheck-core] is a test-only weight).

    {2 Custom generators}

    When you need a generator a type can't express — a numeric range, a constrained value — drop
    to the explicit form with {!forall} ([open Fennec_hunt.Prop] for [forall] / {!Gen} / {!Print}):

    {@ocaml skip[
      open Fennec_hunt.Prop
      let%prop "clamp stays within bounds" =
        forall ~print:Print.int Gen.(int_range 0 1000) (fun n ->
          let c = clamp ~lo:10 ~hi:90 n in c >= 10 && c <= 90)
    ]}

    Pass [~print] in the explicit form so a failing case is shown (the type-driven form derives
    it for you). *)

(** QCheck2 generator combinators ([int], [string], [list], [int_range], [pair], …). *)
module Gen = QCheck2.Gen

(** QCheck2 value printers, for readable counterexamples ([int], [list], [pair], …). *)
module Print = QCheck2.Print

(** [forall ?count ?print gen prop] builds a property: [prop] must return [true] for every value
    [gen] produces ([count] cases, default 100). [~print] makes a failing case readable. The
    result is the payload of an explicit [let%prop "name" = forall …]. *)
val forall : ?count:int -> ?print:('a -> string) -> 'a Gen.t -> ('a -> bool) -> QCheck2.Test.t

(** [assume cond] discards the current generated case — it does {e not} count as a failure —
    unless [cond] holds. For conditional properties, e.g. [assume (b <> 0)] before dividing by
    [b]. Raises internally; only meaningful inside a property body. *)
val assume : bool -> unit

(** [check ~name ?print gen prop] runs a property and raises on the (shrunk) counterexample.
    {b ppx-generated} by the type-driven [let%prop]; not meant to be called by hand. *)
val check : name:string -> ?print:('a -> string) -> 'a Gen.t -> ('a -> bool) -> unit

(** [check_named ~name test] names [test] and runs it, raising on the (shrunk) counterexample.
    {b ppx-generated} by the explicit [let%prop = forall …]; not meant to be called by hand. *)
val check_named : name:string -> QCheck2.Test.t -> unit

(** Adapt a 2-argument curried predicate to a tupled one. {b ppx-generated} for a 2-argument
    type-driven [let%prop]; not meant to be called by hand. *)
val uncurry2 : ('a -> 'b -> 'c) -> 'a * 'b -> 'c

(** Adapt a 3-argument curried predicate to a tupled one. {b ppx-generated}; not for direct use. *)
val uncurry3 : ('a -> 'b -> 'c -> 'd) -> 'a * 'b * 'c -> 'd

(** Adapt a 4-argument curried predicate to a tupled one. {b ppx-generated}; not for direct use. *)
val uncurry4 : ('a -> 'b -> 'c -> 'd -> 'e) -> 'a * 'b * 'c * 'd -> 'e
