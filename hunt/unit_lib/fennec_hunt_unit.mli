(** Inline unit tests — the third hunt layer (alongside Http and Browser).

    {b With the ppx} ([fennec-hunt.ppx]) — the headline form:
    {[
      let%test "addition" = 1 + 1 = 2

      let%test_unit "decode round-trips" =
        Fennec_hunt.Unit.check_eq "decoded" ~expected:"a b" ~got:(percent_decode "a%20b")
    ]}

    {b Without the ppx}, register the same tests explicitly:
    {[
      let () = Fennec_hunt.Unit.test "addition" (fun () -> 1 + 1 = 2)
      let () = exit (Fennec_hunt.Unit.run ())
    ]}

    Tests register as module-init side effects; {!run} executes them, reports via the shared
    {!Reporter} caps, and returns an exit code. One runner per library, parallelised by dune
    across libraries. In production builds the ppx {b drops test bodies to [()]}, so there is
    zero runtime cost — no closures, no strings, no registration calls. *)

(** {2 Registration (called by user code or ppx-generated code)} *)

(** Register a boolean test. Fails if the body returns [false]. *)
val test : string -> (unit -> bool) -> unit

(** Register a unit test. Fails if the body raises. *)
val test_unit : string -> (unit -> unit) -> unit

(** Register a boolean test with source location (ppx-generated; prefer {!test} by hand). *)
val test_loc : name:string -> file:string -> line:int -> (unit -> bool) -> unit

(** Register a unit test with source location (ppx-generated). *)
val test_unit_loc : name:string -> file:string -> line:int -> (unit -> unit) -> unit

(** {2 Assertion helpers (for use inside [test_unit] / [let%test_unit] bodies)} *)

(** [check name cond] — fails with [name] if [cond] is [false]. *)
val check : string -> bool -> unit

(** [check_eq name ~expected ~got] — fails with an expected/got diff. Both values are
    converted to strings by the caller (the ppx can insert [string_of_*] or a generic
    [Printexc.to_string] for polymorphic values). *)
val check_eq : string -> expected:string -> got:string -> unit

(** {2 Test helpers} *)

(** [str_contains hay needle] — substring search. Useful in [let%test] for checking a
    string contains a fragment without pulling in a regex library. *)
val str_contains : string -> string -> bool

(** {2 Execution} *)

(** Run every registered test. Returns [0] if all passed, [1] otherwise. Filters by
    [--grep] from [Sys.argv] (substring match on the test name, same semantics as
    {!Http} and {!Live}). *)
val run : unit -> int

(** How many tests are registered (for diagnostics / dry-run). *)
val count : unit -> int
