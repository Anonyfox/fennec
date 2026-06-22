(** The prod-lean scan: assert a built production binary links NONE of the heavy dev/test machinery.

    A native OCaml executable embeds the names of the modules it links, so scanning the binary bytes
    for a handful of dev/test module-name needles is a robust, tooling-free check (no [nm], no
    [objdump], no shell — it runs anywhere OCaml does). [fennec release] runs it on the artifact it is
    about to ship; the same invariant is enforced at [dune runtest] by the example's [check_lean]. *)

(** The forbidden module-name needles: the heavy e2e/CDP test machinery (Chrome CDP, TLS, websockets)
    and its yojson dependency. [Fennec_hunt_unit] (the ~1KB inline-test runtime) is deliberately {e
    not} here — it is inert in production and libraries carry their [let%test] registrations through
    it. (Mirrored in [examples/site/check_lean.ml], a zero-dependency guard that must not take a
    library dependency on the CLI; keep the two lists in sync.) *)
val forbidden : string list

(** A scan verdict: [Clean], or [Leaked needles] naming each forbidden module found in the binary. *)
type verdict =
  | Clean
  | Leaked of string list

(** [needles_in haystack] scans an in-memory string for {!forbidden} — the pure, testable core. *)
val needles_in : string -> verdict

(** [scan ~exe] reads the binary at [exe] and scans its bytes, or [Error msg] if it cannot be read. *)
val scan : exe:string -> (verdict, string) result
