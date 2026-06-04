(** Is a freshly-built artifact a complete, runnable image?

    A complete OCaml bytecode executable ends with the runtime magic ("Caml1999X…"); while dune
    is still writing the artifact during a rebuild, that trailer is not there yet. Checking it
    (a 12-byte read) lets the supervisor refuse to exec a half-written image — the actual cause
    of a "wrong magic number" crash under a rapid edit burst — without copying, waiting, or a
    temp file. *)

(** Does [path] currently end with the OCaml bytecode runtime magic? [false] if absent, too
    short, or unreadable. *)
val bytecode_ready : string -> bool
