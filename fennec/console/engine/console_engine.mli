(** The in-server console eval engine — hosts the OCaml toplevel {e inside} the running app server, so a
    REPL evaluates against the server's own linked modules and {b live state} (the open burrow, booted
    Accounts, in-flight fibers). The local half of the [iex -S mix phx.server] model.

    This library is {b dev-only and bytecode-only} (it links the compiler). It registers itself with
    {!Fennec.set_console_hook} at load time, so simply linking it into the dev server (with [-linkall],
    so the toplevel can resolve every linked module) is all the wiring needed; the production native
    build links neither this nor the compiler. A frontend drives it over {!Console_protocol}.

    The engine self-gates on [FENNEC_CONSOLE_SOCK]: with no socket path it stays idle (a server started
    by a bare [dune exec], not the fennec tooling, opens no console). *)

(** The starter fennec invokes (it has the {!Fennec.console_start} type). Forks the eval-socket listener
    into [sw] when [FENNEC_CONSOLE_SOCK] is set; otherwise a no-op. Exposed mainly for documentation and
    testing — the registration happens automatically when this module is linked. *)
val start : Fennec.console_start

(** [eval src] evaluates one (or several [;;]-separated) toplevel phrase(s) against the live process and
    returns the rendered output (the result line [- : t = v], type errors, raised exceptions) together
    with whether it was an error. The phrase's own [print_*] side effects are {e not} captured here —
    they flow to the process stdout (which the driver renders) so concurrent server logs are never
    swallowed. Exposed for testing. *)
val eval : string -> string * bool

(** [complete prefix] returns completion candidates for the identifier [prefix] (dotted paths resolve
    against the toplevel environment). Best-effort: any compiler-API hiccup yields [[]]. *)
val complete : string -> string list
