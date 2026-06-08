(** Put the dirs holding the project's C-stub dlls — and the opam switch's stublibs — on
    [CAML_LD_LIBRARY_PATH], so a directly-spawned bytecode server can [dlopen] them (e.g. the mongo
    driver's libmongoc stub) without [dune exec] / [opam env]. *)

(** [ensure ()] augments [CAML_LD_LIBRARY_PATH] (idempotently; mutates this process's env, which
    spawned children inherit). Call it AFTER the build / at spawn time: the project's per-lib
    [dll*.so] dirs (under [_build/default]) are found by a directory walk, so they must already
    exist. Relies on the per-lib build dirs, not the install-staging dir (which only a full /
    [@install] build populates), so a targeted [fennec test] / [fennec dev] build is enough. *)
val ensure : unit -> unit
