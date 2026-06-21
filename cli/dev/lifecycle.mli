(** Occasional-use lifecycle helpers behind [fennec clean].

    [clean ~root ~db ~force ()] removes build artifacts: [dune clean] (which wipes [_build/default])
    plus the dev loop's isolated [_build/fastdev] (which [dune clean] alone misses). With [~db:true] it
    ALSO resets the dev database:
    - embedded default (no [MONGO_URL]) → deletes [.fennec/db] (every per-port dev DB);
    - explicit [burrow://] → deletes that engine's directory;
    - external mongo ([mongodb://]) → drops the DEV database via the driver — never the per-suite
      [fennec_test_*] databases.

    Build artifacts and the dev DB are separate concerns; the dev DB lives OUTSIDE [_build] precisely so
    a plain [clean] (or [dune clean]) never touches it. The dev DB and any test/e2e DB are always in
    disjoint locations, so a [--db] reset can never trip over a test run.

    Destructive DB resets are gated: [~force] bypasses the prompt, a TTY asks for confirmation, and a
    non-TTY (CI) without [~force] refuses (so a CI step never silently wipes data). *)
val clean : root:string -> db:bool -> force:bool -> unit -> unit
