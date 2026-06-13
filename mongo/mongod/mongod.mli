(** mongod lifecycle management (native): launch and supervise a real MongoDB server for dev/test in
    pure OCaml over Unix. Fennec tests use explicit [MONGO_URL=:memory:] by default; this is the path
    to a real [mongod] for dev auto-management, real-Mongo tests, and the driver harness.

    A launched instance gets its own data directory and TCP port, is waited on until it actually
    {e accepts connections} (not merely spawned), and is stopped gracefully (SIGTERM, then SIGKILL
    after a grace period); ephemeral instances remove their data dir on stop.

    {[
      (* dev/test: own a real replica-set mongod (see cli/dev/mongo_rs.ml) *)
      match Mongod.find () with
      | None -> prerr_endline (Mongod.install_hint ())
      | Some _ ->
          let t = Mongod.start ?port ?dbpath ~replset:"rs0" () in
          Printf.printf "mongod ready at %s\n" (Mongod.uri t);
          Fun.protect ~finally:(fun () -> Mongod.stop t) (fun () -> run_against t)

      (* a test wants a throwaway instance, cleaned up even on failure *)
      let () = Mongod.with_ephemeral (fun t -> assert (Mongod.port t > 0))
    ]} *)

(** A running, supervised mongod instance. *)
type t

(** Raised by {!start} when no [mongod] binary can be found; carries {!install_hint}. *)
exception Not_installed of string

(** Raised when a spawned [mongod] exits or never accepts connections before the timeout; carries a
    tail of its log. *)
exception Launch_failed of string

(** [find ?extra ()] locates a [mongod] executable: the [extra] paths first, then each [PATH] entry,
    then common install locations (Homebrew, /usr/local, /usr, snap). [None] if none is executable. *)
val find : ?extra:string list -> unit -> string option

(** A human-readable, platform-specific hint for installing MongoDB (and a reminder that [:memory:]
    needs no mongod). *)
val install_hint : unit -> string

(** [start ?mongod ?port ?dbpath ?timeout ()] launches a mongod and blocks until it accepts
    connections (or [timeout] seconds, default 30, elapse — then it is killed and {!Launch_failed}
    is raised). [mongod] overrides the discovered binary; [port] defaults to a free loopback port;
    [dbpath] defaults to a fresh temp dir (marking the instance ephemeral — its data dir is removed
    on {!stop}). [replset] launches the node with [--replSet <name>] — a (single-node) replica set,
    which MongoDB requires for change streams; the caller then initiates it and waits for PRIMARY
    (e.g. {!Fennec_mongo_driver.Server.start} in reuse mode). @raise Not_installed if no binary is
    found. *)
val start :
  ?mongod:string -> ?port:int -> ?dbpath:string -> ?replset:string -> ?timeout:float -> unit -> t

(** The TCP port the instance listens on (loopback only). *)
val port : t -> int

(** The data directory. *)
val dbpath : t -> string

(** The OS process id. *)
val pid : t -> int

(** The path to the instance's [mongod.log] (a tail of it is included in {!Launch_failed}). *)
val logpath : t -> string

(** [uri t] is the connection string, [mongodb://127.0.0.1:<port>]. *)
val uri : t -> string

(** [stop t] stops the instance: SIGTERM, wait up to ~10s, then SIGKILL; an ephemeral instance's
    data directory is removed. Idempotent and thread-safe — safe to call more than once and from
    more than one thread (only the first call does the work). *)
val stop : t -> unit

(** [stop_all ()] stops every still-running instance. It is registered as an [at_exit] hook on the
    first {!start}, so a normal process exit — or an uncaught exception that unwinds to exit — never
    leaves a mongod behind. The CLI also tracks each pid in its signal reaper, so SIGINT/SIGTERM are
    covered too. (Only an uncatchable SIGKILL of the launcher can leak an instance, and because every
    instance uses a free port + a private data dir, a leaked one never collides with the next run.) *)
val stop_all : unit -> unit

(** [with_ephemeral ?mongod ?timeout f] starts a fresh ephemeral instance, runs [f] on it, and stops
    it (removing its data dir) even if [f] raises — the right tool for a test. *)
val with_ephemeral : ?mongod:string -> ?timeout:float -> (t -> 'a) -> 'a
