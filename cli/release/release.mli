(** The [fennec release] pipeline, assembled: resolve the deployable, build it native under the release
    profile, verify it is prod-lean and (for a web app) that its web root is embedded, stage + strip the
    binary, and print the deploy contract. The CLI subcommand is a thin cmdliner wrapper over {!run};
    keeping the orchestration here lets other surfaces drive a release the same way. *)

type opts = {
  outdir : string;  (** where to stage the binary (the CLI defaults this to [dist]) *)
  docker : bool;  (** also write a runtime [./Dockerfile] *)
  strip : bool;  (** strip the staged binary (default [true]) *)
  check_only : bool;  (** verify (build + prod-lean + embed) but stage nothing and write nothing *)
}

(** Run the pipeline, streaming progress + the deploy contract (or an actionable error). Returns the
    process exit code: [0] on success, [1] on any failure (discovery, build, a prod-lean leak, a missing
    web-root embed, or staging). *)
val run : opts -> int
