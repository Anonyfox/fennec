(** The [fennec release] pipeline, assembled: resolve the deployable, build it native under the release
    profile, verify it is prod-lean and (for a web app) that its web root is embedded, stage + strip the
    binary, and print the deploy contract. The CLI subcommand is a thin cmdliner wrapper over {!run};
    keeping the orchestration here lets other surfaces drive a release the same way. *)

type opts = {
  outdir : string;  (** where to stage the binary (the CLI defaults this to [dist]) *)
  docker : bool;  (** for a host build, also write a runtime [./Dockerfile] *)
  strip : bool;  (** strip the staged binary (default [true]) *)
  check_only : bool;  (** verify (build + prod-lean + embed) but stage nothing and write nothing *)
  targets : string list;  (** [--target] platforms (e.g. ["linux/amd64"]); empty ⇒ build for THIS host *)
  image : string option;  (** [--image TAG]: with [targets], emit a runtime image instead of a loose binary *)
  build_image : string option;  (** [--build-image]: the cross-build base image (default {!Cross.default_base}) *)
}

(** Run the pipeline, streaming progress + the deploy contract (or an actionable error). With no
    [targets] this builds for the current host: filesystem side effects relative to the cwd (the project
    root) — it stages the stripped binary under [opts.outdir] (default [dist]) and, with [opts.docker],
    writes a [./Dockerfile]. With [targets], it cross-builds via Docker instead (see {!Cross}): each
    target's binary lands in [<outdir>/<os-arch>/], or — with [image] — is tagged as a runtime image.
    Returns the process exit code: [0] on success, [1] on any failure (discovery, build, a prod-lean
    leak, a missing web-root embed, staging, or a failed cross target). *)
val run : opts -> int
