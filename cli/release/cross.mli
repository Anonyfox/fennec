(** Cross-platform release builds via Docker — the engine behind [fennec release --target <platform>].

    OCaml native cross-compilation is the hard path, especially with C stubs (burrow's LMDB, the mongo
    FFI): there is no official support and it needs per-package overlay toolchains. So we do what
    [cross-rs] does for Rust — don't cross-compile, build {e natively inside a Docker image that IS the
    target platform} ([buildx --platform]), then extract the binary to disk ([--output type=local]) or
    tag a deployable runtime image. Docker reaches Linux targets (amd64/arm64); macOS and Windows have
    no buildable containers, so those need a native host or a CI matrix (see README.md). *)

(** A build target. [os] is always ["linux"] on the Docker path; [arch] is ["amd64"] | ["arm64"]. *)
type platform = {
  os : string;
  arch : string;
}

(** [slug p] is the [os-arch] form used as the per-target output directory under [./dist] (["linux-amd64"]). *)
val slug : platform -> string

(** [docker_platform p] is the [os/arch] form buildx expects (["linux/amd64"]). *)
val docker_platform : platform -> string

(** Where a cross build sends its result. *)
type output =
  | To_dir of string  (** extract the stripped binary into this directory (one per target) *)
  | To_image of string  (** build a slim runtime image and tag it this *)

(** The default build image: a self-contained [ocaml/opam] base the generated Dockerfile installs the
    rest of the toolchain into. Override with [--build-image] (e.g. a pre-baked image) for speed. *)
val default_base : string

(** [parse_targets specs] parses target strings — each may be comma-separated, e.g.
    ["linux/amd64,linux/arm64"] — into a deduped {!platform} list, or an actionable error. Only
    Docker-reachable Linux targets are accepted; [darwin/*] and [windows/*] return an error that points
    at a native host / CI matrix. *)
val parse_targets : string list -> (platform list, string) result

(** [docker_available ()] is [Ok ()] when [docker buildx] is usable, else an actionable error. *)
val docker_available : unit -> (unit, string) result

(** [build_dockerfile ~base ~exe_target ~name ~output] is the multi-stage Dockerfile: a [build] stage
    that compiles [exe_target] under [--profile release] inside the target image, then either a
    [scratch] [export] stage ({!To_dir}) or a slim [runtime] stage ({!To_image}). *)
val build_dockerfile : base:string -> exe_target:string -> name:string -> output:output -> string

(** [buildx_command ~platform ~dockerfile ~context ~output] is the [docker buildx build …] shell command
    that builds [dockerfile] for [platform] over the build [context] and emits per [output]. *)
val buildx_command : platform:platform -> dockerfile:string -> context:string -> output:output -> string
