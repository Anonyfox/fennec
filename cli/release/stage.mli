(** Copy the built binary out of [_build/] into a deployable location and (by default) strip it.

    dune marks its outputs read-only and leaves the native link's debug sections in place; we copy the
    artifact to [<outdir>/<name>], make it writable + executable, and [strip] it (what the CI release
    workflow does by hand). The release is a single self-contained binary — for a web app the assets
    are embedded — so nothing else needs staging. *)

type staged = {
  path : string;  (** the staged binary's path, e.g. [./dist/server] *)
  bytes : int;  (** its size on disk, for the report *)
}

(** [run ~built_exe ~outdir ~name ~strip] copies [built_exe] to [<outdir>/<name>] (creating [outdir]),
    chmods it executable, and strips it when [strip] is set. A failing/absent [strip] is a warning, not
    an error — the (unstripped) binary is still valid. [Error msg] only on a copy failure. *)
val run : built_exe:string -> outdir:string -> name:string -> strip:bool -> (staged, string) result
