(** Resolve the single deployable server in the project — the production counterpart of what the dev
    supervisor discovers. Reuses {!Fennec_dev.Discover} (the one executable that calls [Fennec.serve]),
    but points at the {e native} [.exe] under [_build/default] rather than the dev [.bc], since a
    production build wants the optimized native binary. All paths are computed once, here, so the rest
    of the release pipeline never reconstructs a [_build/…] path itself. *)

type t = {
  root : string;  (** dune workspace root (absolute) *)
  name : string;  (** the server executable's name *)
  src_dir : string;  (** its source directory, workspace-relative *)
  exe_target : string;  (** the dune target to build, workspace-relative: [<src_dir>/<name>.exe] *)
  built_exe : string;  (** the built native binary, absolute, under [_build/default] *)
  assets_ml : string;  (** the generated [assets.ml] beside it (may be absent: an API/SSR-only app) *)
  webroot : string;  (** the assembled web-root dir beside it (may be absent) *)
}

(** Discover the deployable, or a clean, actionable error (zero / more than one server found — the same
    diagnostics {!Fennec_dev.Discover.find} gives, the caller frames it as a [fennec release] error). *)
val resolve : unit -> (t, string) result
