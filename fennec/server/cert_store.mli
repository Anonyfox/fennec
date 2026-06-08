(** Pluggable storage for the ACME account key + issued certificates. The right backing depends on
    the deployment: a writable volume (VM / docker volume / k8s PVC) → {!file}; an ephemeral or
    multi-replica deployment → an external shared store (implement {!t} over a k8s Secret / S3 /
    Redis / a DB) so a restart doesn't re-issue and replicas share one cert. The framework ships
    {!file} (default) + {!memory} and exposes the seam. *)

(** A cert store. Keys are opaque namespaced strings; values are PEM/JSON text. *)
type t = {
  get : string -> string option;
  put : string -> string -> unit;
  delete : string -> unit;
  with_lease : string -> (unit -> unit) -> bool;
      (** [with_lease key f] runs [f] holding a lease on [key] and returns [true]; returns [false]
          (without running [f]) if another holder has the lease — so N replicas don't all order
          certificates at once (Let's Encrypt's duplicate-cert rate limit is low). *)
}

(** [memory ()] — an in-process store. Dev / test / ephemeral (contents are lost on restart, so a
    fresh issue happens each boot). Single-process: the lease is always granted. *)
val memory : unit -> t

(** [file ~dir] — the default: PEM/JSON files under [dir] (created [0700]), written atomically and
    [0600]. Right for a persistent volume; survives restarts. The lease is a cross-process O_EXCL
    lockfile (sufficient on a shared POSIX volume). *)
val file : dir:string -> t
