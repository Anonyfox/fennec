(* Discover + build the suite executables for a cut, by directory convention
   ([test/http/*.ml], [test/browser/*.ml]). Path logic is pure; readdir + dune build are I/O. *)

type t = {
  name : string;    (** suite name (source basename without [.ml]) *)
  target : string;  (** dune build target, cwd-relative *)
  exe : string;     (** absolute path to the built artifact *)
}

(** [cwd] relative to the workspace [root] ("" when equal, or when cwd is not under root). Pure. *)
val relativize : root:string -> cwd:string -> string

(** The built artifact path: [<root>/_build/default/<reldir>/<dir>/<name>.exe]. Pure. *)
val exe_path : root:string -> reldir:string -> dir:string -> name:string -> string

(** Discover the suites in [<cwd>/<dir>], sorted (deterministic); [] if the directory is absent. *)
val discover : root:string -> cwd:string -> dir:string -> t list

(** Build the suite artifacts in one [dune build]; dune's own errors surface to the user. *)
val build : t list -> (unit, string) result
