(* Find THE server in a dune project — the single executable that calls [Fennec.serve] — with no
   config and no folder conventions. See discover.ml for the rationale. *)

type t = {
  root : string; (** dune workspace root (absolute) *)
  name : string; (** the server executable's name *)
  src_dir : string; (** its source directory, workspace-relative *)
  exe : string; (** the built bytecode, absolute: …/_build/default/<src_dir>/<name>.bc *)
  targets : string list; (** the dune target(s) to build and watch for it *)
}

(** Discover the single server executable in the cwd subtree, or a clean, actionable error message
    (zero found → how to point at it explicitly; more than one → which, and how to disambiguate). *)
val find : unit -> (t, string) result

(** Does this OCaml source START a server (call [Fennec.serve])? A discovery heuristic: comments and
    string literals are stripped first, so a mention of "serve" in prose is not a call, and "serve"
    matches only as a whole identifier (not "preserve"). Pure; unit-tested. The runtime is the real
    guarantee — {!Fennec.serve} refuses a second call regardless. *)
val calls_serve : string -> bool
