(* `fennec test new <cut> <name>` — scaffold a suite so even the first file in a new cut dir is
   zero-friction. Templates are pure; only {!create} touches disk. *)

(** The cuts that can be scaffolded: ["http"], ["browser"], ["system"]. *)
val cuts : string list

(** Create [test/<cut>/] under [cwd] — the convention dune + the one-line runner (only if absent,
    so a second [new] in the same cut just adds a suite) plus a starter [<name>_test.ml]. Returns
    the files created (cwd-relative), or a clear error (unknown cut, non-identifier name, or the
    suite file already exists). *)
val create : cwd:string -> cut:string -> name:string -> (string list, string) result
