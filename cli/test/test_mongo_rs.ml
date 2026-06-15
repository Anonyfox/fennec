(* `fennec dev` defaults the Mongo backend to the embedded engine. ensure_dev points MONGO_URL at a
   per-base-port :embedded: directory when it is unset (no mongod), keeps parallel base ports isolated,
   and leaves an explicit MONGO_URL untouched (real mongo / :memory: / :embedded: all win). *)
module M = Fennec_dev.Mongo_rs

let () =
  let saved = Sys.getenv_opt "MONGO_URL" in
  Fun.protect
    ~finally:(fun () -> Unix.putenv "MONGO_URL" (Option.value saved ~default:""))
    (fun () ->
      (* unset -> embedded default at a per-base-port dir; nothing to own (None) *)
      Unix.putenv "MONGO_URL" "";
      assert (Option.is_none (M.ensure_dev ~root:"/tmp/fennec_evt" ~base_port:4000 ()));
      assert (Sys.getenv_opt "MONGO_URL" = Some ":embedded:/tmp/fennec_evt/_build/.fennec/burrow/dev-4000");

      (* a different base port -> a different dir (so parallel dev / e2e instances stay isolated) *)
      Unix.putenv "MONGO_URL" "";
      ignore (M.ensure_dev ~root:"/tmp/fennec_evt" ~base_port:4100 ());
      assert (Sys.getenv_opt "MONGO_URL" = Some ":embedded:/tmp/fennec_evt/_build/.fennec/burrow/dev-4100");

      (* an explicit MONGO_URL always wins and is left untouched *)
      Unix.putenv "MONGO_URL" ":memory:";
      assert (Option.is_none (M.ensure_dev ~root:"/tmp/fennec_evt" ~base_port:5000 ()));
      assert (Sys.getenv_opt "MONGO_URL" = Some ":memory:"));
  print_string "mongo_rs ensure_dev (embedded default + per-port isolation + explicit wins): OK\n"
