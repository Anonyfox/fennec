(* `fennec dev` defaults the Mongo backend to the embedded engine. ensure_dev points MONGO_URL at a
   burrow:// URL with a loopback authority (so mongosh works) over a per-base-port directory when it is
   unset (no mongod), keeps parallel base ports isolated, and leaves an explicit MONGO_URL untouched
   (real mongo / :memory: / burrow:// all win). *)
module M = Fennec_dev.Mongo_rs

let () =
  let saved = Sys.getenv_opt "MONGO_URL" in
  Fun.protect
    ~finally:(fun () -> Unix.putenv "MONGO_URL" (Option.value saved ~default:""))
    (fun () ->
      (* unset -> burrow:// on loopback:27017 over a per-base-port dir UNDER .fennec/db (NOT _build, so
         `fennec clean` can't wipe it), db "fennec"; nothing to own (None) *)
      Unix.putenv "MONGO_URL" "";
      assert (Option.is_none (M.ensure_dev ~root:"/tmp/fennec_evt" ~base_port:4000 ()));
      assert (
        Sys.getenv_opt "MONGO_URL"
        = Some "burrow://localhost:27017/tmp/fennec_evt/.fennec/db/4000/fennec");

      (* a different base port -> a different dir (so parallel dev / e2e instances stay isolated) *)
      Unix.putenv "MONGO_URL" "";
      ignore (M.ensure_dev ~root:"/tmp/fennec_evt" ~base_port:4100 ());
      assert (
        Sys.getenv_opt "MONGO_URL"
        = Some "burrow://localhost:27017/tmp/fennec_evt/.fennec/db/4100/fennec");

      (* an explicit MONGO_URL always wins and is left untouched *)
      Unix.putenv "MONGO_URL" ":memory:";
      assert (Option.is_none (M.ensure_dev ~root:"/tmp/fennec_evt" ~base_port:5000 ()));
      assert (Sys.getenv_opt "MONGO_URL" = Some ":memory:"));
  print_string "mongo_rs ensure_dev (burrow:// default + per-port isolation + explicit wins): OK\n"
