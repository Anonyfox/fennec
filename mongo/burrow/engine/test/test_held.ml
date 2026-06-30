(* The write-lock-hold observable: a workflow transaction holds the single write lock for its whole span,
   so a runaway holder stalls ALL writes. transaction_held_for exposes how long the current txn has held
   it (None when idle) — detection for observability/a watchdog (the sanctioned fix is the effects outbox). *)
module Eng = Burrow.Engine
module S = Burrow_store.Store
module B = Bson

let temp () =
  let d = Filename.concat (Filename.get_temp_dir_name ()) (Printf.sprintf "burrow_held_%d" (Unix.getpid ())) in
  (try Unix.mkdir d 0o755 with Unix.Unix_error (Unix.EEXIST, _, _) -> ());
  List.iter (fun f -> try Sys.remove (Filename.concat d f) with _ -> ()) [ "data.mdb"; "lock.mdb" ];
  d

let doc fields = B.Document fields
let str = B.str

let () =
  Eio_main.run @@ fun _env ->
  Eio.Switch.run @@ fun sw ->
  let eng = Eng.open_ ~sw ~durability:S.No_sync (temp ()) in
  let c = Eng.collection eng "t" in

  (* idle: no transaction holds the lock *)
  assert (Eng.transaction_held_for eng = None);

  (* inside a workflow transaction the lock is held — the observable reports a non-negative duration *)
  let tx = Eng.begin_txn eng in
  (match Eng.transaction_held_for eng with Some d -> assert (d >= 0.) | None -> assert false);
  ignore (Eng.insert_in tx c (doc [ ("_id", str "a") ]));
  (match Eng.transaction_held_for eng with Some _ -> () | None -> assert false);
  Eng.commit_txn eng tx;

  (* released on commit *)
  assert (Eng.transaction_held_for eng = None);

  (* released on abort too *)
  let tx2 = Eng.begin_txn eng in
  ignore (Eng.insert_in tx2 c (doc [ ("_id", str "b") ]));
  Eng.abort_txn eng tx2;
  assert (Eng.transaction_held_for eng = None);

  Eng.close eng;
  print_string "held: OK\n"
