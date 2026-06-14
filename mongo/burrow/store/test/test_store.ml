(* Exercises the new FFI (named sub-DBs, del, cursors, seek) through the typed facade, end-to-end under
   a real Eio scheduler (write commits run off-scheduler via run_in_systhread). Asserts ordered
   iteration, lower-bound seek, early-stop, delete semantics, and point lookups. *)
module S = Burrow_store.Store

let () =
  Eio_main.run @@ fun _env ->
  let dir =
    Filename.concat (Filename.get_temp_dir_name ()) ("burrow_store_t" ^ string_of_int (Unix.getpid ()))
  in
  (try Unix.mkdir dir 0o755 with Unix.Unix_error (Unix.EEXIST, _, _) -> ());
  let t = S.open_ ~durability:S.No_sync dir in
  let db = S.db t "main" in

  (* writes (inserted out of order; the B+-tree keeps them ordered) *)
  S.write t (fun txn ->
      S.put txn db "b" "2";
      S.put txn db "a" "1";
      S.put txn db "c" "3";
      S.put txn db "d" "4");

  (* point gets *)
  let g k = S.read t (fun txn -> S.get txn db k) in
  assert (g "a" = Some "1");
  assert (g "c" = Some "3");
  assert (g "z" = None);

  (* full ordered iteration *)
  let all = ref [] in
  S.read t (fun txn -> S.iter txn db (fun ~key ~data -> all := (key, data) :: !all; true));
  assert (List.rev !all = [ ("a", "1"); ("b", "2"); ("c", "3"); ("d", "4") ]);

  (* lower-bound seek *)
  let from_c = ref [] in
  S.read t (fun txn -> S.iter txn db ~from:"c" (fun ~key ~data:_ -> from_c := key :: !from_c; true));
  assert (List.rev !from_c = [ "c"; "d" ]);

  (* early stop after two *)
  let two = ref [] in
  S.read t (fun txn -> S.iter txn db (fun ~key ~data:_ -> two := key :: !two; List.length !two < 2));
  assert (List.rev !two = [ "a"; "b" ]);

  (* delete semantics: returns whether present *)
  assert (S.write t (fun txn -> S.del txn db "b"));
  assert (not (S.write t (fun txn -> S.del txn db "b")));
  assert (g "b" = None);

  (* a second named sub-DB is independent *)
  let other = S.db t "other" in
  S.write t (fun txn -> S.put txn other "a" "X");
  assert (S.read t (fun txn -> S.get txn other "a") = Some "X");
  assert (g "a" = Some "1");

  (* durability survives reopen (No_sync still flushes on clean close) *)
  assert (S.durability t = S.No_sync);
  S.close t;
  let t2 = S.open_ ~durability:S.No_sync dir in
  let db2 = S.db t2 "main" in
  assert (S.read t2 (fun txn -> S.get txn db2 "d") = Some "4");
  S.close t2;

  print_string "store smoke: OK\n"
