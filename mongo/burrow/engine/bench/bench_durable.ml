(* Group-commit proof: durable (Full = F_FULLFSYNC) write throughput, sequential vs concurrent. A
   sequential writer awaits each commit, so it pays one fsync PER write (no batching). Concurrent
   writers all queue while the writer fiber is mid-fsync, so the next batch fsyncs many writes at once —
   the group-commit win. Run with `dune exec`. *)
module Eng = Burrow.Engine
module S = Burrow_store.Store
module B = Bson

let tmp () =
  let d = Filename.concat (Filename.get_temp_dir_name ()) ("burrow_durable_" ^ string_of_int (Unix.getpid ())) in
  (try Unix.mkdir d 0o755 with Unix.Unix_error (Unix.EEXIST, _, _) -> ());
  d

let doc fields = B.Document fields
let now = Unix.gettimeofday
let mkdoc k = doc [ ("_id", B.str (Printf.sprintf "d%08d" k)); ("v", B.int k) ]

let () =
  Eio_main.run @@ fun _env ->
  Eio.Switch.run @@ fun sw ->
  let eng = Eng.open_ ~sw ~durability:S.Full (tmp ()) in
  (* Full = a real F_FULLFSYNC per durable commit *)
  let c = Eng.collection eng "t" in
  Printf.printf "\n=== Burrow durable-write throughput (Full / F_FULLFSYNC) ===\n\n";

  (* sequential: each insert awaits its own durable commit -> one fsync per write *)
  let seq_n = 200 in
  let t0 = now () in
  for k = 0 to seq_n - 1 do ignore (Eng.insert eng c (mkdoc k)) done;
  let seq_rate = float seq_n /. (now () -. t0) in
  Printf.printf "  sequential (one writer)        %8.0f durable writes/s  (%.2f ms/write)\n%!" seq_rate (1000. /. seq_rate);

  (* concurrent: N fibers writing at once -> the writer fiber group-commits each wave in one fsync *)
  let fibers = 200 and per = 50 in
  let conc_n = fibers * per in
  let t1 = now () in
  Eio.Fiber.all (List.init fibers (fun f () -> for j = 0 to per - 1 do ignore (Eng.insert eng c (mkdoc (1_000_000 + (f * per) + j))) done));
  let conc_rate = float conc_n /. (now () -. t1) in
  Printf.printf "  concurrent (%3d fibers)         %8.0f durable writes/s  (%.0fx the sequential rate)\n%!" fibers conc_rate (conc_rate /. seq_rate);

  assert (Eng.count eng c ~selector:(B.Document []) = seq_n + conc_n);
  Eng.close eng;
  Printf.printf "\n  (all %d writes durable + present; group commit amortizes the fsync across each wave)\n" (seq_n + conc_n)
