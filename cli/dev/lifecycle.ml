(* See lifecycle.mli. The occasional-use DX helpers behind `fennec clean [--db]`: drop build artifacts,
   and — opt-in — reset the dev database.

   The load-bearing invariant (do not break it): the DEV database and any TEST/e2e database live in
   DISJOINT places. The dev DB is only ever [.fennec/db/<port>] (set by {!Mongo_rs.ensure_dev}, i.e. the
   `fennec dev` path); `fennec test` grounds its DB at [:memory:] or a per-scenario temp dir with a
   per-suite [fennec_test_<port>] name. So a `clean --db` reset here can NEVER touch a test DB, and a
   test run can never touch the dev DB. Build artifacts ([_build/…]) are a third, separate concern. *)

module Runtime = Fennec_mongo_driver.Runtime
module Client = Fennec_mongo_driver.Client

(* recursive delete, best-effort (already-gone is success) *)
let rec rm_rf path =
  match Unix.lstat path with
  | exception _ -> ()
  | { Unix.st_kind = Unix.S_DIR; _ } ->
    Array.iter (fun e -> rm_rf (Filename.concat path e)) (try Sys.readdir path with _ -> [||]);
    (try Unix.rmdir path with _ -> ())
  | _ -> (try Sys.remove path with _ -> ())

let dir_exists p = match Unix.stat p with exception _ -> false | s -> s.Unix.st_kind = Unix.S_DIR

(* A destructive-action gate. [--force] bypasses; a TTY prompts; a NON-tty (CI) without [--force]
   REFUSES — so a CI step can never silently wipe data, it must opt in with [--force]. *)
let confirm ~force msg =
  if force then true
  else if not (Unix.isatty Unix.stdin) then (
    Printf.eprintf "fennec clean: %s\n  refusing without a TTY — re-run with --force to proceed.\n%!" msg;
    false)
  else begin
    Printf.printf "fennec clean: %s [y/N] " msg;
    flush stdout;
    match String.lowercase_ascii (String.trim (try input_line stdin with End_of_file -> "")) with
    | "y" | "yes" -> true
    | _ -> Printf.printf "  skipped.\n%!"; false
  end

(* Reset the DEV database — never a test DB. Branches on the resolved backend (reads MONGO_URL from the
   env, exactly as `fennec dev` does), so it targets the same DB the dev loop would use. *)
let wipe_dev_db ~root ~force () =
  match Runtime.backend () with
  | Runtime.Memory -> Printf.printf "  dev database is :memory: (ephemeral) — nothing to wipe.\n%!"
  | Runtime.Missing ->
    (* no MONGO_URL ⇒ the embedded default: every per-port dev DB under .fennec/db *)
    let dir = Filename.concat root ".fennec/db" in
    if not (dir_exists dir) then Printf.printf "  no embedded dev database at %s.\n%!" dir
    else if confirm ~force (Printf.sprintf "wipe the embedded dev database at %s?" dir) then (
      rm_rf dir;
      Printf.printf "  wiped %s — the next `fennec dev` re-seeds it.\n%!" dir)
  | Runtime.Burrow { base; _ } ->
    (* an explicit burrow:// — its files live at [base] *)
    if not (dir_exists base) then Printf.printf "  no burrow dev database at %s.\n%!" base
    else if confirm ~force (Printf.sprintf "wipe the burrow dev database at %s?" base) then (
      rm_rf base;
      Printf.printf "  wiped %s.\n%!" base)
  | Runtime.Mongo { uri; db } ->
    (* external mongo: drop ONLY the dev database [db] — never the per-suite [fennec_test_*] DBs *)
    if confirm ~force (Printf.sprintf "DROP database %S on the external mongo at %s?" db uri) then (
      try
        let c = Client.connect ~uri () in
        Client.drop_database c ~db;
        Printf.printf "  dropped database %S.\n%!" db
      with e -> Printf.eprintf "  could not drop %S: %s\n%!" db (Printexc.to_string e))

let clean ~root ~db ~force () =
  (* 1. build artifacts. `dune clean` wipes _build/default; the dev loop's ISOLATED _build/fastdev is a
     separate build root dune clean does not know about, so remove it explicitly. The dev DB
     (.fennec/db) is deliberately untouched here — that is the whole point of moving it out of _build. *)
  (try ignore (Sys.command "dune clean") with _ -> ());
  let fastdev = Filename.concat root "_build/fastdev" in
  let had_fastdev = dir_exists fastdev in
  rm_rf fastdev;
  Printf.printf "cleaned build artifacts: _build/default%s.\n%!" (if had_fastdev then " + _build/fastdev" else "");
  (* 2. opt-in: the dev database *)
  if db then wipe_dev_db ~root ~force ()

(* ──── tests ──── *)

let%test "rm_rf removes a tree and is idempotent" =
  let d = Filename.concat (Filename.get_temp_dir_name ()) ("fennec-lc-" ^ string_of_int (Unix.getpid ())) in
  (try Unix.mkdir d 0o755 with _ -> ());
  let f = Filename.concat d "x" in
  Out_channel.with_open_text f (fun oc -> output_string oc "hi");
  rm_rf d;
  (not (Sys.file_exists d)) && (rm_rf d; true) (* second call is a no-op, not an error *)

let%test "confirm ~force is always yes (no prompt, CI-safe)" = confirm ~force:true "anything" = true
