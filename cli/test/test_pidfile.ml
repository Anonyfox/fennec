(* Pidfile codec: parse tolerates blanks/garbage and drops pids <= 1; render round-trips. *)

module P = Fennec_dev.Pidfile

let fails = ref 0
let check name c = if c then Printf.printf "  ok   %s\n" name else (incr fails; Printf.printf "  FAIL %s\n" name)
let eq name a b = check name (a = b)

let () =
  print_endline "Pidfile.parse / render:";
  eq "parse simple" (P.parse "10\n20\n30\n") [ 10; 20; 30 ];
  eq "parse ignores blanks + garbage" (P.parse "10\n\n  20 \nnope\n30") [ 10; 20; 30 ];
  eq "parse drops pids <= 1 (no killing init)" (P.parse "0\n1\n2\n") [ 2 ];
  eq "render" (P.render [ 1; 2; 3 ]) "1\n2\n3\n";
  eq "render [] -> empty" (P.render []) "";
  eq "round-trip" (P.parse (P.render [ 7; 8; 9 ])) [ 7; 8; 9 ]

(* reap_stale must be identity-safe: a recorded pid that has been recycled to an UNRELATED
   process (here a `sleep`, which is not one of our dune/server/worker binaries) must NOT be
   SIGKILLed — only the pidfile is cleaned up. This is the destructive-bug guard. *)
let () =
  print_endline "Pidfile.reap_stale (identity-safe):";
  let write path s = let oc = open_out_bin path in output_string oc s; close_out oc in
  let dir = Filename.temp_file "fennec_reap" "" in
  Sys.remove dir;
  Unix.mkdir dir 0o755;
  write (Filename.concat dir "dune-project") "(lang dune 3.0)\n";
  Unix.mkdir (Filename.concat dir "_build") 0o755;
  let pid = Unix.create_process "sleep" [| "sleep"; "30" |] Unix.stdin Unix.stdout Unix.stderr in
  write (P.path_for ~root:dir) (string_of_int pid ^ "\n");
  P.reap_stale ~cwd:dir;
  check "an unrelated recycled pid (sleep) is NOT killed" (try Unix.kill pid 0; true with _ -> false);
  check "the stale pidfile is removed" (not (Sys.file_exists (P.path_for ~root:dir)));
  (try Unix.kill pid Sys.sigkill with _ -> ());
  (try ignore (Unix.waitpid [] pid) with _ -> ());
  (try Sys.remove (Filename.concat dir "dune-project") with _ -> ());
  (try Unix.rmdir (Filename.concat dir "_build") with _ -> ());
  (try Unix.rmdir dir with _ -> ());
  if !fails = 0 then print_endline "all Pidfile tests passed." else (Printf.printf "%d FAILED\n" !fails; exit 1)
