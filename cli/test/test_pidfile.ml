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
  eq "round-trip" (P.parse (P.render [ 7; 8; 9 ])) [ 7; 8; 9 ];
  if !fails = 0 then print_endline "all Pidfile tests passed." else (Printf.printf "%d FAILED\n" !fails; exit 1)
