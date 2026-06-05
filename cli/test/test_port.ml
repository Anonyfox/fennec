(* Port.is_ours is the gate the reclaim SIGKILL trusts — "is this listener our own dev server?" A
   false positive means killing the WRONG process, so this pins both what it accepts (our binary as
   argv[0], started absolutely or relatively, with or without args) and — more importantly — what it
   must REJECT (the path as a mere argument, a different app's server, a near-miss path). *)

module P = Fennec_dev.Port

let fails = ref 0
let check name c = if c then Printf.printf "  ok   %s\n" name else (incr fails; Printf.printf "  FAIL %s\n" name)

let exe = "_build/default/examples/site/server.bc"

let () =
  print_endline "Port.is_ours:";
  (* accepts: our artifact is the PROGRAM being run *)
  (* the real bytecode shape: `ps` shows the .bc as an argument of ocamlrun (this is what regressed
     an earlier argv[0]-only rule — our own server stopped being recognised) *)
  check "ocamlrun running our .bc is ours (abs runtime)" (P.is_ours ~exe ~cmd:"/Users/x/.opam/5.4.1/bin/ocamlrun _build/default/examples/site/server.bc");
  check "ocamlrun running our .bc is ours (bare runtime)" (P.is_ours ~exe ~cmd:("ocamlrun " ^ exe));
  check "our relative path as argv0 is ours" (P.is_ours ~exe ~cmd:exe);
  check "our absolute path as argv0 is ours" (P.is_ours ~exe ~cmd:"/Users/x/proj/_build/default/examples/site/server.bc");
  check "our server with args after argv0 is ours" (P.is_ours ~exe ~cmd:(exe ^ " --some-flag"));
  check "abs exe param matches a relatively-started holder" (P.is_ours ~exe:"/abs/proj/_build/default/examples/site/server.bc" ~cmd:exe);
  check "exe without _build matches its full path as argv0" (P.is_ours ~exe:"/opt/app/server.bc" ~cmd:"/opt/app/server.bc");
  (* rejects: the SIGKILL must NOT fire on these *)
  check "vim editing the path is NOT ours (path is argv[1])" (not (P.is_ours ~exe ~cmd:("vim " ^ exe)));
  check "a different app's server is NOT ours" (not (P.is_ours ~exe ~cmd:"_build/default/examples/admin/server.bc"));
  check "a foreign listener (python) is NOT ours" (not (P.is_ours ~exe ~cmd:"python3 -m http.server 8200 --bind 127.0.0.1"));
  check "a non-'/'-boundary near-miss path is NOT ours" (not (P.is_ours ~exe ~cmd:"/foo/myfake_build/default/examples/site/server.bc"));
  check "the bare path as argv[1] of a wrapper is NOT ours" (not (P.is_ours ~exe ~cmd:("/bin/sh -c " ^ exe)));
  check "empty command is NOT ours" (not (P.is_ours ~exe ~cmd:""));
  if !fails = 0 then print_endline "all Port tests passed." else (Printf.printf "%d FAILED\n" !fails; exit 1)
