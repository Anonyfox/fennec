(* Suite discovery path logic (pure parts) — the cwd-relative-to-root + _build artifact paths. *)

module S = Fennec_testcmd.Suites

let fails = ref 0
let check name c = if c then Printf.printf "  ok   %s\n" name else (incr fails; Printf.printf "  FAIL %s\n" name)

let () =
  (* relativize: downstream app (cwd = root) → "" ; monorepo subdir → the subpath *)
  check "cwd = root → empty reldir" (S.relativize ~root:"/r" ~cwd:"/r" = "");
  check "cwd under root → subpath" (S.relativize ~root:"/r" ~cwd:"/r/examples/site" = "examples/site");
  check "cwd not under root → treated as root" (S.relativize ~root:"/r" ~cwd:"/other" = "");
  check "no false prefix match (/right vs /r)" (S.relativize ~root:"/r" ~cwd:"/right" = "");

  (* exe_path: root case (reldir "") and subdir case *)
  check "exe path at root" (S.exe_path ~root:"/r" ~reldir:"" ~dir:"test/http" ~name:"checkout" = "/r/_build/default/test/http/checkout.exe");
  check "exe path in a subdir" (S.exe_path ~root:"/r" ~reldir:"examples/site" ~dir:"test/browser" ~name:"cart" = "/r/_build/default/examples/site/test/browser/cart.exe");

  (* build target is ROOT-relative (built after chdir to root) *)
  check "build target at root" (S.build_target ~reldir:"" ~dir:"test/http" ~name:"checkout" = "test/http/checkout.exe");
  check "build target in a subdir" (S.build_target ~reldir:"examples/site" ~dir:"test/http" ~name:"smoke" = "examples/site/test/http/smoke.exe");

  if !fails = 0 then print_endline "all Suites tests passed." else (Printf.printf "%d FAILED\n" !fails; exit 1)
