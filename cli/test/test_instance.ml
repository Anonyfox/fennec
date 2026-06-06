(* Per-suite instance allocation: deterministic, collision-free port blocks + correct env. *)

module I = Fennec_testcmd.Instance
module D = Fennec_core.Dev_proto

let fails = ref 0
let check name c = if c then Printf.printf "  ok   %s\n" name else (incr fails; Printf.printf "  FAIL %s\n" name)

let () =
  let inst = I.allocate ~base:7000 [ "a"; "b"; "c" ] in
  check "one instance per suite" (List.length inst = 3);
  let ports = List.map (fun i -> i.I.port) inst in
  check "suite 0 at base" (List.nth ports 0 = 7000);
  check "suite 1 at base+stride" (List.nth ports 1 = 7000 + I.stride);
  check "suite 2 at base+2*stride" (List.nth ports 2 = 7000 + (2 * I.stride));
  check "ports are distinct" (List.sort_uniq compare ports = List.sort compare ports);
  check "blocks don't overlap (stride > 1)" (I.stride > 1);

  let a = List.hd inst in
  check "url matches the port" (a.I.url = "http://localhost:7000");
  check "server_env sets the port" (List.assoc D.env_port a.I.server_env = "7000");
  check "server_env disables livereload (determinism)" (List.assoc D.env_dev_livereload a.I.server_env = "0");
  check "suite_env targets the instance via FENNEC_TEST_URL" (List.assoc D.env_test_url a.I.suite_env = "http://localhost:7000");
  check "suite name carried" (a.I.suite = "a");

  (* deterministic: same inputs → same allocation *)
  let again = I.allocate ~base:7000 [ "a"; "b"; "c" ] in
  check "deterministic (re-run gives identical ports)" (List.map (fun i -> i.I.port) again = ports);
  (* a different base shifts the whole block (parallel CI shards / worktrees) *)
  let shifted = I.allocate ~base:9000 [ "a" ] in
  check "a different base shifts the block" ((List.hd shifted).I.port = 9000);

  check "empty suite list → no instances" (I.allocate ~base:7000 [] = []);

  if !fails = 0 then print_endline "all Instance tests passed." else (Printf.printf "%d FAILED\n" !fails; exit 1)
