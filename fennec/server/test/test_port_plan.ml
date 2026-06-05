(* Port_plan: deterministic allocation + the range/overflow guards. *)

module PP = Fennec_server.Port_plan

let fails = ref 0
let check name c = if c then Printf.printf "  ok   %s\n" name else (incr fails; Printf.printf "  FAIL %s\n" name)
let ok = function Ok _ -> true | Error _ -> false
let err = function Error _ -> true | Ok _ -> false

let () =
  print_endline "Port_plan:";
  let p = Result.get_ok (PP.of_base ~base:8020 ~count:2) in
  check "gateway = base" (PP.gateway p = 8020);
  check "endpoint 0 = base+1" (PP.endpoint_port p ~index:0 = 8021);
  check "endpoint 1 = base+2" (PP.endpoint_port p ~index:1 = 8022);
  check "count is preserved" (PP.count p = 2);
  (* a different base shifts the whole block (instance isolation) *)
  let q = Result.get_ok (PP.of_base ~base:9000 ~count:2) in
  check "isolation: base 9000 -> 9000/9001/9002" (PP.gateway q = 9000 && PP.endpoint_port q ~index:1 = 9002);
  (* guards *)
  check "base 0 -> error" (err (PP.of_base ~base:0 ~count:1));
  check "base > 65535 -> error" (err (PP.of_base ~base:70000 ~count:1));
  check "overflow base+count > 65535 -> error" (err (PP.of_base ~base:65530 ~count:10));
  check "edge: base+count == 65535 is ok" (ok (PP.of_base ~base:65000 ~count:535));
  check "prod base 80, 0 endpoints is ok" (ok (PP.of_base ~base:80 ~count:0));
  if !fails = 0 then print_endline "all Port_plan tests passed." else (Printf.printf "%d FAILED\n" !fails; exit 1)
