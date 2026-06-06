(* The env-contract drift guard: the suite-side name (hunt: Test_proto.env_url) and the
   CLI-side mirror (framework: Dev_proto.env_test_url) must agree. This is the ONE test in
   hunt that cross-links fennec.core — all other Http assertions are inline in http.ml. *)

let () =
  let tp = Fennec_hunt.Test_proto.env_url in
  let dp = Fennec_core.Dev_proto.env_test_url in
  if tp = dp then Printf.printf "  ok   env contract mirror agrees (Test_proto = Dev_proto)\n"
  else (Printf.printf "  FAIL env contract: Test_proto.env_url=%S ≠ Dev_proto.env_test_url=%S\n" tp dp; exit 1)
