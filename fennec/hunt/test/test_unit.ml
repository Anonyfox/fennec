(* Dogfood: the Unit runtime + ppx, tested by the test library itself. Proves the full
   pipeline: ppx expansion → registration → run → output → exit code. Also proves the
   ppx syntax works against the real compiler + ppxlib version. *)

(* ──── explicit API (no ppx) ──── *)

let () = Fennec_hunt_unit.test "explicit bool test" (fun () -> 1 + 1 = 2)

let () = Fennec_hunt_unit.test_unit "explicit unit test" (fun () ->
  Fennec_hunt_unit.check "inner check" (List.length [1;2;3] = 3))

let () = Fennec_hunt_unit.test_unit "check_eq passes on equal" (fun () ->
  Fennec_hunt_unit.check_eq "values" ~expected:"abc" ~got:"abc")

(* ──── ppx sugar (let%test / let%test_unit) ──── *)

let%test "ppx bool test" = 2 * 3 = 6

let%test "ppx bool with function call" = String.length "hello" = 5

let%test_unit "ppx unit test" =
  Fennec_hunt_unit.check "inner" (42 > 0)

let%test_unit "ppx unit with check_eq" =
  Fennec_hunt_unit.check_eq "string" ~expected:"abc" ~got:(String.lowercase_ascii "ABC")

(* ──── the runner ──── *)

let () = exit (Fennec_hunt_unit.run ())
