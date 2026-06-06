(* See port_plan.mli. Deterministic dev port allocation from a single base: the gateway (the
   prod-identical Host router) sits at [base]; the i-th endpoint (declaration order) gets a forced
   convenience port at [base + 1 + i]. [of_base] validates the whole block fits in the port range
   up front, so [gateway]/[endpoint_port] are total. A different base (--port / FENNEC_PORT) shifts
   the entire block, so independent instances never collide. *)

type t = { base : int; count : int }

(* ──── of_base ──── *)

let of_base ~base ~count : (t, string) result =
  if base < 1 || base > 65535 then Error (Printf.sprintf "base port %d is out of range (1..65535)" base)
  else if count < 0 then Error (Printf.sprintf "endpoint count %d is negative" count)
  else if base + count > 65535 then Error (Printf.sprintf "%d endpoints from base %d would need port %d, past 65535 — pick a lower --port" count base (base + count))
  else Ok { base; count }

let%test "base 0 -> error"             = Result.is_error (of_base ~base:0 ~count:1)
let%test "base > 65535 -> error"        = Result.is_error (of_base ~base:70000 ~count:1)
let%test "overflow -> error"            = Result.is_error (of_base ~base:65530 ~count:10)
let%test "edge: base+count == 65535"    = Result.is_ok (of_base ~base:65000 ~count:535)
let%test "prod base 80, 0 endpoints"   = Result.is_ok (of_base ~base:80 ~count:0)

(* ──── gateway ──── *)

let gateway t = t.base

let%test "gateway = base" =
  let p = Result.get_ok (of_base ~base:8020 ~count:2) in gateway p = 8020

(* ──── endpoint_port ──── *)

let endpoint_port t ~index = t.base + 1 + index

let%test "endpoint 0 = base+1" =
  let p = Result.get_ok (of_base ~base:8020 ~count:2) in endpoint_port p ~index:0 = 8021
let%test "endpoint 1 = base+2" =
  let p = Result.get_ok (of_base ~base:8020 ~count:2) in endpoint_port p ~index:1 = 8022

(* ──── count ──── *)

let count t = t.count

let%test "count is preserved" =
  let p = Result.get_ok (of_base ~base:8020 ~count:2) in count p = 2

(* ──── isolation (integration) ──── *)

let%test "isolation: different base" =
  let q = Result.get_ok (of_base ~base:9000 ~count:2) in
  gateway q = 9000 && endpoint_port q ~index:1 = 9002
