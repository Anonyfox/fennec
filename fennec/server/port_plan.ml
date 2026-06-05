(* See port_plan.mli. Deterministic dev port allocation from a single base: the gateway (the
   prod-identical Host router) sits at [base]; the i-th endpoint (declaration order) gets a forced
   convenience port at [base + 1 + i]. [of_base] validates the whole block fits in the port range
   up front, so [gateway]/[endpoint_port] are total. A different base (--port / FENNEC_PORT) shifts
   the entire block, so independent instances never collide. *)

type t = { base : int; count : int }

let of_base ~base ~count : (t, string) result =
  if base < 1 || base > 65535 then Error (Printf.sprintf "base port %d is out of range (1..65535)" base)
  else if count < 0 then Error (Printf.sprintf "endpoint count %d is negative" count)
  else if base + count > 65535 then Error (Printf.sprintf "%d endpoints from base %d would need port %d, past 65535 — pick a lower --port" count base (base + count))
  else Ok { base; count }

let gateway t = t.base
let endpoint_port t ~index = t.base + 1 + index
let count t = t.count
