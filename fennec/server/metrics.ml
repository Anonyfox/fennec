(* Metrics/telemetry — time each request and report (method, path, status, duration) once
   the response is finalized (via a before_send hook). Declines (passes through). *)

module Conn = Fennec_paw.Conn
module Paw = Fennec_paw.Paw
module H = Fennec_core.Http

let make (report : meth:string -> path:string -> status:int -> duration_ms:float -> unit) : Paw.t =
 fun c ->
  let t0 = Unix.gettimeofday () in
  let meth = H.string_of_meth (Conn.meth c) and path = Conn.path c in
  Conn.before_send c (fun r ->
      report ~meth ~path ~status:r.H.status ~duration_ms:((Unix.gettimeofday () -. t0) *. 1000.0);
      r)
