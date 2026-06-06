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

(* ──── metrics tests ──── *)

let req_ ?(meth = H.GET) path = H.make_request ~meth ~path ()
let finalize_ c = Conn.apply_before_send c (Option.value (Conn.resp c) ~default:(H.text ~status:404 ""))

let%test "reports method/path/status at send" =
  let seen = ref None in
  let mp = make (fun ~meth ~path ~status ~duration_ms:_ -> seen := Some (meth, path, status)) in
  let c = mp (Conn.make (req_ ~meth:H.POST "/m")) in
  let c = Conn.text ~status:201 c "ok" in
  let _ = finalize_ c in
  !seen = Some ("POST", "/m", 201)
