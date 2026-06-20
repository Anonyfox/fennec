(* Stamp the elapsed milliseconds onto the response — a [Server-Timing] entry (browsers surface it
   in the Network panel's Timing tab) and, with [~x_header], an [X-Response-Time] header. The
   CLIENT-visible counterpart to {!Logger} (server log) and {!Metrics} (callback). The elapsed time
   comes from the ONE request timer stamped on the conn at {!Conn.make} (see {!Access}) — no
   per-request clock grab here. Mount it FIRST so its span covers the whole pipeline. Stamps the
   responses the pipeline answers (a pure-decline 404/405 is generated on a fresh conn downstream). *)

module Conn = Conn
module H = Http

let make ?(metric = "app") ?(x_header = false) () : Pipeline.t =
 fun c ->
  Conn.before_send c (fun r ->
      let ms = float_of_int (Conn.elapsed_us c) /. 1000.0 in
      let headers = (Printf.sprintf "%s;dur=%.1f" metric ms |> fun st -> ("server-timing", st)) :: r.H.headers in
      let headers = if x_header && not (Headers.mem r.H.headers "x-response-time") then ("x-response-time", Printf.sprintf "%.1fms" ms) :: headers else headers in
      { r with H.headers })

(* ──── response_time tests ──── *)

let req_ () = H.make_request ~meth:H.GET ~path:"/" ()
let finalize_ c = Conn.apply_before_send c (Option.value (Conn.resp c) ~default:(H.text "x"))

let%test "adds a Server-Timing entry" =
  let c = Conn.text (make () (Conn.make (req_ ()))) "ok" in
  match Headers.get (finalize_ c).H.headers "server-timing" with Some v -> Fennec_hunt_unit.str_contains v "app;dur=" | None -> false
let%test "X-Response-Time only when enabled" =
  let off = finalize_ (Conn.text (make () (Conn.make (req_ ()))) "ok") in
  let on = finalize_ (Conn.text (make ~x_header:true () (Conn.make (req_ ()))) "ok") in
  Headers.get off.H.headers "x-response-time" = None && (match Headers.get on.H.headers "x-response-time" with Some v -> Fennec_hunt_unit.str_contains v "ms" | None -> false)
let%test "custom metric name" =
  let c = finalize_ (Conn.text (make ~metric:"total" () (Conn.make (req_ ()))) "ok") in
  match Headers.get c.H.headers "server-timing" with Some v -> Fennec_hunt_unit.str_contains v "total;dur=" | None -> false
