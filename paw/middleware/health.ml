(* Health-check endpoint — answers a fixed path (default [/healthz]) with 200 for liveness/readiness
   probes (k8s, load balancers, uptime monitors). Declines every other request, so it slots in
   before the app's routes with zero cost on the hot path (one path compare). *)

module Conn = Conn
module H = Http

let make ?(path = "/healthz") ?(status = 200) ?(body = "OK") () : Pipeline.t =
 fun c -> if (Conn.meth c = H.GET || Conn.meth c = H.HEAD) && Conn.path c = path then Conn.text ~status c body else c

(* ──── health tests ──── *)

let req_ ?(meth = H.GET) path = H.make_request ~meth ~path ()
let resp_of_ c = Option.value (Conn.resp c) ~default:(H.text ~status:404 "")
let h_ = make ()

let%test "GET /healthz -> 200" = (resp_of_ (h_ (Conn.make (req_ "/healthz")))).H.status = 200
let%test "HEAD /healthz answered" = Conn.answered (h_ (Conn.make (req_ ~meth:H.HEAD "/healthz")))
let%test "other path declines" = not (Conn.answered (h_ (Conn.make (req_ "/"))))
let%test "POST /healthz declines (probes are GET/HEAD)" = not (Conn.answered (h_ (Conn.make (req_ ~meth:H.POST "/healthz"))))
let%test "custom path + body" =
  let h = make ~path:"/livez" ~body:"ready" () in
  (resp_of_ (h (Conn.make (req_ "/livez")))).H.body = "ready"
