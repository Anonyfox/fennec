(* Set a [Cache-Control] directive on responses — ["no-store"] for an API, ["public, max-age=3600"]
   for cacheable assets. Registers a before_send hook (built ONCE in [make], so per request it costs
   only the registration); the hook adds the header unless the handler already set its own, so a more
   specific route still wins. *)

module Conn = Conn
module H = Http

let make (directive : string) : Pipeline.t =
  let hook (r : H.response) = if Headers.mem r.H.headers "cache-control" then r else { r with H.headers = ("cache-control", directive) :: r.H.headers } in
  fun c -> Conn.before_send c hook

(* ──── cache_control tests ──── *)

let req_ () = H.make_request ~meth:H.GET ~path:"/" ()
let finalize_ c = Conn.apply_before_send c (Option.value (Conn.resp c) ~default:(H.text "x"))

let%test "sets cache-control when absent" =
  let c = make "no-store" (Conn.text (Conn.make (req_ ())) "x") in
  Headers.get (finalize_ c).H.headers "cache-control" = Some "no-store"
let%test "does not override a handler's explicit value" =
  let c = make "no-store" (Conn.text ~headers:[ ("cache-control", "max-age=60") ] (Conn.make (req_ ())) "x") in
  Headers.get (finalize_ c).H.headers "cache-control" = Some "max-age=60"
