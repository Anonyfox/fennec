(* Per-route request-body size limit — answer 413 when the body exceeds [max_bytes], before the
   handler touches it. The server already enforces a global hard cap (so an attacker can't make it
   buffer unbounded); this is the tighter, per-endpoint logical limit (e.g. 64 KB for a JSON API). *)

module Conn = Conn
module H = Http

let make ~(max_bytes : int) () : Pipeline.t =
 fun c -> if String.length (Conn.req c).H.body > max_bytes then Conn.text ~status:413 c "Payload Too Large" else c

(* ──── body_limit tests ──── *)

let req_ ?(body = "") () = H.make_request ~meth:H.POST ~path:"/" ~body ()
let resp_of_ c = Option.value (Conn.resp c) ~default:(H.text ~status:404 "")

let%test "under the limit declines" = not (Conn.answered (make ~max_bytes:16 () (Conn.make (req_ ~body:"small" ()))))
let%test "at the limit declines" = not (Conn.answered (make ~max_bytes:5 () (Conn.make (req_ ~body:"hello" ()))))
let%test "over the limit -> 413" = (resp_of_ (make ~max_bytes:4 () (Conn.make (req_ ~body:"hello" ())))).H.status = 413
let%test "empty body declines" = not (Conn.answered (make ~max_bytes:0 () (Conn.make (req_ ()))))
