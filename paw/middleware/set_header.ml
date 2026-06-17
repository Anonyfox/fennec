(* Add fixed response headers to every response the endpoint answers (a [X-Powered-By], a custom
   [X-App-Version], …). Registers a before_send hook built ONCE in [make]; each header is added only
   if absent, so a handler that set its own value still wins and no duplicates appear. *)

module Conn = Conn
module H = Http

let make (headers : (string * string) list) : Pipeline.t =
  let hook (r : H.response) = List.fold_left (fun (r : H.response) (k, v) -> if Headers.mem r.H.headers k then r else { r with H.headers = (k, v) :: r.H.headers }) r headers in
  fun c -> Conn.before_send c hook

(* ──── set_header tests ──── *)

let req_ () = H.make_request ~meth:H.GET ~path:"/" ()
let finalize_ c = Conn.apply_before_send c (Option.value (Conn.resp c) ~default:(H.text "x"))

let%test "adds the headers" =
  let c = make [ ("x-powered-by", "fennec"); ("x-app", "site") ] (Conn.text (Conn.make (req_ ())) "x") in
  let h = (finalize_ c).H.headers in
  Headers.get h "x-powered-by" = Some "fennec" && Headers.get h "x-app" = Some "site"
let%test "does not override an existing header (no duplicate)" =
  let c = make [ ("x-powered-by", "fennec") ] (Conn.text ~headers:[ ("x-powered-by", "custom") ] (Conn.make (req_ ())) "x") in
  Headers.get_all (finalize_ c).H.headers "x-powered-by" = [ "custom" ]
