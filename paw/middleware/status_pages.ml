(* Fill in a body for error responses that have none — so a handler can [Conn.set_status 403 c] (or
   422, 429, …) without also writing a body and still send something sensible. By default it writes
   ["<code> <reason>"] as text; pass [render] for branded HTML/JSON error pages. Registers a
   before_send hook built ONCE in [make]; it only touches responses with status >= 400 AND no body,
   so successful responses are untouched. (Routing 404s / handler exceptions are rendered by the
   server's [~on_error] funnel; this covers statuses a handler sets itself.) *)

module Conn = Conn
module H = Http

let make ?(content_type = "text/plain; charset=utf-8") ?(render : (int -> string option) option) () : Pipeline.t =
  let default_body status = Printf.sprintf "%d %s" status (H.reason_phrase status) in
  let body_for status = match render with Some f -> ( match f status with Some b -> b | None -> default_body status) | None -> default_body status in
  let hook (r : H.response) =
    if r.H.status >= 400 && r.H.body = "" then
      { r with H.body = body_for r.H.status; headers = (if Headers.mem r.H.headers "content-type" then r.H.headers else ("content-type", content_type) :: r.H.headers) }
    else r
  in
  fun c -> Conn.before_send c hook

(* ──── status_pages tests ──── *)

let req_ () = H.make_request ~meth:H.GET ~path:"/" ()
let finalize_ c = Conn.apply_before_send c (Option.value (Conn.resp c) ~default:(H.text "x"))

let%test "fills an empty error body with code + reason" =
  let c = make () (Conn.text ~status:403 (Conn.make (req_ ())) "") in
  (finalize_ c).H.body = "403 Forbidden"
let%test "leaves a non-empty error body alone" =
  let c = make () (Conn.text ~status:403 (Conn.make (req_ ())) "nope") in
  (finalize_ c).H.body = "nope"
let%test "leaves success responses alone" =
  let c = make () (Conn.text ~status:200 (Conn.make (req_ ())) "") in
  (finalize_ c).H.body = ""
let%test "custom render wins" =
  let c = make ~render:(fun s -> Some (Printf.sprintf "<h1>%d</h1>" s)) () (Conn.text ~status:404 (Conn.make (req_ ())) "") in
  (finalize_ c).H.body = "<h1>404</h1>"
let%test "default content-type is text/plain" =
  let c = make () (Conn.set_status 500 (Conn.make (req_ ()))) in
  Headers.get (finalize_ c).H.headers "content-type" = Some "text/plain; charset=utf-8"
let%test "custom content-type for branded pages" =
  let c = make ~content_type:"text/html; charset=utf-8" ~render:(fun _ -> Some "<h1>oops</h1>") () (Conn.set_status 500 (Conn.make (req_ ()))) in
  let r = finalize_ c in
  r.H.body = "<h1>oops</h1>" && Headers.get r.H.headers "content-type" = Some "text/html; charset=utf-8"
