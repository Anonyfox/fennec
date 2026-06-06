(* Method override — let an HTML form POST act as PUT/PATCH/DELETE, via a [_method] form
   field or the X-HTTP-Method-Override header. Only POST is rewritten; anything else passes
   through untouched. *)

module Conn = Fennec_paw.Conn
module Paw = Fennec_paw.Paw
module H = Fennec_core.Http

let make ?(field = "_method") ?(header = "x-http-method-override") () : Paw.t =
 fun c ->
  if Conn.meth c <> H.POST then c
  else
    let ov = match Conn.req_header c header with Some v -> Some v | None -> Conn.body_param c field in
    match Option.map String.uppercase_ascii ov with
    | Some (("PUT" | "PATCH" | "DELETE") as m) -> Conn.override_method c (H.meth_of_string m)
    | _ -> c

(* ──── method_override tests ──── *)

let%test "POST + _method=PUT -> PUT" =
  let mo = make () in
  let c = mo (Conn.make (H.make_request ~meth:H.POST ~headers:[ ("content-type", "application/x-www-form-urlencoded") ] ~body:"_method=PUT" ~path:"/" ())) in
  Conn.meth c = H.PUT

let%test "header override -> DELETE" =
  let mo = make () in
  let c = mo (Conn.make (H.make_request ~meth:H.POST ~headers:[ ("x-http-method-override", "DELETE") ] ~path:"/" ())) in
  Conn.meth c = H.DELETE

let%test "GET is untouched" =
  let mo = make () in
  let c = mo (Conn.make (H.make_request ~meth:H.GET ~path:"/" ())) in
  Conn.meth c = H.GET
