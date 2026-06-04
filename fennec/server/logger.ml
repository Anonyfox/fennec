(* Request logger — prints "method path -> status" once the response is finalized
   (the status is read in a before_send hook). Declines (passes through). *)

module Conn = Fennec_paw.Conn
module Paw = Fennec_paw.Paw
module H = Fennec_core.Http

(* [sink] defaults to stderr; pass your own to route the line elsewhere. *)
let make ?(sink = prerr_string) () : Paw.t =
 fun c ->
  let meth = H.string_of_meth (Conn.meth c) in
  let path = Conn.path c in
  Conn.before_send c (fun r ->
      sink (Printf.sprintf "[fennec] %s %s -> %d\n" meth path r.H.status);
      r)
