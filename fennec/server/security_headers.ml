(* Security headers — adds a small set of conservative defaults to every response via a
   before_send hook. Each default is set only if absent (an explicit header wins), and
   [extra] lets an app add or override headers it needs (e.g. a Content-Security-Policy or
   Strict-Transport-Security) — [extra] takes precedence. Header names are matched
   case-insensitively, so an extra never duplicates a default. Declines (passes through). *)

module Conn = Fennec_paw.Conn
module Paw = Fennec_paw.Paw
module H = Fennec_core.Http

let lower = String.lowercase_ascii
let has k hs = List.exists (fun (hk, _) -> lower hk = lower k) hs
let add k v hs = if has k hs then hs else (k, v) :: hs
let set k v hs = (k, v) :: List.filter (fun (hk, _) -> lower hk <> lower k) hs

let make ?(extra = []) () : Paw.t =
 fun c ->
  Conn.before_send c (fun r ->
      let h = r.H.headers in
      (* app-supplied extras win (replace any same-named header, ours included) *)
      let h = List.fold_left (fun h (k, v) -> set k v h) h extra in
      let h = add "X-Content-Type-Options" "nosniff" h in
      let h = add "X-Frame-Options" "SAMEORIGIN" h in
      let h = add "Referrer-Policy" "strict-origin-when-cross-origin" h in
      { r with H.headers = h })

(* ──── security_headers tests ──── *)

module Headers_ = Fennec_core.Headers
let req_ path = H.make_request ~meth:H.GET ~path ()
let finalize_ c = Conn.apply_before_send c (Option.value (Conn.resp c) ~default:(H.text ~status:404 ""))

let%test_unit "default nosniff + extra CSP + extra X-Frame-Options" =
  let sh = make ~extra:[ ("Content-Security-Policy", "default-src 'self'"); ("X-Frame-Options", "DENY") ] () in
  let r = finalize_ (Conn.text (sh (Conn.make (req_ "/"))) "x") in
  Fennec_hunt_unit.check "nosniff present" (Headers_.get r.H.headers "x-content-type-options" = Some "nosniff");
  Fennec_hunt_unit.check "extra CSP added" (Headers_.get r.H.headers "content-security-policy" = Some "default-src 'self'");
  Fennec_hunt_unit.check "extra overrides X-Frame-Options" (Headers_.get r.H.headers "x-frame-options" = Some "DENY")
