(* Request logger — one line per request once the response is finalized:

     [fennec] 200 GET /path (1.4ms) <request-id?>

   The status drives a colour on a terminal (2xx green, 3xx cyan, 4xx yellow, 5xx red),
   and if a Request_id paw ran upstream its id is appended for log↔trace correlation. It
   declines (passes through), only registering a before_send hook to read the final status
   and timing. Pass [?sink] to redirect the line elsewhere (a file, JSON, syslog) — a custom
   sink is written verbatim (never colourised). *)

module Conn = Fennec_paw.Conn
module Paw = Fennec_paw.Paw
module H = Fennec_core.Http

(* colour only when the default stderr sink is a real terminal and NO_COLOR is unset —
   the same restraint the e2e reporter uses, so piped/redirected logs stay clean *)
let color_enabled =
  lazy
    ((try Unix.isatty Unix.stderr with _ -> false)
    && match Sys.getenv_opt "NO_COLOR" with Some s when s <> "" -> false | _ -> true)

let paint (status : int) (s : string) : string =
  let code =
    if status >= 500 then "31" else if status >= 400 then "33" else if status >= 300 then "36" else "32"
  in
  "\027[" ^ code ^ "m" ^ s ^ "\027[0m"

let make ?sink () : Paw.t =
 fun c ->
  let t0 = Unix.gettimeofday () in
  let meth = H.string_of_meth (Conn.meth c) and path = Conn.path c in
  Conn.before_send c (fun r ->
      let ms = (Unix.gettimeofday () -. t0) *. 1000.0 in
      let rid = match Request_id.current c with Some id -> " " ^ id | None -> "" in
      let colored = Option.is_none sink && Lazy.force color_enabled in
      let status = let s = string_of_int r.H.status in if colored then paint r.H.status s else s in
      let line = Printf.sprintf "[fennec] %s %s %s (%.1fms)%s\n" status meth path ms rid in
      (match sink with Some f -> f line | None -> prerr_string line);
      r)

(* ──── logger tests ──── *)

let req_ ?(meth = H.GET) ?(headers = []) path = H.make_request ~meth ~path ~headers ()
let finalize_ c = Conn.apply_before_send c (Option.value (Conn.resp c) ~default:(H.text ~status:404 ""))

let%test_unit "logs method, path, status, duration" =
  let buf = Buffer.create 64 in
  let lg = make ~sink:(Buffer.add_string buf) () in
  let _ = finalize_ (Conn.text ~status:201 (lg (Conn.make (req_ ~meth:H.POST "/x"))) "ok") in
  let out = Buffer.contents buf in
  Fennec_hunt_unit.check "logs method" (Fennec_hunt_unit.str_contains out "POST");
  Fennec_hunt_unit.check "logs path" (Fennec_hunt_unit.str_contains out "/x");
  Fennec_hunt_unit.check "logs status" (Fennec_hunt_unit.str_contains out "201");
  Fennec_hunt_unit.check "logs duration in ms" (Fennec_hunt_unit.str_contains out "ms")

let%test "custom sink is never colourised" =
  let buf = Buffer.create 64 in
  let lg = make ~sink:(Buffer.add_string buf) () in
  let _ = finalize_ (Conn.text ~status:201 (lg (Conn.make (req_ ~meth:H.POST "/x"))) "ok") in
  let out = Buffer.contents buf in
  not (Fennec_hunt_unit.str_contains out "\027[")

let%test_unit "logs the request id when present" =
  let buf2 = Buffer.create 64 in
  let c = Request_id.make () (Conn.make (req_ ~headers:[ ("X-Request-Id", "abc123") ] "/")) in
  let _ = finalize_ (Conn.text (make ~sink:(Buffer.add_string buf2) () c) "ok") in
  Fennec_hunt_unit.check "request id logged" (Fennec_hunt_unit.str_contains (Buffer.contents buf2) "abc123")
