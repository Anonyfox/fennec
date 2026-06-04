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
