(* Content-negotiation guard: 406 the request unless its [Accept] header accepts one of the media
   types this endpoint serves (Plug's [:accepts]). A missing/empty [Accept] means "no preference" and
   passes. Matching is by media range — ["*/*"] and ["type/*"] count, and an explicit [q=0] refuses a
   range. Only parses when an [Accept] header is present, and only on the endpoint that mounts it. *)

module Conn = Conn
module H = Http

let split_media s =
  match String.index_opt s '/' with
  | Some i -> (String.sub s 0 i, String.sub s (i + 1) (String.length s - i - 1))
  | None -> (s, "")

(* does media range [range] (e.g. ["application/*"]) cover the concrete type [offered]? *)
let range_accepts range offered =
  let rt, rs = split_media range and ot, os = split_media offered in
  (rt = "*" || rt = ot) && (rs = "*" || rs = os)

let make (offered : string list) : Pipeline.t =
 fun c ->
  match Conn.req_header c "accept" with
  | None | Some "" -> c (* no preference → serve it *)
  | Some accept ->
    (* one [Accept] entry like ["application/json;q=0.9"]: take the media range, honour an explicit q=0 *)
    let entry_ok entry =
      match String.split_on_char ';' entry with
      | [] -> false
      | mr :: params ->
        let mr = String.trim mr in
        let refused = List.exists (fun p -> match String.split_on_char '=' (String.trim p) with [ "q"; v ] -> ( match float_of_string_opt (String.trim v) with Some q -> q <= 0. | None -> false) | _ -> false) params in
        (not refused) && List.exists (range_accepts mr) offered
    in
    if List.exists entry_ok (String.split_on_char ',' accept) then c else Conn.text ~status:406 c "Not Acceptable"

(* ──── accepts tests ──── *)

let req_ ?accept () = H.make_request ~meth:H.GET ~path:"/" ~headers:(match accept with Some a -> [ ("accept", a) ] | None -> []) ()
let conn_ ?accept () = Conn.make (req_ ?accept ())
let declined c = Conn.resp c = None
let status_ c = (Option.value (Conn.resp c) ~default:(H.text ~status:200 "")).H.status

let%test "no Accept header passes" = declined (make [ "application/json" ] (conn_ ()))
let%test "*/* passes" = declined (make [ "application/json" ] (conn_ ~accept:"*/*" ()))
let%test "exact match passes" = declined (make [ "application/json" ] (conn_ ~accept:"application/json" ()))
let%test "type/* match passes" = declined (make [ "application/json" ] (conn_ ~accept:"application/*" ()))
let%test "no match is 406" = status_ (make [ "application/json" ] (conn_ ~accept:"text/html" ())) = 406
let%test "q=0 refusal is 406" = status_ (make [ "application/json" ] (conn_ ~accept:"application/json;q=0" ())) = 406
let%test "one of many matches passes" = declined (make [ "application/json" ] (conn_ ~accept:"text/html, application/json;q=0.9, */*;q=0.1" ()))
let%test "first wins on offered list" = declined (make [ "text/html"; "application/json" ] (conn_ ~accept:"application/json" ()))
