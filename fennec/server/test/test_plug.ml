(* Unit tests for the built-in plugs: request_id, method_override, basic_auth,
   force_https, metrics. *)

module Plug = Fennec_server.Plug
module Conn = Fennec_paw.Conn
module H = Fennec_core.Http
module Headers = Fennec_core.Headers

let fails = ref 0
let check name c = if c then Printf.printf "  ok   %s\n" name else (incr fails; Printf.printf "  FAIL %s\n" name)
let eq name a b = check name (a = b)

let req ?(meth = H.GET) ?(headers = []) ?(body = "") ?(host = "") ?(scheme = "http") path =
  H.make_request ~meth ~path ~headers ~body ~host ~scheme ()

let resp_of c = Option.value (Conn.resp c) ~default:(H.text ~status:404 "")
let finalize c = Conn.apply_before_send c (resp_of c)

let () =
  print_endline "Plug.request_id:";
  let c = Plug.request_id () (Conn.make (req "/")) in
  check "sets the assign" (Plug.current_request_id c <> None);
  check "sets the response header"
    (match Conn.resp (Conn.text c "x") with Some r -> Headers.mem r.H.headers "x-request-id" | None -> false);
  let c2 = Plug.request_id () (Conn.make (req ~headers:[ ("X-Request-Id", "abc123") ] "/")) in
  eq "reuses an inbound id" (Plug.current_request_id c2) (Some "abc123");
  (* freshly minted ids are unique (atomic counter, domain-safe) *)
  let id_of c = Option.value (Plug.current_request_id c) ~default:"" in
  let a = id_of (Plug.request_id () (Conn.make (req "/"))) in
  let b = id_of (Plug.request_id () (Conn.make (req "/"))) in
  check "minted request ids are unique" (a <> "" && a <> b)

let () =
  print_endline "Plug.method_override:";
  let mo = Plug.method_override () in
  let c = mo (Conn.make (req ~meth:H.POST ~headers:[ ("content-type", "application/x-www-form-urlencoded") ] ~body:"_method=PUT" "/")) in
  eq "POST + _method=PUT -> PUT" (Conn.meth c) H.PUT;
  let c = mo (Conn.make (req ~meth:H.POST ~headers:[ ("x-http-method-override", "DELETE") ] "/")) in
  eq "header override -> DELETE" (Conn.meth c) H.DELETE;
  let c = mo (Conn.make (req ~meth:H.GET "/")) in
  eq "GET is untouched" (Conn.meth c) H.GET

let () =
  print_endline "Plug.basic_auth:";
  let ba = Plug.basic_auth ~username:"user" ~password:"pass" () in
  let auth = "Basic " ^ Base64.encode_string "user:pass" in
  let c_ok = ba (Conn.make (req ~headers:[ ("authorization", auth) ] "/")) in
  check "correct credentials decline (pass through)" (not (Conn.answered c_ok));
  let c_no = ba (Conn.make (req "/")) in
  eq "missing auth -> 401" (resp_of c_no).H.status 401;
  check "401 carries a challenge" (Headers.mem (resp_of c_no).H.headers "www-authenticate");
  let bad = "Basic " ^ Base64.encode_string "user:wrong" in
  eq "wrong password -> 401" (resp_of (ba (Conn.make (req ~headers:[ ("authorization", bad) ] "/")))).H.status 401

let () =
  print_endline "Plug.force_https:";
  let fh = Plug.force_https () in
  let c = fh (Conn.make (req ~host:"example.com" "/a/b")) in
  eq "http -> 301" (resp_of c).H.status 301;
  eq "redirects to https target" (Headers.get (resp_of c).H.headers "location") (Some "https://example.com/a/b");
  let c2 = fh (Conn.make (req ~host:"example.com" ~headers:[ ("x-forwarded-proto", "https") ] "/")) in
  check "already https declines" (not (Conn.answered c2))

let () =
  print_endline "Plug.metrics:";
  let seen = ref None in
  let mp = Plug.metrics (fun ~meth ~path ~status ~duration_ms:_ -> seen := Some (meth, path, status)) in
  let c = mp (Conn.make (req ~meth:H.POST "/m")) in
  let c = Conn.text ~status:201 c "ok" in
  let _ = finalize c in
  eq "reports method/path/status at send" !seen (Some ("POST", "/m", 201))

let () =
  if !fails = 0 then print_endline "all Plug tests passed."
  else (Printf.printf "%d FAILED\n" !fails; exit 1)
