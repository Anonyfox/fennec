(* Unit tests for the prebuilt middleware paws: Request_id, Method_override,
   Basic_auth, Force_https, Metrics. *)

module Request_id = Fennec_server.Request_id
module Method_override = Fennec_server.Method_override
module Basic_auth = Fennec_server.Basic_auth
module Force_https = Fennec_server.Force_https
module Metrics = Fennec_server.Metrics
module Security_headers = Fennec_server.Security_headers
module Logger = Fennec_server.Logger
module Conn = Fennec_paw.Conn
module H = Fennec_core.Http
module Headers = Fennec_core.Headers

let fails = ref 0
let check name c = if c then Printf.printf "  ok   %s\n" name else (incr fails; Printf.printf "  FAIL %s\n" name)
let eq name a b = check name (a = b)

let contains s sub =
  let ls = String.length s and lb = String.length sub in
  let rec go i = i + lb <= ls && (String.sub s i lb = sub || go (i + 1)) in
  lb = 0 || go 0

let req ?(meth = H.GET) ?(headers = []) ?(body = "") ?(host = "") ?(scheme = "http") path =
  H.make_request ~meth ~path ~headers ~body ~host ~scheme ()

let resp_of c = Option.value (Conn.resp c) ~default:(H.text ~status:404 "")
let finalize c = Conn.apply_before_send c (resp_of c)

let () =
  print_endline "Request_id:";
  let c = Request_id.make () (Conn.make (req "/")) in
  check "sets the assign" (Request_id.current c <> None);
  check "sets the response header"
    (match Conn.resp (Conn.text c "x") with Some r -> Headers.mem r.H.headers "x-request-id" | None -> false);
  let c2 = Request_id.make () (Conn.make (req ~headers:[ ("X-Request-Id", "abc123") ] "/")) in
  eq "reuses an inbound id" (Request_id.current c2) (Some "abc123");
  (* freshly minted ids are unique (atomic counter, domain-safe) *)
  let id_of c = Option.value (Request_id.current c) ~default:"" in
  let a = id_of (Request_id.make () (Conn.make (req "/"))) in
  let b = id_of (Request_id.make () (Conn.make (req "/"))) in
  check "minted request ids are unique" (a <> "" && a <> b);
  (* a crafted inbound id (control bytes / over-long) is NOT echoed — a fresh one is minted *)
  let ctrl = id_of (Request_id.make () (Conn.make (req ~headers:[ ("X-Request-Id", "bad\r\nInjected: 1") ] "/"))) in
  check "control-char inbound id rejected (minted instead)" (ctrl <> "" && ctrl <> "bad\r\nInjected: 1");
  let long = String.make 200 'a' in
  let lc = id_of (Request_id.make () (Conn.make (req ~headers:[ ("X-Request-Id", long) ] "/"))) in
  check "over-long inbound id rejected" (lc <> long)

let () =
  print_endline "Method_override:";
  let mo = Method_override.make () in
  let c = mo (Conn.make (req ~meth:H.POST ~headers:[ ("content-type", "application/x-www-form-urlencoded") ] ~body:"_method=PUT" "/")) in
  eq "POST + _method=PUT -> PUT" (Conn.meth c) H.PUT;
  let c = mo (Conn.make (req ~meth:H.POST ~headers:[ ("x-http-method-override", "DELETE") ] "/")) in
  eq "header override -> DELETE" (Conn.meth c) H.DELETE;
  let c = mo (Conn.make (req ~meth:H.GET "/")) in
  eq "GET is untouched" (Conn.meth c) H.GET

let () =
  print_endline "Basic_auth:";
  let ba = Basic_auth.make ~username:"user" ~password:"pass" () in
  let auth = "Basic " ^ Base64.encode_string "user:pass" in
  let c_ok = ba (Conn.make (req ~headers:[ ("authorization", auth) ] "/")) in
  check "correct credentials decline (pass through)" (not (Conn.answered c_ok));
  let c_no = ba (Conn.make (req "/")) in
  eq "missing auth -> 401" (resp_of c_no).H.status 401;
  check "401 carries a challenge" (Headers.mem (resp_of c_no).H.headers "www-authenticate");
  let bad = "Basic " ^ Base64.encode_string "user:wrong" in
  eq "wrong password -> 401" (resp_of (ba (Conn.make (req ~headers:[ ("authorization", bad) ] "/")))).H.status 401;
  let auth_lc = "basic " ^ Base64.encode_string "user:pass" in
  check "lowercase scheme accepted (RFC 7617)" (not (Conn.answered (ba (Conn.make (req ~headers:[ ("authorization", auth_lc) ] "/")))))

let () =
  print_endline "Force_https:";
  let fh = Force_https.make () in
  let c = fh (Conn.make (req ~host:"example.com" "/a/b")) in
  eq "http -> 308 (method/body preserving)" (resp_of c).H.status 308;
  eq "redirects to https target" (Headers.get (resp_of c).H.headers "location") (Some "https://example.com/a/b");
  let c2 = fh (Conn.make (req ~host:"example.com" ~headers:[ ("x-forwarded-proto", "https") ] "/")) in
  check "already https declines" (not (Conn.answered c2));
  (* a proto LIST starting with https must also decline — not redirect into a loop *)
  let c3 = fh (Conn.make (req ~host:"example.com" ~headers:[ ("x-forwarded-proto", "https, http") ] "/")) in
  check "X-Forwarded-Proto list 'https, http' declines (no loop)" (not (Conn.answered c3));
  (* hsts: on an already-secure request, emit Strict-Transport-Security *)
  let fh2 = Force_https.make ~hsts:31536000 () in
  let cs = fh2 (Conn.make (req ~host:"example.com" ~headers:[ ("x-forwarded-proto", "https") ] "/")) in
  let rs = finalize (Conn.text cs "x") in
  check "hsts emitted on https response" (Headers.get rs.H.headers "strict-transport-security" <> None)

let () =
  print_endline "Security_headers:";
  let sh = Security_headers.make ~extra:[ ("Content-Security-Policy", "default-src 'self'"); ("X-Frame-Options", "DENY") ] () in
  let r = finalize (Conn.text (sh (Conn.make (req "/"))) "x") in
  eq "default nosniff present" (Headers.get r.H.headers "x-content-type-options") (Some "nosniff");
  eq "extra CSP added" (Headers.get r.H.headers "content-security-policy") (Some "default-src 'self'");
  eq "extra overrides default X-Frame-Options" (Headers.get r.H.headers "x-frame-options") (Some "DENY")

let () =
  print_endline "Metrics:";
  let seen = ref None in
  let mp = Metrics.make (fun ~meth ~path ~status ~duration_ms:_ -> seen := Some (meth, path, status)) in
  let c = mp (Conn.make (req ~meth:H.POST "/m")) in
  let c = Conn.text ~status:201 c "ok" in
  let _ = finalize c in
  eq "reports method/path/status at send" !seen (Some ("POST", "/m", 201))

let () =
  print_endline "Logger:";
  let buf = Buffer.create 64 in
  let lg = Logger.make ~sink:(Buffer.add_string buf) () in
  let _ = finalize (Conn.text ~status:201 (lg (Conn.make (req ~meth:H.POST "/x"))) "ok") in
  let out = Buffer.contents buf in
  check "logs method" (contains out "POST");
  check "logs path" (contains out "/x");
  check "logs status" (contains out "201");
  check "logs duration in ms" (contains out "ms");
  check "custom sink is never colourised" (not (contains out "\027["));
  (* zero-config correlation: with Request_id upstream, the id is appended *)
  let buf2 = Buffer.create 64 in
  let c = Request_id.make () (Conn.make (req ~headers:[ ("X-Request-Id", "abc123") ] "/")) in
  let _ = finalize (Conn.text (Logger.make ~sink:(Buffer.add_string buf2) () c) "ok") in
  check "logs the request id when present" (contains (Buffer.contents buf2) "abc123")

let () =
  if !fails = 0 then print_endline "all middleware tests passed."
  else (Printf.printf "%d FAILED\n" !fails; exit 1)
