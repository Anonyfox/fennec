(* Unit tests for cookie parsing + Set-Cookie serialization. *)

module C = Fennec_core.Cookie

let fails = ref 0
let check name c = if c then Printf.printf "  ok   %s\n" name else (incr fails; Printf.printf "  FAIL %s\n" name)
let eq name a b = check name (a = b)
let has name s sub =
  let n = String.length s and m = String.length sub in
  let rec go i = i + m <= n && (String.sub s i m = sub || go (i + 1)) in
  check name (m = 0 || go 0)

let () =
  print_endline "Cookie.parse_header:";
  eq "single" (C.parse_header "a=1") [ ("a", "1") ];
  eq "multiple" (C.parse_header "a=1; b=2") [ ("a", "1"); ("b", "2") ];
  eq "no space after ;" (C.parse_header "a=1;b=2") [ ("a", "1"); ("b", "2") ];
  eq "quoted value stripped" (C.parse_header {|s="abc"|}) [ ("s", "abc") ];
  eq "empty value" (C.parse_header "a=") [ ("a", "") ];
  eq "value with =" (C.parse_header "t=a=b") [ ("t", "a=b") ];
  eq "blank segments ignored" (C.parse_header "a=1; ; b=2") [ ("a", "1"); ("b", "2") ];
  eq "empty header" (C.parse_header "") [];

  print_endline "Cookie.to_set_cookie:";
  let basic = C.to_set_cookie ~name:"sid" ~value:"xyz" () in
  has "name=value" basic "sid=xyz";
  has "default path /" basic "Path=/";
  has "default SameSite=Lax" basic "SameSite=Lax";
  has "default HttpOnly" basic "HttpOnly";
  check "no Secure by default" (not (String.length basic >= 0 && (let n=String.length basic and m=6 in let rec go i = i+m<=n && (String.sub basic i m = "Secure" || go (i+1)) in go 0)));
  let full =
    C.to_set_cookie ~name:"s" ~value:"v" ~path:"/app" ~domain:"example.com" ~max_age:3600
      ~secure:true ~http_only:false ~same_site:C.Strict ()
  in
  has "custom path" full "Path=/app";
  has "domain" full "Domain=example.com";
  has "max-age" full "Max-Age=3600";
  has "secure when asked" full "Secure";
  has "SameSite=Strict" full "SameSite=Strict";
  check "no HttpOnly when off"
    (not (let n=String.length full and m=8 in let rec go i = i+m<=n && (String.sub full i m = "HttpOnly" || go (i+1)) in go 0));
  (* SameSite=None forces Secure *)
  let none = C.to_set_cookie ~name:"s" ~value:"v" ~same_site:C.None_ () in
  has "SameSite=None" none "SameSite=None";
  has "None implies Secure" none "Secure";

  if !fails = 0 then print_endline "all Cookie tests passed."
  else (Printf.printf "%d FAILED\n" !fails; exit 1)
