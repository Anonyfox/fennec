(* The /greet HANDLER at the HTTP layer — a standalone .mlx (frontend/handlers/greet.mlx) mounted
   MANUALLY at /greet and reused at /hi/:name. It is a FULL HTTP handler: the SAME view + payload is
   content-negotiated into a hydrated SPA, a JSON body, or static no-JS HTML — and it stays leak-proof
   (request data + Server_only secrets never cross). *)
open Fennec_hunt.Http

let find_sub hay needle =
  let nh = String.length hay and nn = String.length needle in
  let rec go i = if i + nn > nh then None else if String.sub hay i nn = needle then Some i else go (i + 1) in
  if nn = 0 then Some 0 else go 0

let%http "handler /greet: full HTTP handler — SPA / JSON / static; leak-proof; reusable" = fun () ->
  check "default -> hydrated SPA; ONLY the codec payload crosses (leak-proof)" (fun () ->
      get "/greet?name=Ada&leak=TOPSECRET"
        ~expect:
          [ status 200; is_html;
            body_contains "Hello, Ada!"; body_contains "server-computed start = 3";
            body_contains "__FUR_DATA__"; body_contains "handler:greet";
            body_contains {|\"who\":\"Ada\"|}; body_contains "/_handlers/greet/main.js" ];
      assert (find_sub (response_body ()) "TOPSECRET" = None);
      assert (find_sub (response_body ()) "sk-live" = None));

  check "?format=json -> the SAME payload as JSON (content negotiation); no HTML/seed/bundle" (fun () ->
      get "/greet?name=Ada&format=json"
        ~expect:[ status 200; is_json; body_contains {|"who":"Ada"|}; body_contains {|"count"|} ];
      assert (find_sub (response_body ()) "__FUR_DATA__" = None);
      assert (find_sub (response_body ()) "main.js" = None));

  check "?format=static -> the SAME view as static HTML, NO JS (no bundle, no seed, no #app)" (fun () ->
      get "/greet?name=Ada&format=static" ~expect:[ status 200; is_html; body_contains "Hello, Ada!" ];
      assert (find_sub (response_body ()) "__FUR_DATA__" = None);
      assert (find_sub (response_body ()) "main.js" = None);
      assert (find_sub (response_body ()) {|id="app"|} = None));

  check "the SAME handler is mounted at a path-param route /hi/:name (reuse + params)" (fun () ->
      get "/hi/Bob" ~expect:[ status 200; is_html; body_contains "Hello, Bob!"; body_contains "/_handlers/greet/main.js" ])
