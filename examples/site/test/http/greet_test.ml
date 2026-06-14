(* The /greet HANDLER at the HTTP layer — a standalone .mlx (frontend/handlers/greet.mlx) mounted
   MANUALLY at /greet and reused at /hi/:name. Proves the cross-stage boundary + its safety: load runs
   with the full request, but the ONLY thing that crosses is the codec payload — request data and
   Server_only secrets never appear. *)
open Fennec_hunt.Http

let find_sub hay needle =
  let nh = String.length hay and nn = String.length needle in
  let rec go i = if i + nn > nh then None else if String.sub hay i nn = needle then Some i else go (i + 1) in
  if nn = 0 then Some 0 else go 0

let%http "handler /greet: cross-stage payload SSRs; only the codec payload crosses; reusable" = fun () ->
  check "the cross-stage payload SSRs, and ONLY the codec payload crosses (leak-proof)" (fun () ->
      get "/greet?name=Ada&leak=TOPSECRET"
        ~expect:
          [ status 200; is_html;
            body_contains "Hello, Ada!"; body_contains "server-computed start = 3";
            body_contains "__FUR_DATA__"; body_contains "handler:greet" (* filename-derived seed key *);
            body_contains {|\"who\":\"Ada\"|}; body_contains "/_handlers/greet/main.js" (* own bundle *) ];
      assert (find_sub (response_body ()) "TOPSECRET" = None) (* request data never crosses *);
      assert (find_sub (response_body ()) "sk-live" = None) (* Server_only secret: no codec, no leak *));

  check "the SAME handler is mounted at a path-param route /hi/:name (reuse + params)" (fun () ->
      get "/hi/Bob" ~expect:[ status 200; is_html; body_contains "Hello, Bob!"; body_contains "/_handlers/greet/main.js" ])
