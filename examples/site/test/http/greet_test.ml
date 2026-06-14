(* The /greet PAGE at the HTTP layer — the cross-stage boundary, asserted on the actual bytes the
   server sends. Proves the model AND its safety property: the conn block (stage 1) runs with the full
   request in scope, but the ONLY thing that crosses to the client is [Greet.codec]'s encoding of the
   SHAPED payload — request data it did not seed never appears. Leak-proof by construction. *)
open Fennec_hunt.Http

let find_sub hay needle =
  let nh = String.length hay and nn = String.length needle in
  let rec go i = if i + nn > nh then None else if String.sub hay i nn = needle then Some i else go (i + 1) in
  if nn = 0 then Some 0 else go 0

let%http "page /greet: cross-stage payload SSRs; only the codec payload crosses" = fun () ->
  check "the cross-stage payload SSRs, and ONLY the codec payload crosses (leak-proof)" (fun () ->
      (* the request carries a secret-looking param the conn block has in scope but does NOT seed *)
      get "/greet?name=Ada&leak=TOPSECRET"
        ~expect:
          [ status 200; is_html;
            body_contains "Hello, Ada!" (* the cross-stage value, server-computed, present pre-JS *);
            body_contains "server-computed start = 3" (* count = String.length "Ada" *);
            body_contains "__FUR_DATA__" (* the seed channel *);
            body_contains "page:greet" (* keyed by the page key *);
            body_contains {|\"who\":\"Ada\"|} (* the shaped payload itself, in the seed *);
            body_contains "/_pages/greet/main.js" (* the page's OWN bundle for hydration, no app router *) ];
      (* leak-proof: ?leak=TOPSECRET was fully in the conn block's scope, but it is not in the payload,
         so it is nowhere in the response. Only what you encode crosses — the Conn cannot be seeded. *)
      assert (find_sub (response_body ()) "TOPSECRET" = None);
      (* #3: the conn block holds a secret as Server_only.t (sk-live-…). It has no Codec, so it CANNOT
         be put in the payload — and indeed it is nowhere in the response. A leak is a compile error. *)
      assert (find_sub (response_body ()) "sk-live" = None))
