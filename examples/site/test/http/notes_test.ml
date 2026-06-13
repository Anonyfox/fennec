(* End-to-end proof of the typed middle layer (fennec.web) against a LIVE server: the /notes CRUD
   resource — server-rendered HTML, typed forms validated through the Codec model, CSRF, method
   override (PUT/DELETE from POST), content negotiation, and session flash. Each [check] has its own
   cookie jar; the store is server-side, so notes created in one check are visible in the next. *)
open Fennec_hunt.Http

(* pull the CSRF token a rendered form embeds in its hidden input *)
let find_sub hay needle =
  let nh = String.length hay and nn = String.length needle in
  let rec go i = if i + nn > nh then None else if String.sub hay i nn = needle then Some i else go (i + 1) in
  go 0

let csrf_token () =
  let b = response_body () in
  let marker = {|name="_csrf_token" value="|} in
  match find_sub b marker with
  | None -> failwith "no _csrf_token in the rendered form"
  | Some i ->
      let start = i + String.length marker in
      let stop = String.index_from b start '"' in
      String.sub b start (stop - start)

let%http "notes CRUD: the server-rendered middle layer end to end" = fun () ->
  check "index renders as HTML" (fun () ->
      get "/notes" ~expect:[ status 200; is_html; body_contains "Notes"; body_contains "New note" ]);

  check "create: CSRF-protected POST → redirect (PRG) → show page with flash" (fun () ->
      get "/notes/new" ~expect:[ status 200; body_contains "_csrf_token"; body_contains {|name="title"|} ];
      let tok = csrf_token () in
      post "/notes"
        ~form:[ ("title", "Buy milk"); ("body", "the 2% kind"); ("priority", "2"); ("pinned", "on"); ("_csrf_token", tok) ]
        ~follow:true
        ~expect:[ status 200; body_contains "Buy milk"; body_contains "Note created." ]);

  check "CSRF: a POST without a token is rejected" (fun () ->
      post "/notes" ~form:[ ("title", "x"); ("body", "y"); ("priority", "1") ] ~expect:[ status 403 ]);

  check "validation: an invalid POST re-renders the form with field errors (422)" (fun () ->
      get "/notes/new";
      let tok = csrf_token () in
      post "/notes"
        ~form:[ ("title", ""); ("body", "ok"); ("priority", "9"); ("_csrf_token", tok) ]
        ~expect:[ status 422; body_contains "field-errors"; body_contains {|name="title"|} ]);

  check "update + delete via method override (PUT/DELETE from a POST form)" (fun () ->
      (* create, capturing the new id from the redirect Location *)
      get "/notes/new";
      let tok = csrf_token () in
      post "/notes" ~form:[ ("title", "Draft"); ("body", "b"); ("priority", "1"); ("_csrf_token", tok) ] ~expect:[ status 302 ];
      let loc = header "location" in
      get loc ~expect:[ status 200; body_contains "Draft" ];
      (* update via _method=PUT *)
      get (loc ^ "/edit") ~expect:[ status 200; body_contains "Draft" ];
      let tok2 = csrf_token () in
      post loc
        ~form:[ ("_method", "PUT"); ("title", "Edited"); ("body", "b"); ("priority", "3"); ("_csrf_token", tok2) ]
        ~follow:true
        ~expect:[ status 200; body_contains "Edited"; body_contains "Note updated." ];
      (* delete via _method=DELETE from the show page's delete form *)
      get loc;
      let tok3 = csrf_token () in
      post loc ~form:[ ("_method", "DELETE"); ("_csrf_token", tok3) ] ~follow:true
        ~expect:[ status 200; body_contains "Note deleted." ];
      get loc ~expect:[ status 404 ])
