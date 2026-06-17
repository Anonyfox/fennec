(* End-to-end proof of a server-rendered HANDLER against a live server: GET renders the page+form
   (static HTML, no JS), POST reads typed input + validates over the Sift model, CSRF-gates the
   write, and flashes through a redirect (post-redirect-get). Each [check] has its own cookie jar. *)
open Fennec_hunt.Http

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

let%http "hello handler: server-rendered form, typed POST, CSRF + flash" = fun () ->
  check "GET renders a static HTML page with the form (no JS bundle)" (fun () ->
      get "/hello"
        ~expect:
          [ status 200; is_html; body_contains "Say hello"; body_contains {|name="name"|}; body_contains "_csrf_token" ];
      assert (not (find_sub (response_body ()) "<script" <> None)));

  check "POST without a CSRF token is rejected" (fun () ->
      post "/hello" ~form:[ ("name", "Ada") ] ~expect:[ status 403 ]);

  check "valid POST redirects (PRG) and the flash shows on the next page" (fun () ->
      get "/hello";
      let tok = csrf_token () in
      post "/hello" ~form:[ ("name", "Ada"); ("_csrf_token", tok) ] ~follow:true
        ~expect:[ status 200; body_contains "Hello, Ada!" ]);

  check "codec-invalid POST re-renders the form with the per-field error (422)" (fun () ->
      get "/hello";
      let tok = csrf_token () in
      (* [@non_empty] fails -> the codec's own message surfaces through Form.error (no hand-written string) *)
      post "/hello" ~form:[ ("name", ""); ("_csrf_token", tok) ] ~expect:[ status 422; body_contains "must not be empty" ]);

  check "a business-rule error re-renders (422) with the message AND the submitted input preserved" (fun () ->
      get "/hello";
      let tok = csrf_token () in
      (* submit returns `again [...]` — a first-class re-render; Form.value repopulates the input so the
         user can correct it (awaiting data corrections), not retype from scratch *)
      post "/hello" ~form:[ ("name", "admin"); ("_csrf_token", tok) ]
        ~expect:[ status 422; body_contains "reserved"; body_contains {|value="admin"|} ]);

  check "no cross-request <head> leak: /hello does not inherit another page's <title>" (fun () ->
      (* the web home (/) sets <title>Home — Fennec Site</title>; this form handler sets no title. With
         per-request Head isolation, /hello's fresh render must NOT carry the home's title across requests
         (the bug this guards: a shared global Head registry leaking one request's tags into the next). *)
      get "/";
      get "/hello" ~expect:[ status 200 ];
      assert (find_sub (response_body ()) "Home — Fennec Site" = None))
