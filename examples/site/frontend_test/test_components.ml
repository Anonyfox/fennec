(* Unit tests for the mlx components: render each to an HTML string (Fur.to_html, the
   native SSR path) and assert on the output. Lives in its own dir, NOT inside
   frontend/, whose route_gen rule + data_only_dirs would otherwise absorb it.

   A Fur component is [props -> unit -> (unit -> vnode)]: applying the props + unit
   runs setup and returns the render thunk; calling it yields the vnode to render. *)

let fails = ref 0
let check name c = if c then Printf.printf "  ok   %s\n" name else (incr fails; Printf.printf "  FAIL %s\n" name)

(* substring search, stdlib only *)
let has hay needle =
  let nh = String.length hay and nn = String.length needle in
  let rec go i = i + nn <= nh && (String.sub hay i nn = needle || go (i + 1)) in
  nn = 0 || go 0

let render (thunk : unit -> Fur.vnode) = Fur.to_html (thunk ())

let () =
  let hero = render (Hero.make ~title:"Pricing" ~subtitle:"Free in beta" ()) in
  check "hero: section class" (has hero "class=\"hero\"");
  check "hero: title text" (has hero "Pricing");
  check "hero: subtitle text" (has hero "Free in beta");
  check "hero: CTA link" (has hero "hero-cta");

  let counter = render (Counter.make ~label:"seats" ()) in
  check "counter: class" (has counter "class=\"counter\"");
  check "counter: label" (has counter "seats:");
  check "counter: initial count 0" (has counter ">0<");
  check "counter: inline scope attr" (has counter "data-fur=");

  let nav = render (Nav.make ~links:[ ("/", "Home"); ("/about", "About") ] ()) in
  check "nav: home link" (has nav "href=\"/\"");
  check "nav: about label" (has nav "About");

  (* CO-LOCATED data (Data.local): with no SSR source set, Greeting renders the fallback + loading +
     refetch button — exactly like the old Data.string form (the migration kept the client behaviour). *)
  Fur.Data.clear_seed ();
  let greeting = render (Greeting.make ()) in
  check "greeting: fallback shown" (has greeting "…");
  check "greeting: refetch button" (has greeting "id=\"refetch\"");

  (* PROOF #1 — the co-located SSR seed: declaring Greeting REGISTERED its inline server fetcher under
     /api/greeting (no server.ml api_source arm). The SSR driver runs that registered source between
     render passes; here we replay exactly that — fetch the registered producer, seed it, re-render — and
     the value appears with NO loading flash (the "(ready)" frame, not "(loading)"). This is the seed half
     of the kill: the value reached SSR straight from the component's inline declaration. *)
  (match Fur.Data.local_source "/api/greeting" with
   | Some src ->
     check "greeting (Data.local): producer registered under the derived path" true;
     check "greeting (Data.local): producer is text (not json)" (not (Fur.Data.src_is_json src));
     Fur.Data.clear_seed ();
     Fur.Data.put_seed "/api/greeting" (Fur.Data.src_produce src);   (* what the SSR driver seeds *)
     let seeded = render (Greeting.make ()) in
     check "greeting (Data.local): SSR seed shows the server value (no loading flash)"
       (has seeded "Hello from the server" && has seeded "(ready)" && not (has seeded "(loading)"));
     Fur.Data.clear_seed ()
   | None -> check "greeting (Data.local): producer registered under /api/greeting" false);

  (* TYPED resource (Data.model): seed exactly what the server's Sift.encode_json emits, then render —
     the card decodes the seed through Site_info.codec and shows the TYPED fields (name/tagline/stars),
     proving the typed value round-trips server-encode -> client-decode through the seed. *)
  Fur.Data.clear_seed ();
  Fur.Data.put_seed "/api/site-info"
    (Sift.encode_json Site_info.codec { Site_info.name = "Fennec"; tagline = "OCaml, end to end."; stars = 1280 });
  let card = render (Site_card.make ()) in
  check "site_card (Data.model): typed name from the seed" (has card ">Fennec<");
  check "site_card (Data.model): typed tagline from the seed" (has card "OCaml, end to end.");
  check "site_card (Data.model): typed int field rendered" (has card "1280");
  Fur.Data.clear_seed ();

  (* global store: empty at start, Stats reflects it *)
  let stats = render (Stats.make ()) in
  check "stats: reads store count" (has stats "todos in store: 0");

  (* the SSR (cacheable) frame of the reactive-user contract: with no session — exactly the
     server-render case, where [Accounts.current_user ()] resolves to None — User_badge renders the
     ANONYMOUS frame ("Sign in"), never any personalized "Hello, …". This is the half of the contract
     that makes the SSR HTML identical for every visitor and safe to edge-cache. *)
  let badge = render (User_badge.make ()) in
  check "user_badge: fixed-size slot present" (has badge "class=\"user-badge\"");
  check "user_badge: anonymous frame (Sign in)" (has badge "Sign in");
  check "user_badge: anonymous label" (has badge "Guest");
  check "user_badge: no personalized content in the cacheable SSR frame" (not (has badge "Hello,"));

  (* the Mode-B MIRROR of the badge above: a standalone handler's [view] renders the user's data
     PERSONALIZED server-side from its payload (the server [load] read the userId off the Conn and put
     exactly this here), so the first paint is already personalized — and the SAME tree hydrates from
     the seed, byte-identical, no snap. See frontend/handlers/me.mlx + the Mode-B README section. *)
  let me =
    Fur.to_html
      (Site_handlers.Me.view
         { Site_handlers.Me.username = "alice";
           roles = [ "admin" ];
           emails = [ { Site_handlers.Me.Email.address = "alice@example.com"; verified = true } ] })
  in
  check "me (Mode B): personalized server-render names the user" (has me "Signed in as alice");
  check "me (Mode B): role rendered" (has me "admin");
  check "me (Mode B): verified email rendered" (has me "alice@example.com");
  check "me (Mode B): verified marker" (has me "verified");

  (* forms: controlled input + add button render server-side *)
  let todos = render (Todo_list.make ()) in
  check "todo_list: input present" (has todos "id=\"todo-input\"");
  check "todo_list: add button" (has todos "id=\"add\"");
  (* the migrated todo input carries its [maxlength] STRAIGHT OFF Todo_form.codec ([@max_len 80]) — the
     codec-driven HTML5 attr (Form.input_attrs), not a hand-written number; Add is disabled while the
     empty draft is invalid (the [@non_empty] refinement, validated reactively with no round-trip) *)
  check "todo_list: input maxlength comes off the codec" (has todos "maxlength=\"80\"");
  check "todo_list: add disabled while the draft is invalid" (has todos "disabled");

  (* the TYPED CLIENT FORM (Form.signal): the SAME Hello_form.codec the server /hello handler validates.
     Its <input> gets [required] + [maxlength=40] from the codec (Form.input_attrs), and the empty draft
     is invalid so the Greet button renders disabled — all from one Sift model, no hand-written rules. *)
  let nf = render (Name_form.make ()) in
  check "name_form: input present" (has nf "id=\"hf-name\"");
  check "name_form: required comes off the codec" (has nf "required=\"required\"");
  check "name_form: maxlength=40 comes off the codec ([@max_len 40])" (has nf "maxlength=\"40\"");
  check "name_form: Greet disabled while the empty draft is invalid ([@non_empty])" (has nf "disabled");

  (* email: the SAME [%%style] component machinery, inlined for email via Fur_email (server-side, pure).
     The engine in isolation — an explicit stylesheet inlines by (scope, class): *)
  let ss = Fur_email.of_rules [ ("s1", "box", "color:red"); ("s1", "p", "margin:0") ] in
  let boxed = Fur_email.to_email ~stylesheet:ss (Fur.h "div" [ Fur.attr "data-fur" "s1"; Fur.class_ "box" ] [ Fur.text "hi" ]) in
  check "inline: matched class decls land in style=" (has boxed "style=\"color:red\"");
  check "inline: unscoped element gets no style"
    (not (has (Fur_email.to_email ~stylesheet:ss (Fur.h "div" [] [ Fur.text "x" ])) "style="));

  (* the real welcome email through the AMBIENT stylesheet. The app installs the precompiled styles ONCE
     (exactly this line, in server.ml); thereafter render with NOTHING passed — no stylesheet, no tokens
     (var(--brand) was resolved to a literal at build time). *)
  Fur_email.install Site_styles.inline;
  let mail = Fur_email.to_email (Email_welcome.make ~name:"Ada" ~url:"https://x.test/confirm" () ()) in
  check "email: wrapper styles inlined from [%%style]" (has mail "max-width: 480px");
  check "email: var(--brand) resolved to a literal (build-time)" (has mail "background: #e8590c");
  check "email: no var() left in the output" (not (has mail "var("));
  check "email: dynamic content rendered" (has mail "Welcome, Ada!");
  check "email: href preserved" (has mail "https://x.test/confirm");

  if !fails = 0 then print_endline "all component tests passed."
  else (Printf.printf "%d FAILED\n" !fails; exit 1)
