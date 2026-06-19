(* BYTE-EXACT HTML ORACLE for the bare-text pre-pass migration.

   Renders every example component to its FULL HTML (Fur.to_html, the same native SSR path the
   unit tests use) and prints a labelled dump. Run BEFORE the .mlx migration and AFTER it; the two
   dumps must be byte-identical (the pre-pass is a pure source transform → same AST → same HTML).

   This is a stronger oracle than the substring asserts in test_components.ml: it compares the WHOLE
   rendered tree, so any whitespace/attribute/text-collapse difference shows up in a plain `diff`.
   Not wired into the default test alias — it is a manual proof tool (run via `dune exec`). *)

let line name html = Printf.printf "----- %s -----\n%s\n" name html
let render name (thunk : unit -> Fur.vnode) = line name (Fur.to_html (thunk ()))

let () =
  render "hero" (Hero.make ~title:"Pricing" ~subtitle:"Free in beta" ());
  render "counter" (Counter.make ~label:"seats" ());
  render "counter_default" (Counter.make ());
  render "nav" (Nav.make ~links:[ ("/", "Home"); ("/about", "About") ] ());
  Fur.Data.clear_seed ();
  render "greeting_fallback" (Greeting.make ());
  (match Fur.Data.local_source "/api/greeting" with
   | Some src ->
     Fur.Data.clear_seed ();
     Fur.Data.put_seed "/api/greeting" (Fur.Data.src_produce src);
     render "greeting_seeded" (Greeting.make ());
     Fur.Data.clear_seed ()
   | None -> ());
  Fur.Data.clear_seed ();
  Fur.Data.put_seed "/api/site-info"
    (Sift.encode_json Site_info.codec
       { Site_info.name = "Fennec"; tagline = "OCaml, end to end."; stars = 1280 });
  render "site_card" (Site_card.make ());
  Fur.Data.clear_seed ();
  render "stats" (Stats.make ());
  render "user_badge" (User_badge.make ());
  line "me"
    (Fur.to_html
       (Site_handlers.Me.view
          { Site_handlers.Me.username = "alice";
            roles = [ "admin" ];
            emails = [ { Site_handlers.Me.Email.address = "alice@example.com"; verified = true } ] }));
  render "todo_list" (Todo_list.make ());
  render "name_form" (Name_form.make ());
  Fur_email.install Site_styles.inline;
  line "email_welcome"
    (Fur_email.to_email (Email_welcome.make ~name:"Ada" ~url:"https://x.test/confirm" () ()));
  (* the remaining markup components, rendered with representative props *)
  render "browser_box" (Browser_box.make ());
  render "outbox_view" (Outbox_view.make ());
  render "task_list" (Task_list.make ());
  render "user_badge_again" (User_badge.make ());
  render "stats_again" (Stats.make ());
  render "visits" (Visits.make ());
  ()
