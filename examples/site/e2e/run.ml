(* Runner entrypoint (Model A): the suite modules in this directory register their tests at
   load; this parses flags (--grep/--bail/--headed/--jobs/--timeout/--browsers + a
   positional app-server path), boots the server + a headless browser, and runs them with a
   fresh isolated context each. One process, in-process lifecycle, torn down on exit. *)
let () = Site.load (* force-link the suite module so its test registrations run *)
let () = Fennec_e2e.Run.main_cli ~base_url:"http://localhost:4001" ()
