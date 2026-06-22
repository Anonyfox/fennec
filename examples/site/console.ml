(* The console entry point — the whole of it. `fennec console` builds and runs this bytecode toplevel,
   which boots the app's runtime (no HTTP) and serves the eval socket the terminal driver connects to.
   `Fennec.console_run` is NOT `Fennec.serve`, so server discovery ignores this target; the app's real
   server stays the single `server.bc`/`server.exe`. The app's whole library graph is linked above
   (with -linkall) so a REPL phrase can reach any of it. *)
let () = Fennec.console_run ()
