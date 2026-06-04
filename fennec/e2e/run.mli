(** The batteries-included harness: own the Eio loop, optionally boot the app server, launch
    the browser(s), and run every registered {!Live} test with a fresh isolated page each,
    bounded by [config.jobs]. Everything is created under one switch, so on return — pass,
    fail, or exception — every browser, socket, and temp profile is gone.

    For full control (a custom backend, your own process lifecycle), use {!Live.run}
    directly; this module is the common case wired up for you. *)

(** Run every registered test and return the tally. Boots [server_exe] first if given
    (waiting for it to accept HTTP), launches [browsers] headless Chrome processes
    (default 1; [headless] off shows a window; [binary] overrides the Chrome path), and
    reports via [reporter] (default: one auto-detected for the terminal). *)
val main :
  ?binary:string ->
  ?reporter:Reporter.t ->
  ?browsers:int ->
  ?headless:bool ->
  ?server_exe:string ->
  base_url:string ->
  config:Live.config ->
  unit ->
  Live.report

(** A ready-made CLI entry point: parse argv — [--grep], [--bail], [--jobs N], [--retries N],
    [--headed], [--timeout S], [--browsers M], [--reporter auto|plain|pretty], [--color],
    [--no-color], [--ascii], and a positional server-exe path — run the suite, and [exit 1]
    if anything failed. Drop this in an executable's [let () = ...]. *)
val main_cli : ?binary:string -> base_url:string -> unit -> unit
