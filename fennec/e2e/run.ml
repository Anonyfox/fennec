(* The Model-A harness: own the Eio loop, (optionally) boot the app server, launch the
   browser(s), and run every registered {!Live} test with a fresh isolated page each,
   bounded by [config.jobs]. Everything is created under one [Switch], so on return — pass,
   fail, or exception — every browser, socket, and temp profile is gone.

   [main_cli] adds argv flags for fast iteration: --grep one test, --bail on first failure,
   --headed, --jobs N, --timeout S, --browsers M, and a positional app-server path. *)

let port_of_base_url u =
  match String.rindex_opt u ':' with
  | Some i -> ( match int_of_string_opt (String.sub u (i + 1) (String.length u - i - 1)) with Some p -> p | None -> 80 )
  | None -> 80

let wait_http_ready ~sw ~net ~clock ~port =
  let deadline = Eio.Time.now clock +. 30.0 in
  let rec loop () =
    match Chrome.http_get ~sw net ~port ~path:"/" with
    | _ -> ()
    | exception _ ->
      if Eio.Time.now clock > deadline then failwith "app server never became ready";
      Eio.Time.sleep clock 0.1;
      loop ()
  in
  loop ()

let main ?binary ?reporter ?(browsers = 1) ?(headless = true) ?server_exe ~base_url ~(config : Live.config) () : Live.report =
  let reporter = match reporter with Some r -> r | None -> Reporter.create () in
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let net = (Eio.Stdenv.net env :> [ `Generic ] Eio.Net.ty Eio.Resource.t) in
  let clock = Eio.Stdenv.clock env in
  let proc_mgr = Eio.Stdenv.process_mgr env and fs = Eio.Stdenv.fs env in
  let config = { config with Live.base_url } in
  let port = port_of_base_url base_url in
  (match server_exe with
   | None -> ()
   | Some exe ->
     (* serve the dev web root WITHOUT livereload, which would otherwise reload the page
        spontaneously and make a controlled run non-deterministic. The spawned server
        inherits this env. (Fennec-specific; harmless to any other server, which ignores it.) *)
     Unix.putenv "FENNEC_DEV_LIVERELOAD" "0";
     let devnull = Eio.Path.open_out ~sw ~create:(`If_missing 0o644) (Eio.Path.(/) fs "/dev/null") in
     ignore (Eio.Process.spawn ~sw proc_mgr ~stdout:devnull ~stderr:devnull [ exe ]);
     wait_http_ready ~sw ~net ~clock ~port);
  let browsers = max 1 browsers in
  (* one browser process + one long-lived control connection each (reader alive for the whole
     run, so context teardown never hangs). Per test: an isolated context + its own page
     connection, on the test switch. *)
  let fleet =
    List.init browsers (fun _ ->
        let chrome = Chrome.launch ~sw ~net ~clock ~proc_mgr ~fs ?binary ~headless () in
        (chrome, Chrome.connect ~sw chrome))
  in
  let next = ref 0 in
  let pick () = let x = List.nth fleet (!next mod browsers) in incr next; x in
  let provision f =
    let chrome, browser = pick () in
    Eio.Switch.run (fun test_sw -> f (Cdp_backend.create_isolated ~sw:test_sw ~browser ~chrome))
  in
  let tests = Live.registered () in
  let n = List.length (Live.select config tests) in
  Reporter.run_started reporter ~total:n ~jobs:config.Live.jobs ~grep:config.Live.grep
    ~note:(Printf.sprintf "on %d browser(s)" browsers) ();
  let report = Live.run ~reporter ~clock ~config ~provision tests in
  Reporter.run_finished reporter report;
  report

(* simple argv flag parser → run → exit status; for a runner executable *)
let main_cli ?binary ~base_url () =
  let grep = ref None and bail = ref false and jobs = ref 1 and headed = ref false in
  let timeout = ref 5.0 and browsers = ref 1 and server = ref None and retries = ref 0 in
  let style = ref Reporter.Auto and color = ref None and ascii = ref false in
  let rec parse = function
    | [] -> ()
    | "--grep" :: v :: r -> grep := Some v; parse r
    | "--bail" :: r -> bail := true; parse r
    | "--jobs" :: v :: r -> jobs := (try int_of_string v with _ -> !jobs); parse r
    | "--retries" :: v :: r -> retries := (try int_of_string v with _ -> !retries); parse r
    | "--headed" :: r -> headed := true; parse r
    | "--timeout" :: v :: r -> timeout := (try float_of_string v with _ -> !timeout); parse r
    | "--browsers" :: v :: r -> browsers := (try int_of_string v with _ -> !browsers); parse r
    | "--server" :: v :: r -> server := Some v; parse r
    (* reporter controls: pick the look explicitly, or force colour/ascii on a sink whose
       capabilities we can't sniff (e.g. a CI that hides the TTY but renders ANSI) *)
    | "--reporter" :: v :: r ->
      style := (match v with "plain" -> Reporter.Plain | "pretty" -> Reporter.Pretty | _ -> Reporter.Auto); parse r
    | "--color" :: r -> color := Some true; parse r
    | "--no-color" :: r -> color := Some false; parse r
    | "--ascii" :: r -> ascii := true; parse r
    | s :: r when String.length s < 2 || String.sub s 0 2 <> "--" -> server := Some s; parse r (* positional = server exe *)
    | _ :: r -> parse r
  in
  parse (List.tl (Array.to_list Sys.argv));
  let caps0 = Reporter.detect_caps () in
  let caps =
    { caps0 with
      color = (match !color with Some b -> b | None -> caps0.Reporter.color);
      unicode = (if !ascii then false else caps0.Reporter.unicode) }
  in
  let reporter = Reporter.create ~style:!style ~caps () in
  let config =
    { Live.default_config with jobs = (if !bail then 1 else !jobs); retries = !retries; bail = !bail;
      grep = !grep; step_timeout = !timeout }
  in
  let r = main ?binary ~reporter ~browsers:!browsers ~headless:(not !headed) ?server_exe:!server ~base_url ~config () in
  if r.Live.failed > 0 then exit 1
