(* Headless-Chrome process lifecycle, made leak-proof by Eio. A launched browser is tied
   to the [Switch] it was created in: when that scope ends — normal exit, exception, or
   crash unwinding — the process is SIGKILL'd and its throwaway profile directory removed.
   There is no code path that leaves a browser behind.

   Port handshake uses Chrome's own DevToolsActivePort file (written once the debug server
   is up) rather than a guessed port, so there is no race and no fixed-port collision. *)

exception No_browser of string

(* ---- binary discovery: $CHROME, then the usual platform locations / $PATH ---- *)
let executable_exists p = try Sys.file_exists p && not (Sys.is_directory p) with _ -> false

let which name =
  match Sys.getenv_opt "PATH" with
  | None -> None
  | Some path ->
    String.split_on_char ':' path
    |> List.find_map (fun dir ->
           let p = Filename.concat dir name in
           if executable_exists p then Some p else None)

let find_binary () =
  let candidates =
    (match Sys.getenv_opt "CHROME" with Some p -> [ p ] | None -> [])
    @ [ "/Applications/Google Chrome.app/Contents/MacOS/Google Chrome";
        "/Applications/Chromium.app/Contents/MacOS/Chromium";
        "/Applications/Google Chrome Canary.app/Contents/MacOS/Google Chrome Canary" ]
  in
  match List.find_opt executable_exists candidates with
  | Some p -> p
  | None -> (
    match List.find_map which [ "google-chrome"; "google-chrome-stable"; "chromium"; "chromium-browser"; "chrome" ] with
    | Some p -> p
    | None -> raise (No_browser "no Chrome/Chromium found; install one or set CHROME=/path/to/chrome"))

(* ---- tiny HTTP/1.1 GET (keep-alive aware: honour Content-Length, never read to EOF) -- *)
let starts p s = String.length s >= String.length p && String.sub s 0 (String.length p) = p

let http_get ~sw net ~port ~path =
  let flow = Eio.Net.connect ~sw net (`Tcp (Eio.Net.Ipaddr.V4.loopback, port)) in
  Eio.Flow.copy_string
    (Printf.sprintf "GET %s HTTP/1.1\r\nHost: localhost:%d\r\nConnection: close\r\n\r\n" path port)
    (flow :> _ Eio.Flow.sink);
  let r = Eio.Buf_read.of_flow (flow :> _ Eio.Flow.source) ~max_size:(8 * 1024 * 1024) in
  let _ = Eio.Buf_read.line r in
  let clen = ref None in
  let rec hdrs () =
    match Eio.Buf_read.line r with
    | "" -> ()
    | l ->
      let lo = String.lowercase_ascii l in
      if starts "content-length:" lo then clen := int_of_string_opt (String.trim (String.sub l 15 (String.length l - 15)));
      hdrs ()
  in
  hdrs ();
  match !clen with Some n -> Eio.Buf_read.take n r | None -> Eio.Buf_read.take_all r

let ws_path url =
  let u = if starts "ws://" url then String.sub url 5 (String.length url - 5) else url in
  match String.index_opt u '/' with Some i -> String.sub u i (String.length u - i) | None -> "/"

(* ---- the browser handle ---- *)
type t = {
  binary : string;
  port : int;
  ws_url : string;
  net : [ `Generic ] Eio.Net.ty Eio.Resource.t;
  clock : float Eio.Time.clock_ty Eio.Resource.t;
  call_timeout : float;
}

let rm_rf dir = ignore (Sys.command (Printf.sprintf "rm -rf %s" (Filename.quote dir)))

let read_active_port dir =
  let f = Filename.concat dir "DevToolsActivePort" in
  if not (Sys.file_exists f) then None
  else
    match In_channel.with_open_bin f In_channel.input_all with
    | s -> ( match String.split_on_char '\n' s with l :: _ -> int_of_string_opt (String.trim l) | [] -> None )
    | exception _ -> None

(* Launch a browser inside [sw]. Killed + profile-purged when [sw] ends, guaranteed. *)
let launch ~sw ~net ~clock ~proc_mgr ~fs ?binary ?(headless = true) ?(call_timeout = 15.0)
    ?(extra_args = []) () : t =
  let binary = match binary with Some b -> b | None -> find_binary () in
  let profile = Filename.temp_dir "fennec_hunt_chrome" "" in
  let devnull = Eio.Path.open_out ~sw ~create:(`If_missing 0o644) (Eio.Path.(/) fs "/dev/null") in
  let args =
    [ binary ]
    @ (if headless then [ "--headless=new" ] else [])
    @ [ "--disable-gpu"; "--no-sandbox"; "--no-first-run"; "--no-default-browser-check";
        "--disable-background-networking"; "--disable-extensions"; "--disk-cache-size=1";
        "--remote-allow-origins=*"; "--remote-debugging-port=0";
        Printf.sprintf "--user-data-dir=%s" profile ]
    @ extra_args @ [ "about:blank" ]
  in
  let proc = Eio.Process.spawn ~sw proc_mgr ~stdout:devnull ~stderr:devnull args in
  (* leak-proof teardown: SIGKILL + purge the profile. We do NOT await the process here —
     headless Chrome spawns a tree of helper processes, and waiting on the group can wedge
     the whole shutdown; SIGKILL is immediate and the OS reaps on our exit. *)
  Eio.Switch.on_release sw (fun () ->
      (try Eio.Process.signal proc Sys.sigkill with _ -> ());
      rm_rf profile);
  (* wait for the debug server: DevToolsActivePort, then /json/version *)
  let deadline = Eio.Time.now clock +. 30.0 in
  let rec wait_port () =
    match read_active_port profile with
    | Some p -> p
    | None -> if Eio.Time.now clock > deadline then raise (No_browser "browser never opened its debug port");
              Eio.Time.sleep clock 0.02; wait_port ()
  in
  let port = wait_port () in
  let rec wait_ws () =
    match http_get ~sw net ~port ~path:"/json/version" with
    | v -> ( match Cdp.field "webSocketDebuggerUrl" (Yojson.Safe.from_string v) with
             | Some (`String u) -> u
             | _ -> raise (No_browser "debug endpoint returned no webSocketDebuggerUrl") )
    | exception _ -> if Eio.Time.now clock > deadline then raise (No_browser "debug endpoint never became ready");
                     Eio.Time.sleep clock 0.02; wait_ws ()
  in
  let ws_url = wait_ws () in
  { binary; port; ws_url; net = (net :> _ Eio.Net.ty Eio.Resource.t); clock; call_timeout }

(* A fresh browser-level CDP connection (its own WebSocket + reader fiber, tied to [sw]). *)
let connect ~sw (t : t) : Cdp.t =
  Cdp.attach ~sw ~timeout:t.call_timeout (Cdp.ws_connect ~sw t.net ~port:t.port ~path:(ws_path t.ws_url)) t.clock

(* A dedicated CDP connection to one page target (its own WebSocket + reader fiber). *)
let connect_page ~sw (t : t) target_id : Cdp.t =
  Cdp.attach ~sw ~timeout:t.call_timeout
    (Cdp.ws_connect ~sw t.net ~port:t.port ~path:("/devtools/page/" ^ target_id)) t.clock

let port t = t.port
let binary t = t.binary
let clock t = t.clock
