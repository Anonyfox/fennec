(* Real-browser end-to-end test for the site example — pure OCaml/Eio, ZERO npm. It
   boots the native server.exe, launches a headless Chrome, and drives it over the Chrome
   DevTools Protocol (see cdp.ml — a hand-written WebSocket+CDP client over Eio) to prove
   the things SSR alone can't: actual js_of_ocaml HYDRATION, local + global signal state,
   isomorphic data + fast-render + refetch, the Browser/localStorage facade, on_mount,
   controlled form inputs, SPA navigation, dynamic params, the catch-all, and strict
   per-app bundle isolation.

   Run with examples/site/e2e/run.sh (builds deps, then execs this). Override the browser
   with CHROME=… and the server binary with argv(1). *)

let chrome_default = "/Applications/Google Chrome.app/Contents/MacOS/Google Chrome"
let cdp_port = 9456
let web = "http://localhost:8200"
let admin = "http://localhost:8201"

let fails = ref 0
let check name ok = if ok then Printf.printf "  ok   %s\n%!" name else (incr fails; Printf.printf "  FAIL %s\n%!" name)

let starts p s = String.length s >= String.length p && String.sub s 0 (String.length p) = p

(* tiny HTTP/1.1 GET over Eio. The server is keep-alive, so we MUST honour Content-Length
   (reading to EOF would block) — read the status line + headers, then exactly N bytes. *)
let http_get ~sw net ~port ~path =
  let flow = Eio.Net.connect ~sw net (`Tcp (Eio.Net.Ipaddr.V4.loopback, port)) in
  Eio.Flow.copy_string
    (Printf.sprintf "GET %s HTTP/1.1\r\nHost: localhost:%d\r\nConnection: close\r\n\r\n" path port)
    (flow :> _ Eio.Flow.sink);
  let r = Eio.Buf_read.of_flow (flow :> _ Eio.Flow.source) ~max_size:(8 * 1024 * 1024) in
  let _status = Eio.Buf_read.line r in
  let clen = ref None in
  let rec headers () =
    match Eio.Buf_read.line r with
    | "" -> ()
    | h ->
      let lower = String.lowercase_ascii h in
      if starts "content-length:" lower then
        clen := int_of_string_opt (String.trim (String.sub h 15 (String.length h - 15)));
      headers ()
  in
  headers ();
  match !clen with Some n -> Eio.Buf_read.take n r | None -> Eio.Buf_read.take_all r
let ws_path url =
  let u = if starts "ws://" url then String.sub url 5 (String.length url - 5) else url in
  match String.index_opt u '/' with Some i -> String.sub u i (String.length u - i) | None -> "/"

(* retry a thunk until it returns without raising, or time out *)
let wait_ready clock ~desc ~timeout f =
  let deadline = Eio.Time.now clock +. timeout in
  let rec loop () =
    match f () with
    | x -> x
    | exception e -> if Eio.Time.now clock > deadline then failwith (desc ^ ": " ^ Printexc.to_string e)
                     else (Eio.Time.sleep clock 0.1; loop ())
  in
  loop ()

let () =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let net = Eio.Stdenv.net env and clock = Eio.Stdenv.clock env in
  let pmgr = Eio.Stdenv.process_mgr env and fs = Eio.Stdenv.fs env in
  let server_exe = if Array.length Sys.argv > 1 then Sys.argv.(1) else "_build/default/examples/site/server.exe" in
  let chrome = try Sys.getenv "CHROME" with Not_found -> chrome_default in
  let devnull = Eio.Path.open_out ~sw ~create:(`If_missing 0o644) (Eio.Path.(/) fs "/tmp/fennec_e2e.log") in
  let spawn args = Eio.Process.spawn ~sw pmgr ~stdout:devnull ~stderr:devnull args in

  (* 1. boot the server + a headless Chrome with a CDP debug port. Start from a clean
     browser profile each run so a previously-cached bundle can never be served. *)
  ignore (Sys.command "rm -rf /tmp/fennec_e2e_chrome");
  let server = spawn [ server_exe ] in
  let chrome_proc =
    spawn
      [ chrome; "--headless=new"; "--disable-gpu"; "--no-sandbox"; "--no-first-run";
        "--user-data-dir=/tmp/fennec_e2e_chrome"; "--disk-cache-size=1"; "--disable-application-cache";
        Printf.sprintf "--remote-debugging-port=%d" cdp_port; "--remote-allow-origins=*"; "about:blank" ]
  in
  let cleanup () =
    (try Eio.Process.signal chrome_proc Sys.sigkill with _ -> ());
    (try Eio.Process.signal server Sys.sigkill with _ -> ())
  in
  Fun.protect ~finally:cleanup @@ fun () ->
  (* wait for the server to answer *)
  ignore (wait_ready clock ~desc:"server boot" ~timeout:15.0 (fun () ->
              let b = http_get ~sw net ~port:8200 ~path:"/" in
              if Cdp.contains b "Fennec" then b else failwith "not ready yet"));
  (* wait for Chrome's debug endpoint, get the browser websocket *)
  let ws_url =
    wait_ready clock ~desc:"chrome boot" ~timeout:15.0 (fun () ->
        let v = http_get ~sw net ~port:cdp_port ~path:"/json/version" in
        match Cdp.field "webSocketDebuggerUrl" (Yojson.Safe.from_string v) with
        | Some (`String u) -> u
        | _ -> failwith "no ws url")
  in
  let browser = Cdp.attach (Cdp.ws_connect ~sw net ~port:cdp_port ~path:(ws_path ws_url)) clock in
  let sess = Cdp.new_page browser in
  let t = browser in
  let pollc = Cdp.poll_contains t sess in
  let eval_s = Cdp.eval_str t sess and eval_b = Cdp.eval_bool t sess in

  Printf.printf "\n-- hydration + isomorphic data --\n%!";
  Cdp.navigate t sess (web ^ "/");
  ignore (pollc ~desc:"hydration (on_mount)" ~want:"mounted ✓" ~timeout:15.0
            "document.querySelector('.mounted').textContent");
  check "hydration: on_mount ran" true;
  check "data: fast-render value present" (Cdp.contains (eval_s "document.querySelector('.greeting .msg').textContent") "Hello from the server");
  check "data: seeded resource is ready (no flash)" (Cdp.contains (eval_s "document.querySelector('.greeting .gstatus').textContent") "(ready)");
  ignore (pollc ~desc:"client-only data fetch" ~want:"fetched live in the browser" ~timeout:5.0
            "document.querySelector('.bdata').textContent");
  check "data: client-only resource fetched after hydration" true;

  Printf.printf "-- local signal state (counter) --\n%!";
  ignore (pollc ~desc:"counter click → 1" ~want:"1" ~timeout:5.0
            "(function(){document.querySelector('.cbtn.inc').click();return document.querySelector('.count').textContent})()");
  check "counter: increments on click" true;
  ignore (pollc ~desc:"counter click → 2" ~want:"2" ~timeout:5.0
            "(function(){document.querySelector('.cbtn.inc').click();return document.querySelector('.count').textContent})()");
  ignore (pollc ~desc:"counter click → 1" ~want:"1" ~timeout:5.0
            "(function(){document.querySelector('.cbtn.dec').click();return document.querySelector('.count').textContent})()");
  check "counter: decrements on click" true;

  Printf.printf "-- Browser facade (localStorage) --\n%!";
  check "localStorage: first visit = 1" (Cdp.contains (eval_s "document.querySelector('.visits').textContent") "visits (localStorage): 1");
  Cdp.navigate t sess (web ^ "/");
  ignore (pollc ~desc:"re-hydrate" ~want:"mounted ✓" ~timeout:15.0 "document.querySelector('.mounted').textContent");
  ignore (pollc ~desc:"localStorage persists/increments" ~want:"visits (localStorage): 2" ~timeout:5.0
            "document.querySelector('.visits').textContent");
  check "localStorage: persists + increments to 2" true;

  Printf.printf "-- data refetch --\n%!";
  ignore (eval_s "document.querySelector('#refetch').click()");
  ignore (pollc ~desc:"refetch returns" ~want:"(ready)" ~timeout:5.0 "document.querySelector('.greeting .gstatus').textContent");
  check "data: refetch keeps server value" (Cdp.contains (eval_s "document.querySelector('.greeting .msg').textContent") "Hello from the server");

  Printf.printf "-- SPA navigation (no reload) --\n%!";
  ignore (eval_s "window.__spa = 1");
  ignore (eval_s "document.querySelector('.nav-link[href=\"/products\"]').click()");
  ignore (pollc ~desc:"SPA nav to /products" ~want:"/products" ~timeout:5.0 "location.pathname");
  check "spa: url changed to /products" (eval_s "location.pathname" = "/products");
  check "spa: products content rendered" (Cdp.contains (eval_s "document.body.textContent") "todos in store");
  check "spa: no full reload (window marker survived)" (eval_b "window.__spa === 1");

  Printf.printf "-- global store + forms (each/keyed) --\n%!";
  check "store: starts empty" (Cdp.contains (eval_s "document.querySelector('.stats').textContent") "todos in store: 0");
  ignore (eval_s "(function(){var e=document.querySelector('#todo-input');e.value='milk';e.dispatchEvent(new Event('input',{bubbles:true}));e.dispatchEvent(new KeyboardEvent('keydown',{key:'Enter',bubbles:true}))})()");
  ignore (pollc ~desc:"add via Enter" ~want:"todos in store: 1" ~timeout:5.0 "document.querySelector('.stats').textContent");
  check "forms: Enter adds todo, store updates Stats" true;
  check "forms+each: keyed row rendered" (Cdp.contains (eval_s "document.querySelector('.todo-items').textContent") "milk");
  ignore (eval_s "(function(){var e=document.querySelector('#todo-input');e.value='eggs';e.dispatchEvent(new Event('input',{bubbles:true}));document.querySelector('#add').click()})()");
  ignore (pollc ~desc:"add via button" ~want:"todos in store: 2" ~timeout:5.0 "document.querySelector('.stats').textContent");
  check "forms: add-button adds second todo" true;
  ignore (eval_s "document.querySelector('.todo .rm').click()");
  ignore (pollc ~desc:"remove row" ~want:"todos in store: 1" ~timeout:5.0 "document.querySelector('.stats').textContent");
  check "store: remove updates Stats" true;

  Printf.printf "-- dynamic route param (typed Paths, SPA) --\n%!";
  ignore (eval_s "document.querySelector('.p7').click()");
  ignore (pollc ~desc:"SPA nav to /products/7" ~want:"/products/7" ~timeout:5.0 "location.pathname");
  check "param: /products/7 renders Product #7" (Cdp.contains (eval_s "document.body.textContent") "Product #7");

  Printf.printf "-- catch-all (not_found) --\n%!";
  Cdp.navigate t sess (web ^ "/nope/xyz");
  ignore (pollc ~desc:"catch-all" ~want:"no route: /nope/xyz" ~timeout:10.0 "document.body.textContent");
  check "catch-all: unmatched path renders not_found with param" true;

  Printf.printf "-- strict per-app bundle isolation --\n%!";
  Cdp.navigate t sess (web ^ "/");
  ignore (pollc ~desc:"home reload" ~want:"mounted ✓" ~timeout:15.0 "document.querySelector('.mounted').textContent");
  check "isolation: web bundle EXCLUDES admin-only code"
    (not (eval_b "(async()=>(await (await fetch('/_apps/web/main.js')).text()).includes('admin actions'))()"));

  Printf.printf "-- admin app (separate endpoint, shared components, recolored) --\n%!";
  Cdp.navigate t sess (admin ^ "/");
  ignore (pollc ~desc:"admin counter present" ~want:"admin actions" ~timeout:15.0 "document.body.textContent");
  check "admin: whitelabel body class" (Cdp.contains (eval_s "document.body.className") "admin");
  ignore (pollc ~desc:"admin counter hydrates" ~want:"1" ~timeout:5.0
            "(function(){document.querySelector('.cbtn.inc').click();return document.querySelector('.count').textContent})()");
  check "admin: shared Counter hydrates independently" true;
  check "isolation: admin bundle INCLUDES admin-only code"
    (eval_b "(async()=>(await (await fetch('/_apps/admin/main.js')).text()).includes('admin actions'))()");

  Printf.printf "\n%s\n%!" (if !fails = 0 then "site e2e OK (real browser)" else Printf.sprintf "%d check(s) failed" !fails);
  if !fails <> 0 then exit 1
