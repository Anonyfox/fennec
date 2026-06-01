(* Dev-mode livereload — the PURE parts: the client script and the HTML injection.

   Design (see examples/CLI-INTEROP.md): all build-output watching lives in the
   CLI; the framework only relays. Livereload has two cases, and the client script
   handles both with one mechanism — a dedicated websocket that the browser keeps open:

   - Backend change: dune rebuilds the exe, the CLI restarts the server, the
     livereload socket drops. The script polls and, on reconnect, reloads. No
     server-side signal needed — the disconnect IS the reload trigger.
   - Frontend-only change (CSS/JS): the server stays up, so the socket stays
     open; the CLI (which watches the built bundles) pings the server's dev control
     socket, the server pushes a frame ("reload" or "css"), and the script acts on it.

   The script is injected into HTML responses in memory (before </body>); it
   NEVER rewrites a file on disk. Disabled entirely outside dev mode, so a prod
   build ships none of this. *)

let endpoint = "/_fennec/livereload"

(* A tiny, dependency-free client. Keeps a websocket open; when it drops (server
   restart) it reconnects, and once the server is back it reloads. A "css" text
   frame hot-swaps stylesheets with no reload; anything else does a full reload.

   Reconnect strategy (mirrors what mature dev servers like Vite settled on, tuned
   for a LOCAL server that's down only ~0.3–0.5s): a flat, fast retry interval — no
   exponential backoff, because backoff exists to spare a remote/overloaded server
   and only ADDS latency for localhost. While the tab is hidden we stop retrying
   (pointless), and we reconnect IMMEDIATELY on focus / visibility-change — the
   single biggest perceived-latency win, since the dev's tab-switch back from the
   editor is exactly when they want the page fresh. The poll itself can't be
   removed (no browser API signals "endpoint reachable again"), only made cheap,
   fast, and event-gated. *)
let client_script =
  Printf.sprintf
    {js|(function(){
  var EP = "%s";
  var url = (location.protocol === "https:" ? "wss://" : "ws://") + location.host + EP;
  var RETRY_MS = 250;
  var hadConnection = false;
  var live = false;       // is a socket currently open?
  var pending = null;     // a scheduled reconnect timer, if any
  function swapCss(){
    var links = document.querySelectorAll('link[rel="stylesheet"]');
    links.forEach(function(l){
      var u = l.href.replace(/([?&])_fennec=\d+/, "$1") .replace(/[?&]$/, "");
      l.href = u + (u.indexOf("?") >= 0 ? "&" : "?") + "_fennec=" + Date.now();
    });
  }
  function schedule(){
    if (pending !== null || live || document.hidden) return; // hidden: resume on visibility
    pending = setTimeout(connect, RETRY_MS);
  }
  function reconnectNow(){
    if (live) return;
    if (pending !== null) { clearTimeout(pending); pending = null; }
    connect();
  }
  function connect(){
    pending = null;
    var ws;
    try { ws = new WebSocket(url); } catch(e) { return schedule(); }
    ws.onopen = function(){
      live = true;
      if (hadConnection) { location.reload(); return; }
      hadConnection = true;
    };
    ws.onmessage = function(e){
      if (e.data === "css") swapCss();
      else location.reload();
    };
    ws.onclose = function(){ live = false; schedule(); };
    ws.onerror = function(){ try { ws.close(); } catch(_){} };
  }
  addEventListener("visibilitychange", function(){ if (!document.hidden) reconnectNow(); });
  addEventListener("focus", reconnectNow);
  connect();
})();|js}
    endpoint

let script_tag = Printf.sprintf "<script>%s</script>" client_script

(* find the last occurrence of [needle] in [hay], Stdlib only *)
let rfind hay needle =
  let nh = String.length hay and nn = String.length needle in
  if nn = 0 || nn > nh then None
  else
    let rec go i =
      if i < 0 then None
      else if String.sub hay i nn = needle then Some i
      else go (i - 1)
    in
    go (nh - nn)

(* Insert the livereload script before the last </body> (or append if absent).
   In-memory only — the response body the server already produced, lightly
   transformed. *)
let inject_html (body : string) : string =
  match rfind body "</body>" with
  | Some i -> String.sub body 0 i ^ script_tag ^ String.sub body i (String.length body - i)
  | None -> body ^ script_tag
