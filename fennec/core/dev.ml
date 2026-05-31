(* Dev-mode livereload — the PURE parts: the client script and the HTML injection.

   Design (see examples/CLI-INTEROP.md): the framework reacts to build *outputs*,
   never to source files. Livereload has two cases, and the client script handles
   both with one mechanism — a dedicated websocket that the browser keeps open:

   - Backend change: dune rebuilds the exe, the CLI restarts the server, the
     livereload socket drops. The script polls and, on reconnect, reloads. No
     server-side signal needed — the disconnect IS the reload trigger.
   - Frontend-only change (CSS/JS): the server stays up, so the socket stays
     open; the framework's asset poller pushes a frame ("reload" or "css") and
     the script acts on it.

   The script is injected into HTML responses in memory (before </body>); it
   NEVER rewrites a file on disk. Disabled entirely outside dev mode, so a prod
   build ships none of this. *)

let endpoint = "/_fennec/livereload"

(* A tiny, dependency-free client. Keeps a websocket open; on any close it polls
   to reconnect, and once the server is back it reloads (covers server restart).
   A "css" text frame hot-swaps stylesheets with no reload; anything else (e.g.
   "reload") does a full reload. *)
let client_script =
  Printf.sprintf
    {js|(function(){
  var EP = "%s";
  var url = (location.protocol === "https:" ? "wss://" : "ws://") + location.host + EP;
  var hadConnection = false;
  function swapCss(){
    var links = document.querySelectorAll('link[rel="stylesheet"]');
    links.forEach(function(l){
      var u = l.href.replace(/([?&])_fennec=\d+/, "$1") .replace(/[?&]$/, "");
      l.href = u + (u.indexOf("?") >= 0 ? "&" : "?") + "_fennec=" + Date.now();
    });
  }
  function connect(){
    var ws;
    try { ws = new WebSocket(url); } catch(e) { return retry(); }
    ws.onopen = function(){
      if (hadConnection) { location.reload(); return; }
      hadConnection = true;
    };
    ws.onmessage = function(e){
      if (e.data === "css") swapCss();
      else location.reload();
    };
    ws.onclose = function(){ retry(); };
    ws.onerror = function(){ try { ws.close(); } catch(_){} };
  }
  function retry(){ setTimeout(connect, 500); }
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
