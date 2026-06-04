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

(* A tiny, dependency-free client. Keeps a websocket open; on connect the server sends a
   "boot:<id>" frame (its per-process id). The client adopts the FIRST id it sees and reloads
   only when it later sees a DIFFERENT one — i.e. the server was actually replaced by a new
   process (a real backend restart). A mere reconnect to the SAME server (a transient network
   blip, a paused laptop, a flaky socket, a livereload ws that hiccupped) is NOT a restart, so
   it does NOT reload. This is the fix for reload storms: the reload trigger is a change of
   server identity, not the disconnect. "css" hot-swaps stylesheets with no reload; "reload"
   forces one.

   Reconnect strategy (tuned for a LOCAL server down only ~0.3–0.5s): a flat, fast retry — no
   backoff (it only adds latency for localhost). While the tab is hidden we stop retrying, and
   reconnect IMMEDIATELY on focus/visibility-change — the dev's tab-switch back from the editor
   is exactly when they want the page fresh. *)
let client_script =
  Printf.sprintf
    {js|(function(){
  var EP = "%s";
  var url = (location.protocol === "https:" ? "wss://" : "ws://") + location.host + EP;
  var RETRY_MS = 250;
  var sock = null;        // the current socket (may be STALE on a throttled background tab)
  var live = false;       // is a socket currently open?
  var pending = null;     // a scheduled reconnect timer, if any
  var bootId = null;      // the server's process id, adopted on first sight
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
    // Regaining focus/visibility is exactly when the dev wants the page fresh — and exactly
    // when `live` can lie: a backgrounded tab gets its socket throttled, so the server can
    // restart WITHOUT the close event arriving, leaving `live` stuck true. So never trust it
    // here — always tear the socket down and reconnect, re-reading the boot id (an unchanged
    // server just replies with the same id and nothing reloads; a new one triggers the reload).
    if (pending !== null) { clearTimeout(pending); pending = null; }
    if (sock) { try { sock.onclose = null; sock.close(); } catch(_){} sock = null; }
    live = false;
    connect();
  }
  function connect(){
    pending = null;
    try { sock = new WebSocket(url); } catch(e) { sock = null; return schedule(); }
    sock.onopen = function(){ live = true; };
    sock.onmessage = function(e){
      var d = e.data;
      if (d.slice(0,5) === "boot:") {
        var id = d.slice(5);
        if (bootId === null) bootId = id;            // first connection: adopt, do not reload
        else if (id !== bootId) location.reload();   // server is a NEW process: reload once
        return;
      }
      if (d === "css") swapCss();
      else location.reload();
    };
    sock.onclose = function(){ live = false; sock = null; schedule(); };
    sock.onerror = function(){ try { sock.close(); } catch(_){} };
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
