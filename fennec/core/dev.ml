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
    // jitter the retry so that when the backend restarts, N open tabs don't reconnect AND reload
    // in lockstep (a thundering herd against a just-booted server); spread them over RETRY_MS.
    pending = setTimeout(connect, RETRY_MS + Math.floor(Math.random() * RETRY_MS));
  }
  function reconnectNow(){
    // Regaining focus/visibility is exactly when the dev wants the page fresh — and exactly
    // when `live` can lie: a backgrounded tab gets its socket throttled, so the server can
    // restart WITHOUT the close event arriving, leaving `live` stuck true. So never trust it
    // here — always tear the socket down and reconnect, re-reading the boot id (an unchanged
    // server just replies with the same id and nothing reloads; a new one triggers the reload).
    if (pending !== null) { clearTimeout(pending); pending = null; }
    if (sock) { try { sock.onclose = null; sock.onmessage = null; sock.close(); } catch(_){} sock = null; }
    live = false;
    connect();
  }
  function connect(){
    pending = null;
    try { sock = new WebSocket(url); } catch(e) { sock = null; return schedule(); }
    sock.onopen = function(){ live = true; };
    sock.onmessage = function(e){
      var d = e.data;
      if (typeof d !== "string") return;   // frames are always text; never throw on a Blob
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
  // open the socket only AFTER the page has finished loading: a websocket opened mid-navigation
  // is aborted by the browser ("interrupted while the page was loading"), which spams the console
  // on every reload. Once loaded there's no navigation to interrupt it. (Reconnects are fine — by
  // then the page is already loaded.)
  if (document.readyState === "complete") connect();
  else addEventListener("load", function(){ connect(); }, { once: true });
})();|js}
    endpoint

let script_tag = Printf.sprintf "<script>%s</script>" client_script

(* Last index of [needle] in [hay], case-insensitive, Stdlib only. Allocation-free: compares
   char-by-char rather than [String.sub]'ing a fresh substring at every position (which, on a
   page with no match, would allocate O(n) garbage). Case-insensitive so a legal "</BODY>" or
   "</Body>" is still found. *)
let rfind_ci hay needle =
  let nh = String.length hay and nn = String.length needle in
  if nn = 0 || nn > nh then None
  else
    let matches i =
      let rec go k = k >= nn || (Char.lowercase_ascii hay.[i + k] = Char.lowercase_ascii needle.[k] && go (k + 1)) in
      go 0
    in
    let rec scan i = if i < 0 then None else if matches i then Some i else scan (i - 1) in
    scan (nh - nn)

(* Insert the livereload script before the last </body> (or append if absent). In-memory only —
   the response body the server already produced, lightly transformed; spliced into the ORIGINAL
   body so its casing is preserved. *)
let inject_html (body : string) : string =
  match rfind_ci body "</body>" with
  | Some i -> String.sub body 0 i ^ script_tag ^ String.sub body i (String.length body - i)
  | None -> body ^ script_tag

(* ──── rfind_ci ──── *)
let%test "rfind_ci: finds </body>"        = rfind_ci "<html><body>hi</body></html>" "</body>" = Some 14
let%test "rfind_ci: case-insensitive"     = rfind_ci "<BODY>hi</BODY>" "</body>" <> None
let%test "rfind_ci: mixed case"           = rfind_ci "<Body>hi</Body>" "</body>" <> None
let%test "rfind_ci: not found"            = rfind_ci "<html><p>hi</p></html>" "</body>" = None
let%test "rfind_ci: empty needle"         = rfind_ci "abc" "" = None
let%test "rfind_ci: needle longer"        = rfind_ci "ab" "abcdef" = None
let%test "rfind_ci: finds LAST occurrence" =
  rfind_ci "<body></body><body></body>" "</body>" = Some 19

(* ──── inject_html ──── *)
let%test "inject: before </body>" =
  let r = inject_html "<html><body>hi</body></html>" in
  Fennec_hunt_unit.str_contains r "<script>" && Fennec_hunt_unit.str_contains r "</body>"

let%test "inject: no </body> appends" =
  let r = inject_html "<html><body>hi" in
  let len = String.length r in
  len > String.length "<html><body>hi" && String.sub r (len - String.length "</script>") (String.length "</script>") = "</script>"

let%test "inject: preserves original content" =
  let r = inject_html "<html><body>content</body></html>" in
  Fennec_hunt_unit.str_contains r "content"

let%test "inject: endpoint in output" =
  let r = inject_html "<html><body></body></html>" in
  Fennec_hunt_unit.str_contains r endpoint

let%test "inject: empty body" =
  let r = inject_html "" in
  Fennec_hunt_unit.str_contains r "<script>"
