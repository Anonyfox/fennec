(* The real {!Backend.S} over a live browser. Each page is a target in its OWN isolated
   BrowserContext (incognito-grade), created per test and disposed when its switch ends.

   Waiting is EVENT-DRIVEN and done IN THE PAGE: a small harness ([__fennecWait]) is
   injected on every document; given a structured condition it resolves a Promise the
   instant the condition holds (a MutationObserver fires on DOM changes; a rAF loop covers
   layout/url/JS conditions), racing an in-page setTimeout. The OCaml side awaits that
   Promise with a SINGLE Runtime.evaluate — so a wait costs one round-trip, not one per
   poll tick. On timeout the harness returns a precise per-condition diagnostic, and a
   console/pageerror capture (installed before app code) is folded in. *)

module J = Yojson.Safe
module Cond = Backend.Cond
module Diag = Backend.Diag

(* Per-page state. We track, from CDP events on the page's reader fiber:
   - [ctxid]: the live default execution context of the main frame, so every eval is PINNED
     to a known-good context (never silently running in a half-swapped one);
   - [loaded] / [waiters]: which navigations (by loaderId) have fired their 'load' lifecycle
     event, so navigate can wait for THIS navigation's load with zero ambiguity and no race.
   This is what makes navigation + evaluation deterministic. *)
type t = {
  page : Cdp.t;
  clock : float Eio.Time.clock_ty Eio.Resource.t;
  ctxid : int option ref;        (* current main-frame default execution context *)
  ctx_cond : Eio.Condition.t;    (* broadcast when [ctxid] becomes available *)
  main_frame : string ref;       (* main frame id (set after getFrameTree) *)
  loaded : (string, unit) Hashtbl.t;          (* loaderIds whose 'load' fired *)
  waiters : (string, unit Eio.Promise.u) Hashtbl.t; (* loaderId -> resolver awaiting its load *)
}

(* a JS string literal for an arbitrary OCaml string (JSON strings ARE valid JS strings) *)
let lit s = J.to_string (`String s)

(* the in-page harness: console capture, a snapshot helper, and the evented wait. Installed
   via addScriptToEvaluateOnNewDocument so it is present before any app code on every doc. *)
let harness =
  {js|(function(){
  if (window.__fennecInstalled) return; window.__fennecInstalled = true;
  window.__fennec_logs = [];
  var push = function(s){ try { window.__fennec_logs.push(String(s).slice(0,200)); if (window.__fennec_logs.length>50) window.__fennec_logs.shift(); } catch(_){} };
  ['error','warn'].forEach(function(m){ var o = console[m] ? console[m].bind(console) : function(){}; console[m] = function(){ push('console.'+m+': '+Array.prototype.map.call(arguments,String).join(' ')); return o.apply(null, arguments); }; });
  window.addEventListener('error', function(e){ push('pageerror: '+((e && (e.message||e.error)) || 'error')); });
  window.addEventListener('unhandledrejection', function(e){ push('unhandledrejection: '+(e && e.reason)); });

  window.__fennecSnapshot = function(){ return { url: location.pathname+location.search, ready: document.readyState, logs: (window.__fennec_logs||[]).slice(-8) }; };

  var vis = function(el){ if (!el) return false; var s = getComputedStyle(el);
    if (s.display==='none'||s.visibility==='hidden'||s.visibility==='collapse'||parseFloat(s.opacity||'1')===0) return false;
    var r = el.getBoundingClientRect(); return r.width>0 && r.height>0; };
  var q = function(sel){ try { return document.querySelector(sel); } catch(_){ return null; } };
  var qa = function(sel){ try { return document.querySelectorAll(sel); } catch(_){ return []; } };

  var check = function(spec){ var e;
    switch(spec.kind){
      case 'visible': return vis(q(spec.sel));
      case 'hidden': e=q(spec.sel); return !e || !vis(e);
      case 'present': return !!q(spec.sel);
      case 'detached': return qa(spec.sel).length===0;
      case 'text': e=q(spec.sel); return !!e && (e.textContent||'').indexOf(spec.arg)>=0;
      case 'value': e=q(spec.sel); return !!e && e.value===spec.arg;
      case 'attr': e=q(spec.sel); return !!e && e.getAttribute(spec.arg)===spec.arg2;
      case 'count': return qa(spec.sel).length===(spec.arg|0);
      case 'url': return (location.pathname+location.search).indexOf(spec.sel)>=0;
      case 'actionable':
        e=q(spec.sel); if (!vis(e)) return false; if (e.disabled) return false;
        try { e.scrollIntoView({block:'center',inline:'center'}); } catch(_){}
        var r=e.getBoundingClientRect(), cx=r.left+r.width/2, cy=r.top+r.height/2, top=document.elementFromPoint(cx,cy);
        return !!top && (top===e || e.contains(top) || top.contains(e));
      case 'js': try { return !!eval(spec.sel); } catch(_){ return false; }
    }
    return false; };

  var oneline = function(s){ return String(s||'').replace(/\s+/g,' ').trim(); };
  var outer = function(e){ if(!e) return null; var h=oneline(e.outerHTML); return h.length>200 ? h.slice(0,199)+'…' : h; };
  var coverDesc = function(e){ if(!e) return ''; var cls=''; try { if (typeof e.className==='string' && e.className.trim()) cls='.'+e.className.trim().split(/\s+/).join('.'); } catch(_){}; return '<'+e.tagName.toLowerCase()+(e.id?('#'+e.id):'')+cls+'>'; };
  /* progressive descendant-selector probe: which prefix first fails to match anything */
  var probe = function(sel){ var parts=String(sel).trim().split(/\s+/); if (parts.length<2) return []; var acc=[], out=[];
    for (var i=0;i<parts.length;i++){ acc.push(parts[i]); var pfx=acc.join(' '); var ok=false; try{ok=!!document.querySelector(pfx);}catch(_){}; out.push([pfx,ok]); } return out; };

  var diag = function(spec){ var n=-1, e=null, reason='unknown', arg='';
    if (spec.kind!=='url' && spec.kind!=='js') { n=qa(spec.sel).length; e=q(spec.sel); }
    switch(spec.kind){
      case 'visible': case 'actionable':
        if (n===0) reason='no_match';
        else { var s=getComputedStyle(e);
          if (s.display==='none') { reason='hidden_display'; arg='none'; }
          else if (s.visibility!=='visible') { reason='hidden_visibility'; arg=s.visibility; }
          else if (parseFloat(s.opacity||'1')===0) reason='hidden_opacity';
          else { var r=e.getBoundingClientRect();
            if (r.width===0||r.height===0) reason='zero_size';
            else if (spec.kind==='actionable'){
              if (e.disabled) reason='disabled';
              else { try{e.scrollIntoView({block:'center',inline:'center'});}catch(_){}; var rr=e.getBoundingClientRect(), cx=rr.left+rr.width/2, cy=rr.top+rr.height/2, top=document.elementFromPoint(cx,cy);
                if (!top) reason='not_hit_testable'; else { reason='covered'; arg=coverDesc(top); } }
            } else reason='zero_size'; } }
        break;
      case 'hidden': reason='still_visible'; break;
      case 'present': reason='no_match'; break;
      case 'detached': reason='still_present'; arg=String(n); break;
      case 'text': if(n===0) reason='no_match'; else { reason='text_mismatch'; arg=oneline(e.textContent); } break;
      case 'value': if(n===0) reason='no_match'; else { reason='value_mismatch'; arg=(e.value===undefined||e.value===null?null:String(e.value)); } break;
      case 'attr': if(n===0) reason='no_match'; else { var av=e.getAttribute(spec.arg); if(av===null) reason='attr_absent'; else { reason='attr_mismatch'; arg=String(av); } } break;
      case 'count': reason='wrong_count'; arg=String(n); break;
      case 'url': reason='url_mismatch'; arg=location.pathname+location.search; break;
      case 'js': try { eval(spec.sel); reason='js_false'; } catch(err){ reason='js_threw'; arg=String((err&&err.message)||err); } break;
    }
    return { reason:reason, arg:arg, matched:n, outerHtml:outer(e),
             probe:(spec.kind!=='url'&&spec.kind!=='js')?probe(spec.sel):[],
             url:location.pathname+location.search, ready:document.readyState, logs:(window.__fennec_logs||[]).slice(-8) }; };

  window.__fennecWait = function(spec, timeoutMs){ return new Promise(function(resolve){
    var done=false, mo=null, raf=0, to=0;
    var cleanup=function(){ try{ if(mo)mo.disconnect(); }catch(_){}; if(raf)cancelAnimationFrame(raf); if(to)clearTimeout(to); };
    var finish=function(r){ if(done)return; done=true; cleanup(); resolve(r); };
    var tick=function(){ if(done)return; if(check(spec)) return finish({ok:true}); raf=requestAnimationFrame(tick); };
    if (check(spec)) return finish({ok:true});
    try { mo=new MutationObserver(function(){ if(!done && check(spec)) finish({ok:true}); });
          mo.observe(document.documentElement,{childList:true,subtree:true,attributes:true,characterData:true}); } catch(_){}
    raf=requestAnimationFrame(tick);
    to=setTimeout(function(){ finish({ok:false, diag:diag(spec)}); }, timeoutMs);
  }); };
})();|js}

let on_first sel body = Printf.sprintf "(function(){var e=document.querySelector(%s);return %s})()" (lit sel) body
let opt = function `String s -> Some s | _ -> None

(* wait (event-driven, bounded) until a live default context is known. Cooperative scheduling
   guarantees no lost wakeup: there is no suspension point between the [ctxid] check and the
   condition await, so the reader fiber cannot set+broadcast in between. *)
let await_context t ~timeout : int option =
  let deadline = Eio.Time.now t.clock +. timeout in
  let rec loop () =
    match !(t.ctxid) with
    | Some _ as c -> c
    | None ->
      let remaining = deadline -. Eio.Time.now t.clock in
      if remaining <= 0.0 then None
      else (
        ignore (Eio.Time.with_timeout t.clock remaining (fun () -> Eio.Condition.await_no_mutex t.ctx_cond; Ok ()));
        loop ())
  in
  loop ()

(* evaluate JS, awaiting promises, PINNED to the tracked execution context. If that context
   was replaced by a navigation mid-eval, acquire the new one and evaluate there ONCE — a
   deterministic state transition (wait for the real context-created event), not a blind retry. *)
let eval_json ?timeout t expr : J.t =
  let extract r = match Cdp.field "result" r with Some inner -> ( match Cdp.field "value" inner with Some v -> v | None -> `Null ) | None -> `Null in
  let run ctxopt =
    let ctx = match ctxopt with Some c -> [ ("contextId", `Int c) ] | None -> [] in
    Cdp.call ?timeout t.page "Runtime.evaluate"
      (`Assoc (ctx @ [ ("expression", `String expr); ("returnByValue", `Bool true); ("awaitPromise", `Bool true) ]))
  in
  let c0 = match !(t.ctxid) with Some _ as c -> c | None -> await_context t ~timeout:5.0 in
  match (try `R (extract (run c0)) with Cdp.Protocol_error m -> `E m) with
  | `R v -> v
  | `E m when Cdp.contains m "context" || Cdp.contains m "navigated" ->
    t.ctxid := None; (* force re-acquire of the post-navigation context *)
    extract (run (await_context t ~timeout:5.0))
  | `E m -> raise (Cdp.Protocol_error m)

(* ---- one-shot reads ---- *)
let current_url t = match eval_json t "location.pathname+location.search" with `String s -> s | _ -> ""
let read_text t ~selector = opt (eval_json t (on_first selector "e?e.textContent:null"))
let read_value t ~selector = opt (eval_json t (on_first selector "e?e.value:null"))
let read_attr t ~selector ~name = opt (eval_json t (on_first selector (Printf.sprintf "e?e.getAttribute(%s):null" (lit name))))
let read_count t ~selector = Cdp.as_int (Some (eval_json t (Printf.sprintf "document.querySelectorAll(%s).length" (lit selector))))
let eval t expr = match eval_json t expr with `String s -> s | `Null -> "" | v -> J.to_string v

(* ---- actions (one-shot; the DSL has already waited for the precondition; pinned eval) ---- *)
let act t expr = try ignore (eval_json t expr) with _ -> ()
let click t ~selector = act t (on_first selector "(e&&(e.scrollIntoView({block:'center'}),e.click(),true))")
let fill t ~selector ~value =
  act t (on_first selector
    (Printf.sprintf "(e&&(e.focus(),e.value=%s,e.dispatchEvent(new Event('input',{bubbles:true})),e.dispatchEvent(new Event('change',{bubbles:true})),true))" (lit value)))
let press t ~selector ~key =
  act t (on_first selector
    (Printf.sprintf "(e&&(['keydown','keypress','keyup'].forEach(function(ty){e.dispatchEvent(new KeyboardEvent(ty,{key:%s,bubbles:true}))}),true))" (lit key)))

(* ---- condition → harness spec ---- *)
let spec = function
  | Cond.Visible s -> `Assoc [ ("kind", `String "visible"); ("sel", `String s) ]
  | Cond.Hidden s -> `Assoc [ ("kind", `String "hidden"); ("sel", `String s) ]
  | Cond.Present s -> `Assoc [ ("kind", `String "present"); ("sel", `String s) ]
  | Cond.Detached s -> `Assoc [ ("kind", `String "detached"); ("sel", `String s) ]
  | Cond.Text (s, t) -> `Assoc [ ("kind", `String "text"); ("sel", `String s); ("arg", `String t) ]
  | Cond.Value (s, v) -> `Assoc [ ("kind", `String "value"); ("sel", `String s); ("arg", `String v) ]
  | Cond.Attr (s, n, v) -> `Assoc [ ("kind", `String "attr"); ("sel", `String s); ("arg", `String n); ("arg2", `String v) ]
  | Cond.Count (s, n) -> `Assoc [ ("kind", `String "count"); ("sel", `String s); ("arg", `Int n) ]
  | Cond.Url u -> `Assoc [ ("kind", `String "url"); ("sel", `String u) ]
  | Cond.Actionable s -> `Assoc [ ("kind", `String "actionable"); ("sel", `String s) ]
  | Cond.Js e -> `Assoc [ ("kind", `String "js"); ("sel", `String e) ]

let reason_of (rs : string) (arg : J.t option) : Diag.reason =
  let s = match arg with Some (`String x) -> x | _ -> "" in
  let opt_s = match arg with Some (`String x) -> Some x | _ -> None in
  let int_arg = match int_of_string_opt s with Some i -> i | None -> 0 in
  match rs with
  | "no_match" -> Diag.No_match
  | "hidden_display" -> Diag.Hidden_display s
  | "hidden_visibility" -> Diag.Hidden_visibility s
  | "hidden_opacity" -> Diag.Hidden_opacity
  | "zero_size" -> Diag.Zero_size
  | "disabled" -> Diag.Disabled
  | "covered" -> Diag.Covered s
  | "not_hit_testable" -> Diag.Not_hit_testable
  | "still_visible" -> Diag.Still_visible
  | "still_present" -> Diag.Still_present int_arg
  | "wrong_count" -> Diag.Wrong_count int_arg
  | "text_mismatch" -> Diag.Text_mismatch s
  | "value_mismatch" -> Diag.Value_mismatch opt_s
  | "attr_absent" -> Diag.Attr_absent
  | "attr_mismatch" -> Diag.Attr_mismatch s
  | "url_mismatch" -> Diag.Url_mismatch s
  | "js_false" -> Diag.Js_false
  | "js_threw" -> Diag.Js_threw s
  | other -> Diag.Unknown other

let diag_of_json ~(selector : string option) (j : J.t) : Diag.t =
  match j with
  | `Assoc _ ->
    let probe =
      match Cdp.field "probe" j with
      | Some (`List l) ->
        List.filter_map
          (function `List [ `String p; `Bool b ] -> Some (p, b) | _ -> None)
          l
      | _ -> []
    in
    Diag.make
      ~selector
      ~matched:(match Cdp.field "matched" j with Some (`Int n) -> n | _ -> -1)
      ~outer_html:(match Cdp.field "outerHtml" j with Some (`String s) -> Some s | _ -> None)
      ~probe
      ~url:(Cdp.as_string (Cdp.field "url" j))
      ~ready:(Cdp.as_string (Cdp.field "ready" j))
      ~logs:(match Cdp.field "logs" j with Some (`List l) -> List.filter_map (function `String s -> Some s | _ -> None) l | _ -> [])
      (reason_of (Cdp.as_string (Cdp.field "reason" j)) (Cdp.field "arg" j))
  | _ -> Diag.empty

(* ---- the evented wait: ONE round-trip, awaiting the in-page promise. The eval is pinned to
   the live context (eval_json), so a navigation during the wait is handled deterministically
   by re-acquiring the context — no blind retries. ---- *)
let wait t cond ~timeout : (unit, Diag.t) result =
  let expr = Printf.sprintf "__fennecWait(%s,%d)" (J.to_string (spec cond)) (int_of_float (timeout *. 1000.0)) in
  (* the CDP call gets a little longer than the in-page deadline, so the in-page setTimeout
     (which produces the diagnostic) is what bounds the wait, not a blunt CDP timeout *)
  match try `R (eval_json ~timeout:(timeout +. 5.0) t expr) with Cdp.Protocol_error m -> `E m with
  | `R r -> (
    match Cdp.field "ok" r with
    | Some (`Bool true) -> Ok ()
    | _ -> Error (diag_of_json ~selector:(Cond.selector cond) (match Cdp.field "diag" r with Some d -> d | None -> `Null)))
  | `E m -> Error (Diag.make (Diag.Backend_error m))

(* ---- navigation: trigger, then wait for the 'load' lifecycle event whose loaderId MATCHES
   this navigation. That makes "is this navigation done?" unambiguous — stray about:blank
   loads, reloads, and interleaved events all carry different loaderIds. Race-free: if the
   load already fired it is recorded in [loaded]; otherwise we register a waiter that the
   reader's handler resolves. No suspension point between the check and the registration, so
   the event cannot be missed. ---- *)
let navigate t ~url ~timeout : (unit, Diag.t) result =
  match try `R (Cdp.call ~timeout t.page "Page.navigate" (`Assoc [ ("url", `String url) ])) with Cdp.Protocol_error m -> `E m with
  | `E m -> Error (Diag.make (Diag.Backend_error (Printf.sprintf "Page.navigate(%s): %s" url m)))
  | `R res -> (
    match Cdp.field "errorText" res with
    | Some (`String e) when e <> "" -> Error (Diag.make (Diag.Nav_error e))
    | _ ->
      let loader = Cdp.as_string (Cdp.field "loaderId" res) in
      if loader = "" then Error (Diag.make (Diag.Nav_error "navigation returned no loaderId"))
      else if Hashtbl.mem t.loaded loader then Ok ()
      else begin
        let p, u = Eio.Promise.create () in
        Hashtbl.replace t.waiters loader u;
        match Eio.Time.with_timeout t.clock timeout (fun () -> Eio.Promise.await p; Ok ()) with
        | Ok () -> Ok ()
        | Error `Timeout -> Hashtbl.remove t.waiters loader; Error (Diag.make Diag.Nav_timeout)
      end)

(* ---- isolated-page lifecycle ----
   Each test gets its OWN control connection + its OWN incognito BrowserContext + page, all
   tied to the test's switch — no cross-test shared state. We register the navigation/context
   tracking handlers BEFORE enabling the domains (so no initial event is missed), then enable
   Runtime + Page + lifecycle events and learn the main frame id. ---- *)
let create_isolated ~sw ~(browser : Cdp.t) ~(chrome : Chrome.t) : t =
  (* [browser] is a long-lived control connection shared by all tests — its reader fiber
     stays alive, so the context teardown below gets its reply promptly (a per-test control
     connection would already be cancelled at teardown, hanging the dispose). The async
     dispatcher correlates replies by id, so concurrent create/dispose need no mutex. *)
  let ctx = Cdp.call browser "Target.createBrowserContext" (`Assoc [ ("disposeOnDetach", `Bool false) ]) in
  let ctx_id = Cdp.as_string (Cdp.field "browserContextId" ctx) in
  let tgt =
    Cdp.call browser "Target.createTarget" (`Assoc [ ("url", `String "about:blank"); ("browserContextId", `String ctx_id) ])
  in
  let tid = Cdp.as_string (Cdp.field "targetId" tgt) in
  let page = Chrome.connect_page ~sw chrome tid in
  let t =
    { page; clock = Chrome.clock chrome; ctxid = ref None; ctx_cond = Eio.Condition.create ();
      main_frame = ref ""; loaded = Hashtbl.create 8; waiters = Hashtbl.create 8 }
  in
  (* track the main-frame default execution context *)
  Cdp.on page "Runtime.executionContextCreated" (fun p ->
      match Cdp.field "context" p with
      | Some c ->
        let aux = Cdp.field "auxData" c in
        let is_default = Cdp.as_bool (match aux with Some a -> Cdp.field "isDefault" a | None -> None) in
        let fid = Cdp.as_string (match aux with Some a -> Cdp.field "frameId" a | None -> None) in
        if is_default && (!(t.main_frame) = "" || fid = !(t.main_frame)) then begin
          t.ctxid := Some (Cdp.as_int (Cdp.field "id" c));
          Eio.Condition.broadcast t.ctx_cond
        end
      | None -> ());
  Cdp.on page "Runtime.executionContextDestroyed" (fun p ->
      if Some (Cdp.as_int (Cdp.field "executionContextId" p)) = !(t.ctxid) then t.ctxid := None);
  Cdp.on page "Runtime.executionContextsCleared" (fun _ -> t.ctxid := None);
  (* record 'load' lifecycle events by loaderId and wake any navigate awaiting that loader *)
  Cdp.on page "Page.lifecycleEvent" (fun p ->
      if Cdp.field "name" p = Some (`String "load") then begin
        let l = Cdp.as_string (Cdp.field "loaderId" p) in
        Hashtbl.replace t.loaded l ();
        match Hashtbl.find_opt t.waiters l with
        | Some u -> Hashtbl.remove t.waiters l; Eio.Promise.resolve u ()
        | None -> ()
      end);
  (* enable AFTER handlers are registered, so the initial context/lifecycle events are caught *)
  ignore (Cdp.call page "Runtime.enable" (`Assoc []));
  ignore (Cdp.call page "Page.enable" (`Assoc []));
  ignore (Cdp.call page "Page.setLifecycleEventsEnabled" (`Assoc [ ("enabled", `Bool true) ]));
  ignore (Cdp.call page "Page.addScriptToEvaluateOnNewDocument" (`Assoc [ ("source", `String harness) ]));
  (let ft = Cdp.call page "Page.getFrameTree" (`Assoc []) in
   t.main_frame :=
     ( match Cdp.field "frameTree" ft with
       | Some tree -> ( match Cdp.field "frame" tree with Some f -> Cdp.as_string (Cdp.field "id" f) | None -> "" )
       | None -> "" ));
  Eio.Switch.on_release sw (fun () ->
      try ignore (Cdp.call browser "Target.disposeBrowserContext" (`Assoc [ ("browserContextId", `String ctx_id) ])) with _ -> ());
  t
