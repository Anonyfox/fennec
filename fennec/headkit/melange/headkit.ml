(* Melange (CSR) head runtime. A [Provider] wraps the app: it creates a fresh sink
   per render, provides it via context (so mounted <Head>s push their tags during
   render, document order), and a useLayoutEffect applies the merged head to the
   live document AFTER children render — setting document.title and replacing the
   managed <meta>/<link> nodes (marked data-fennec-head, so user/static head stays
   untouched). Same merge as SSR => client head matches server head, no flash.
   Works on any React/Preact (touches the DOM + useContext/useLayoutEffect, never
   renderer internals). *)

module Head = Fennec_head.Head

let set_title : string -> unit = [%mel.raw {| function (t) { document.title = t; } |}]

let apply_head_html : string -> unit =
  [%mel.raw
    {| function (html) {
         var head = document.head;
         head.querySelectorAll('[data-fennec-head]').forEach(function (n) {
           n.parentNode.removeChild(n);
         });
         if (!html) return;
         var tmp = document.createElement('head');
         tmp.innerHTML = html;
         Array.prototype.slice.call(tmp.childNodes).forEach(function (n) {
           if (n.nodeType === 1 && n.tagName !== 'TITLE') {
             n.setAttribute('data-fennec-head', '');
             head.appendChild(n);
           }
         });
       } |}]

let apply (tags : Head.tag list) : unit =
  let merged = Head.merge tags in
  (match Head.title_of merged with Some t -> set_title t | None -> ());
  apply_head_html (Head.to_html merged)

(* Wrap the app once. A fresh sink each render (so stale tags can't accumulate);
   children push into it during render; the layout-effect applies the merged head
   after commit and reverts managed nodes on cleanup. *)
let[@react.component] provider ~(children : React.element) =
  let sink = { Head_ctx.tags = [] } in
  React.useLayoutEffect (fun () ->
      apply sink.Head_ctx.tags;
      Some (fun () -> apply_head_html ""));
  React.createElement
    (React.Context.provider Head_ctx.context)
    (React.Context.makeProps ~value:sink ~children ())
