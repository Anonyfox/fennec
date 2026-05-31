(* Native SSR head collection. [render_collect] wraps a page element in a fresh
   sink-providing context, renders it (the <Head>s register during render, in
   document order — verified), and returns the rendered body HTML + the merged
   tags. No global, no eval-order hack: the sink lives in the context value,
   created per call. *)

module Head = Fennec_head.Head

let render_collect (element : React.element) : string * Head.tag list =
  let sink : Head_ctx.sink = ref [] in
  let tree = Head_ctx.context.React.Context.provider ~value:sink ~children:element () in
  let html = ReactDOM.renderToString tree in
  (html, Head.merge !sink)
