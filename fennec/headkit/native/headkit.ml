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

(* Render the merged head tags as React elements so a template can splice them
   straight into its <head> (composing elements, not injecting a raw string).
   Every tag kind maps through React.createElement with a dynamic prop list, so
   even the arbitrary-attr Link/Meta escape hatches render correctly. The caller
   should pass already-merged tags (render_collect returns them merged). *)
let head_element (tags : Head.tag list) : React.element =
  let prop k v = React.JSX.string k k v in
  let el i (t : Head.tag) : React.element =
    let key = string_of_int i in
    match t with
    | Head.Title s -> React.createElementWithKey ~key "title" [] [ React.string s ]
    | Head.Charset c -> React.createElementWithKey ~key "meta" [ prop "charset" c ] []
    | Head.Meta_name (n, c) ->
      React.createElementWithKey ~key "meta" [ prop "name" n; prop "content" c ] []
    | Head.Meta_property (p, c) ->
      React.createElementWithKey ~key "meta" [ prop "property" p; prop "content" c ] []
    | Head.Canonical href ->
      React.createElementWithKey ~key "link" [ prop "rel" "canonical"; prop "href" href ] []
    | Head.Link attrs ->
      React.createElementWithKey ~key "link" (List.map (fun (k, v) -> prop k v) attrs) []
    | Head.Meta attrs ->
      React.createElementWithKey ~key "meta" (List.map (fun (k, v) -> prop k v) attrs) []
  in
  React.array (Array.of_list (List.mapi el (Head.merge tags)))
