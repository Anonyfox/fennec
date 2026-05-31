(* Native (SSR) runtime for <Head>. A page mounts <Head> components anywhere in
   its tree; each one registers its tags into a scoped collector. After the page
   is built and rendered, the merged tags feed the document template's <head>.

   TIMING (verified by spike): a <Head> registers when its [make] runs, which —
   depending on whether it sits at the top of a component or inside a nested
   [@react.component] body — can be either at element-CONSTRUCTION time or at
   RENDER time. So [render_collect] keeps the collector active across BOTH phases:
   it takes a THUNK, builds the element (top-level registrations fire here) and
   renders it (nested registrations fire here) all within one collector binding.

   [make] is a PLAIN eager function (NOT [@react.component]): it registers and
   returns React.null (rendering nothing itself).

   The collector is SCOPED (saved/restored), so it is correct per-request even
   under Eio concurrency: the whole build+render runs synchronously to completion
   in one fiber before the binding is restored — no interleaving. *)

module Head = Fennec_head.Head

let current : Head.tag list ref option ref = ref None

let register (tags : Head.tag list) : unit =
  match !current with Some c -> c := List.rev_append (List.rev tags) !c | None -> ()

(* The <Head> component. Typed common props + an [extra] escape hatch for
   arbitrary meta/link tags. Registers eagerly, renders nothing. *)
let make ?title ?description ?canonical ?(extra = []) () : React.element =
  register (Head.of_props ?title ?description ?canonical ~extra ());
  React.null

(* Build [page] and render it to HTML while collecting the <Head> tags it
   registers (across both construction and render). Returns (body_html,
   merged_tags). *)
let render_collect (page : unit -> React.element) : string * Head.tag list =
  let c = ref [] in
  let saved = !current in
  current := Some c;
  let html =
    Fun.protect
      ~finally:(fun () -> current := saved)
      (fun () ->
        let el = page () in
        ReactDOM.renderToString el)
  in
  (html, Head.merge (List.rev !c))
