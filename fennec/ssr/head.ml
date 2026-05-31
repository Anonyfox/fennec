(* Native (SSR) runtime for <Head>. A page mounts <Head> components anywhere in
   its tree; each registers its tags into a scoped collector, and after render the
   merged tags feed the document template's <head>.

   REGISTRATION HAPPENS AT RENDER TIME, NOT CONSTRUCTION TIME. [make] is a real
   [@react.component]: server-reason-react invokes its body during the render
   walk, which is reliably parent → child (depth-first). OCaml's argument/list
   evaluation order is unspecified (often right-to-left), so registering eagerly
   when the element is *constructed* would scramble tree order — making "last wins"
   no longer mean "innermost wins". Render-time registration fixes this: children
   register after parents, so the deduping merge's last-write-wins == inside-out.

   The collector is SCOPED (saved/restored around one render), so it is correct
   per-request even under Eio concurrency: the whole render runs synchronously to
   completion in one fiber before the binding is restored. *)

module Head = Fennec_head.Head

let current : Head.tag list ref option ref = ref None

let register (tags : Head.tag list) : unit =
  match !current with Some c -> c := !c @ tags | None -> ()

(* internal component: registers during RENDER (parent→child order), renders
   nothing *)
let[@react.component] component ~(tags : Head.tag list) =
  register tags;
  React.null

(* The PUBLIC <Head> — a PLAIN labelled function (identical signature to the CSR
   runtime's [make]) so the one shared component compiles to both targets. Builds
   tags from typed props + [extra], and instantiates the render-time component. *)
let make ?title ?description ?canonical ?(extra = []) () : React.element =
  let tags = Head.of_props ?title ?description ?canonical ~extra () in
  component ~tags

(* Build [page] and render it, collecting the <Head> tags registered during the
   render walk. Returns (body_html, merged_tags) with inside-out precedence. *)
let render_collect (page : unit -> React.element) : string * Head.tag list =
  let c = ref [] in
  let saved = !current in
  current := Some c;
  let html =
    Fun.protect
      ~finally:(fun () -> current := saved)
      (fun () -> ReactDOM.renderToString (page ()))
  in
  (html, Head.merge !c)
