(* Native (SSR) runtime for <Head>. A page mounts <Head> components anywhere in
   its tree; each registers its tags into a scoped collector, and after render the
   merged tags feed the document template's <head>.

   REGISTRATION ORDER, and why we reverse it: on native, server-reason-react runs
   a component body EAGERLY when the element is constructed (not at a later render
   pass — verified). OCaml constructs a createElement child list RIGHT-TO-LEFT, so
   a deeper/later-in-document <Head> registers BEFORE a shallower/earlier one. A
   right-to-left depth-first construction is the exact reverse of document order,
   so we REVERSE the collected list to recover document order. Then the pure merge
   (last-write-wins per key == last-in-document == innermost/deepest wins, the
   react-helmet convention) matches the CSR runtime, which already registers in
   document order (its hook effects run parent→child). Same merge, same result on
   both sides — no hydration flash. The nested-precedence test pins this.

   The collector is SCOPED (saved/restored around one render): correct per-request
   even under Eio concurrency, since the render runs synchronously to completion in
   one fiber before the binding is restored. *)

module Head = Fennec_head.Head

let current : Head.tag list ref option ref = ref None

let register (tags : Head.tag list) : unit =
  match !current with Some c -> c := !c @ tags | None -> ()

(* The <Head> component — a plain labelled function (same signature as the CSR
   runtime's [make], so the one shared component compiles to both). Registers its
   tags at construction and renders nothing. *)
let make ?title ?description ?canonical ?(extra = []) () : React.element =
  register (Head.of_props ?title ?description ?canonical ~extra ());
  React.null

(* Build [page] and render it, collecting the <Head> tags. The collected list is
   in reverse-document order (right-to-left construction), so reverse it to
   document order before merging. Returns (body_html, merged_tags) with inside-out
   precedence. *)
let render_collect (page : unit -> React.element) : string * Head.tag list =
  let c = ref [] in
  let saved = !current in
  current := Some c;
  let html =
    Fun.protect
      ~finally:(fun () -> current := saved)
      (fun () -> ReactDOM.renderToString (page ()))
  in
  (html, Head.merge (List.rev !c))
