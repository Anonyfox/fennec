(* The shared <Head> component — ONE source compiled for both targets (copied into
   the native and melange libs, like app components). It reads a per-render "sink"
   from React context and registers its tags into it during its render body. Since
   server-reason-react and reason-react both run a component body in document order
   (parent → child, verified), registration order == document order, so the pure
   Fennec_head.Head.merge's last-wins == innermost/deepest wins (react-helmet
   convention) — identically on SSR and CSR.

   The per-target seam is [Head_ctx]: it owns the React context (whose value is the
   mutable sink) and [push]. Native binds it to a collect-sink; melange to a
   document-applying store. The component code below is target-independent. *)

module Head = Fennec_head.Head

(* register tags via the platform context sink, render nothing *)
let[@react.component] make ?title ?description ?canonical ?(extra = []) () =
  let tags = Head.of_props ?title ?description ?canonical ~extra () in
  let sink = React.useContext Head_ctx.context in
  Head_ctx.push sink tags;
  React.null
