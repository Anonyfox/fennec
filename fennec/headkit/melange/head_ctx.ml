(* Melange (CSR) head context. The sink carried in context is a store that, on
   each render, accumulates the tags the mounted <Head>s push (in document order),
   then a Provider effect applies the merged head to the live document. Same
   Fennec_head.Head.merge as SSR, so the client head matches the server head. *)

module Head = Fennec_head.Head

type sink = { mutable tags : Head.tag list }

let context : sink React.Context.t = React.createContext { tags = [] }

(* <Head> pushes during render (document order) *)
let push (sink : sink) (tags : Head.tag list) : unit = sink.tags <- sink.tags @ tags
