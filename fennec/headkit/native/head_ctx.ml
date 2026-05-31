(* Native (SSR) head context: the sink is a per-render tag accumulator carried in
   React context. NO global state — a fresh sink is created per render and
   provided via the context, so concurrent SSR renders never interfere. *)

module Head = Fennec_head.Head

type sink = Head.tag list ref

let context : sink React.Context.t = React.createContext (ref [])

(* <Head> calls this during its render body (document order) *)
let push (sink : sink) (tags : Head.tag list) : unit = sink := !sink @ tags
