(* Plug-style middleware: a [conn] flows through the pipeline; any middleware may
   set a response, which HALTS the pipeline (short-circuits everything after it,
   including route dispatch). Model lifted from Elixir's Plug. *)

type conn = { req : Http.request; mutable resp : Http.response option }
type t = conn -> unit

let halt conn resp = conn.resp <- Some resp
let halted conn = conn.resp <> None
