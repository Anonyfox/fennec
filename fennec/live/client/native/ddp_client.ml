(* Native / SSR stub of the DDP client: there is no WebSocket on the server, so [connect] yields an
   empty live store and [find] renders [] (the server emits an empty/placeholder list). The browser
   implementation connects for real and the live data fills in after hydration. *)

module Live = Fennec_live.Live

type t = { live : Live.t }

let connect ?path () =
  ignore path;
  { live = Live.create () }

let subscribe _ ~name ?params () = ignore (name, params)
let call _ ~name ?params () = ignore (name, params)
let find t = Live.find t.live
