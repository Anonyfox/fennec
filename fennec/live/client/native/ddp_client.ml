(* Native / SSR stub of the DDP client: there is no WebSocket on the server, so [connect] yields an
   empty live store and [find] renders [] (the server emits an empty/placeholder list). A
   subscription never becomes ready here — SSR renders the loading state, and the browser
   implementation connects for real, hydrates, and confirms readiness. *)

module Live = Fennec_live.Live

type t = { live : Live.t }
type subscription = { ready : bool Fur.signal; stop : unit -> unit }

let connect ?path () =
  ignore path;
  { live = Live.create () }

let subscribe _ ~name ?(params = []) () =
  ignore (name, params);
  { ready = Fur.signal false; stop = (fun () -> ()) }

let use_subscribe _ ~name ?(params = []) () =
  ignore (name, params);
  Fur.signal false

let call _ ~name ?(params = []) () = ignore (name, params)
let find t = Live.find t.live
