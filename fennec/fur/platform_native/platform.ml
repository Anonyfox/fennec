(* Native (SSR) platform: everything inert. Handlers/browser code never run during
   SSR, so these are unreachable in practice; they exist so the core links natively. *)
let event_value () = ""
let event_checked () = false
let event_key () = ""
let event_prevent_default () = ()
let local_get _ = None
let local_set _ _ = ()
let local_remove _ = ()
let push_state _ = ()

(* Per-request data context, FIBER-LOCAL on the concurrent server so simultaneous SSR requests never
   share a seed table. Outside an Eio run (one-shot SSR / tests), [Fiber.get] has no handler — we
   catch that and fall back to a process-global context (single-threaded there, so safe). *)
type _data_ctx = {
  seed : (string, string) Hashtbl.t;
  mutable source : string -> (string -> unit) -> unit;
  mutable head : Obj.t option;  (* opaque per-request Head context — Fur.Head fills it lazily (the
                                   signal type lives a layer up, so the slot is type-erased here) *)
}

let _data_key : _data_ctx Eio.Fiber.key = Eio.Fiber.create_key ()
let _data_fallback = { seed = Hashtbl.create 16; source = (fun _ _ -> ()); head = None }

let _data_current () =
  (* outside an Eio run (one-shot SSR / tests) Fiber.get's effect is unhandled — catch ONLY that and
     fall back to the global; any other exception is a real bug and must propagate, not be hidden *)
  match (try Eio.Fiber.get _data_key with Stdlib.Effect.Unhandled _ -> None) with
  | Some c -> c
  | None -> _data_fallback

(* Establish a fresh per-request render context (seed table + fetch source + Head registry). In an Eio
   run it is a FIBER-LOCAL binding, so simultaneous renders across fibers AND domains never share state.
   Outside an Eio run (one-shot SSR / unit tests) there is no fiber to bind — reset and reuse the global
   fallback (single-threaded there, so safe), so repeated one-shot renders still start clean. *)
let with_data_context f =
  let in_eio = try ignore (Eio.Fiber.get _data_key); true with Stdlib.Effect.Unhandled _ -> false in
  if in_eio then Eio.Fiber.with_binding _data_key { seed = Hashtbl.create 16; source = (fun _ _ -> ()); head = None } f
  else begin
    Hashtbl.reset _data_fallback.seed;
    _data_fallback.source <- (fun _ _ -> ());
    _data_fallback.head <- None;
    f ()
  end

let seed_table () = (_data_current ()).seed
let data_source () = (_data_current ()).source
let set_data_source s = (_data_current ()).source <- s
let head_get () = (_data_current ()).head
let head_set o = (_data_current ()).head <- Some o
