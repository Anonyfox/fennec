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
}

let _data_key : _data_ctx Eio.Fiber.key = Eio.Fiber.create_key ()
let _data_fallback = { seed = Hashtbl.create 16; source = (fun _ _ -> ()) }

let _data_current () =
  (* outside an Eio run (one-shot SSR / tests) Fiber.get's effect is unhandled — catch ONLY that and
     fall back to the global; any other exception is a real bug and must propagate, not be hidden *)
  match (try Eio.Fiber.get _data_key with Stdlib.Effect.Unhandled _ -> None) with
  | Some c -> c
  | None -> _data_fallback

let with_data_context f =
  Eio.Fiber.with_binding _data_key { seed = Hashtbl.create 16; source = (fun _ _ -> ()) } f

let seed_table () = (_data_current ()).seed
let data_source () = (_data_current ()).source
let set_data_source s = (_data_current ()).source <- s
