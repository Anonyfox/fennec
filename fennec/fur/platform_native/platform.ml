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

(* Per-request render context, FIBER-LOCAL on the concurrent server so simultaneous SSR renders never
   share state. [seed]/[source] are the Data context; [locals] is a generic map for the Fur.core
   per-request singletons (the Head tag registry, the active Router) whose types live a layer up, so
   they are stored type-erased ([Obj.t], cast back by their owner). Outside an Eio run (one-shot SSR /
   tests) [Fiber.get] has no handler — caught + degraded to a reset process-global (single-threaded). *)
type _data_ctx = {
  seed : (string, string) Hashtbl.t;
  mutable source : string -> (string -> unit) -> unit;
  locals : (string, Obj.t) Hashtbl.t;
}

let _fresh () = { seed = Hashtbl.create 16; source = (fun _ _ -> ()); locals = Hashtbl.create 4 }
let _data_key : _data_ctx Eio.Fiber.key = Eio.Fiber.create_key ()
let _data_fallback = _fresh ()

let _data_current () =
  (* outside an Eio run (one-shot SSR / tests) Fiber.get's effect is unhandled — catch ONLY that and
     fall back to the global; any other exception is a real bug and must propagate, not be hidden *)
  match (try Eio.Fiber.get _data_key with Stdlib.Effect.Unhandled _ -> None) with
  | Some c -> c
  | None -> _data_fallback

(* Establish a fresh per-request context. In an Eio run -> a FIBER-LOCAL binding, so simultaneous renders
   across fibers AND domains are isolated. Outside one (one-shot SSR / unit tests) there is no fiber to
   bind -> reset and reuse the global fallback (single-threaded there, so safe), so repeated one-shot
   renders still start clean. *)
let with_data_context f =
  let in_eio = try ignore (Eio.Fiber.get _data_key); true with Stdlib.Effect.Unhandled _ -> false in
  if in_eio then Eio.Fiber.with_binding _data_key (_fresh ()) f
  else begin
    Hashtbl.reset _data_fallback.seed;
    _data_fallback.source <- (fun _ _ -> ());
    Hashtbl.reset _data_fallback.locals;
    f ()
  end

let seed_table () = (_data_current ()).seed
let data_source () = (_data_current ()).source
let set_data_source s = (_data_current ()).source <- s
let slot_get key = Hashtbl.find_opt (_data_current ()).locals key
let slot_set key o = Hashtbl.replace (_data_current ()).locals key o
