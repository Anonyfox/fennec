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
