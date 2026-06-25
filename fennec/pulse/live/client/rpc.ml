(* RPC method declaration — the isomorphic seam behind web/methods/. A method file declares its wire
   contract (args/result) + an optional optimistic client stub + a server handler in ONE place; this
   combinator fuses them. The decl + stub are isomorphic (linked into client bundles AND the server); the
   ~server handler runs server-side only — registered through a settable seam the server installs at boot
   (exactly like Coll_writer), and stripped from the client build by the fur ppx so no server logic ships.
   A client invokes the method with [Pulse.call]. *)

(* the context a handler receives — an isomorphic mirror of the server's invocation. The client never
   constructs one; the type only needs to resolve so a ~server handler type-checks on the server build. *)
type invocation = {
  user_id : string option;
  remote_ip : string option;
  is_simulation : bool;
  set_user_id : string option -> unit;
}

(* the install seam: the server provides the real registrar (R.handle, bridged); the client leaves it
   None (a no-op). A first-class-polymorphic field so one install covers every method's types. *)
type registrar = { reg : 'a 'r. ('a, 'r) Method.t -> (invocation -> 'a -> 'r) -> unit }

let _registrar : registrar option ref = ref None

(* a method file's module-init can run BEFORE the server installs the registrar (it doesn't depend on
   Fennec_pulse_app), so a registration that arrives early is buffered and flushed on install. The client
   never installs (and strips ~server anyway), so its buffer just stays put — harmless. *)
let _pending : (registrar -> unit) list ref = ref []

let _register m h =
  match !_registrar with Some r -> r.reg m h | None -> _pending := (fun r -> r.reg m h) :: !_pending

let install (r : registrar) =
  _registrar := Some r;
  List.iter (fun f -> f r) (List.rev !_pending);
  _pending := []

(* the combinator: define the method (decl + stub, isomorphic) and — when a ~server handler is given (the
   server build; the client build has it stripped) — register it. Returns the [Method.t]; call it with
   [Pulse.call]. *)
let method_ name ~args ~result ?stub ?server () =
  let m = Method.define ?stub name ~args ~result in
  (match server with Some h -> _register m h | None -> ());
  m
