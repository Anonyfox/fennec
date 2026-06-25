(** RPC method declaration — the isomorphic seam behind [web/methods/]. {!method_} fuses a method's wire
    contract + optimistic stub + server handler in one place: decl + stub are isomorphic (client bundles
    AND server), the [~server] handler is server-only (registered through the {!install}ed seam, stripped
    from the client build by the fur ppx). A client calls the result with [Pulse.call]. *)

(** The context a server handler receives — an isomorphic mirror of the server's invocation. *)
type invocation = {
  user_id : string option;
  remote_ip : string option;
  is_simulation : bool;
  set_user_id : string option -> unit;
}

(** The registrar the server installs (its real one bridges to [Reactive.handle]); a first-class
    polymorphic field so one install serves every method's types. *)
type registrar = { reg : 'a 'r. ('a, 'r) Method.t -> (invocation -> 'a -> 'r) -> unit }

(** [install r] makes [r] the live registrar (server boot). *)
val install : registrar -> unit

(** [method_ name ~args ~result ?stub ?server ()] declares a method and, when [~server] is given,
    registers its handler. Returns the [Method.t] — invoke it client-side with [Pulse.call]. *)
val method_ :
  string ->
  args:'a Sift.args ->
  result:'r Sift.t ->
  ?stub:(Method.sim_writes -> 'a -> unit) ->
  ?server:(invocation -> 'a -> 'r) ->
  unit ->
  ('a, 'r) Method.t
