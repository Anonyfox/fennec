(** A HANDLER — a standalone, full HTTP handler authored as ONE .mlx
    ([frontend/handlers/<name>.mlx]): a server [load] ([conn -> outcome]) fused with an isomorphic
    [view] ([payload -> vnode]). On [render payload] the framework seeds exactly that — a {!Codec}-typed
    value — SSRs the view, and ships the handler's OWN jsoo bundle so it hydrates into a tiny SPA.
    [load] may also [redirect]/[error]/[not_found] — a handler is a full HTTP handler, not just a page.

    This module is the ISOMORPHIC runtime (jsoo-safe) shared by the server SSR and the client bundle.
    The fur ppx ([-handler]) turns a handler file's [payload]/[load]/[view] into a server [serve]
    (via {!render_doc}) and a client [boot] (via {!payload} + {!Fur_csr.start_page}); the only
    Conn-aware code lives inside the generated server [serve]. Userland never calls this module by
    name — it writes [payload]/[load]/[view] and the smart verbs ([render]/[redirect]/…).

    The cross-stage-persistence model and the leak-proof-by-construction safety are documented in the
    implementation header. In short: only [codec.enc payload] crosses; {!Server_only} makes secrets
    un-encodable so a leak is a compile error; behaviour + live data cross as Pulse/Fur handles. *)

(** What [load] decides — render the handler (SPA), or any other HTTP response. *)
type 'p outcome = Render of 'p | Redirect of string | Not_found | Error of int

(** Smart constructors, so [load] reads as plain verbs. *)

val render : 'p -> 'p outcome
val redirect : string -> 'p outcome
val not_found : 'p outcome
val error : int -> 'p outcome

(** Server-only values: NO {!Codec}, so a secret held in [load] cannot be seeded — putting one in a
    payload is a COMPILE error (Eliom's no-identity-converter, by type). *)
module Server_only : sig
  type 'a t

  val wrap : 'a -> 'a t
  val get : 'a t -> 'a
end

(** Client read of the cross-stage payload (decode the seed with the same [codec]). No fallback: on a
    rendered handler the seed is always present, so a missing/garbled seed raises (corrupt page). *)
val payload : 'a Codec.t -> key:string -> 'a

(** The seeded payload as a reactive resource (signal form, with a fallback) — for views that want it;
    most handler views just take a plain payload. *)
val resource : 'a Codec.t -> key:string -> fallback:'a -> 'a Fur.Data.t

(** The default handler document shell (head + styles + #app hydration root + seed + bundle script). *)
val default_template : string -> Fur.Doc.ctx -> Fur.vnode

(** PURE render: seed ONLY [codec.enc value], SSR [view value], wrap in the shell -> the HTML string. *)
val render_doc :
  key:string ->
  codec:'p Codec.t ->
  bundle:string ->
  ?styles:string ->
  ?template:(Fur.Doc.ctx -> Fur.vnode) ->
  'p ->
  ('p -> Fur.vnode) ->
  string
