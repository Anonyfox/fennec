(** A HANDLER — a standalone, full HTTP handler authored as ONE .mlx
    ([web/handlers/<name>.mlx]): a server [load] ([conn -> outcome]) fused with an isomorphic
    [view] ([payload -> vnode]). On [render payload] the framework seeds exactly that — a {!Sift}-typed
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

(** What [load] decides — a FULL HTTP response. The same [view]/[payload] can be served as a hydrated
    SPA ([render]), as static no-JS HTML ([static]), or content-negotiated to JSON ([json]). *)
type 'p outcome =
  | Render of 'p  (** the rich default: hydrated SPA — seed + SSR view + bundle *)
  | Html of 'p  (** the same view as plain static HTML — no seed, no bundle, no JS *)
  | Json of string  (** the payload as a plain JSON body *)
  | Text of string  (** a plain-text body *)
  | Redirect of string
  | Not_found
  | Error of int

(** Smart constructors, so [load] reads as plain verbs: [render p] (SPA) / [html p] (plain HTML) /
    [json codec v] (data) / … — one [view]/[payload] negotiated into the representation asked for. *)

val render : 'p -> 'p outcome
val html : 'p -> 'p outcome
val json : 'a Sift.t -> 'a -> 'p outcome
val text : string -> 'p outcome
val redirect : string -> 'p outcome
val not_found : 'p outcome
val error : int -> 'p outcome

(** The protected-handler combinator: [guard conn ~user ~login f] runs [f uid] when [user conn] is
    [Some uid], else [redirect login]. The Conn type is abstract here (this lib has no paw/accounts
    dependency), so the caller passes the id extractor — inside a handler's [load] that is
    [Accounts.user_id]: [guard conn ~user:Accounts.user_id ~login:"/login" (fun uid -> render …)].
    It is the Mode-B mirror of a component's anonymous-frame gate — a server-side bounce before any
    personalized [render]. *)
val guard : 'conn -> user:('conn -> string option) -> login:string -> (string -> 'p outcome) -> 'p outcome

(** Server-only values: NO {!Sift}, so a secret held in [load] cannot be seeded — putting one in a
    payload is a COMPILE error (Eliom's no-identity-converter, by type). *)
module Server_only : sig
  type 'a t

  val wrap : 'a -> 'a t
  val get : 'a t -> 'a
end

(** Client read of the cross-stage payload (decode the seed with the same [codec]). No fallback: on a
    rendered handler the seed is always present, so a missing/garbled seed raises (corrupt page). *)
val payload : 'a Sift.t -> key:string -> 'a

(** The default handler document shell (head + styles + #app hydration root + seed + bundle script). *)
val default_template : string -> Fur.Doc.ctx -> Fur.vnode

(** PURE render: seed ONLY [codec.enc value], SSR [view value], wrap in the shell -> the HTML string. *)
val render_doc :
  key:string ->
  codec:'p Sift.t ->
  bundle:string ->
  ?styles:string ->
  ?template:(Fur.Doc.ctx -> Fur.vnode) ->
  'p ->
  ('p -> Fur.vnode) ->
  string

(** STATIC render: SSR a vnode into a plain document (head + styles + body) with NO #app root, NO seed,
    NO bundle — final HTML with no JS. Backs the [Static] outcome. *)
val render_static : ?styles:string -> Fur.vnode -> string
