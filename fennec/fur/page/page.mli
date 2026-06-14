(** A standalone PAGE — an app with no sub-router. An isomorphic [view] (reads its payload from the
    seed, so it SSRs then hydrates) fused with a server [data] handler (the conn block) that runs with
    the Conn (+ Pulse + Accounts), decides the response, and on [Render payload] hands exactly that —
    a {!Codec}-typed value — to the view and SEEDS it for the client.

    This module is the ISOMORPHIC essence (jsoo-safe: Fur + Codec + Bson_json), so a page's view links
    into the server SSR AND the page's own bundle from one place. The one Conn-aware bit ([serve] + the
    [t] record) is added by the app facade {!Fennec.Fur.Page}; the client boot is {!Fur_csr.start_page}.

    {2 Authoring: one .mlx, the [%%conn] block}

    A page is ONE file: [let key]/[codec]/[bundle] + a [page] view that reads the seed, then a server
    block [\[%%conn fun conn -> outcome\]]. The fur ppx compiles the view into both builds; it expands
    [%%conn] to [serve] for the server and STRIPS it for the client (driver flag [-conn-client]), so
    the jsoo bundle never sees Conn — Eliom's server/client split, one flag, no second file.

    {2 The model: cross-stage persistence (the named science)}

    A page is a TWO-STAGE computation: stage 1 = the server conn block; stage 2 = the client view /
    hydration. The values that travel stage 1 → stage 2 are exactly what multi-stage programming
    (MetaML/MetaOCaml — Taha, Kiselyov; "MetaOCaml server pages: web publishing as staged
    computation") calls the CROSS-STAGE-PERSISTENT values: the serializable values the later stage
    actually consumes. Primitives/records lift by value; closures, refs, and server-only handles
    cannot lift and must cross as a HANDLE, not a serialization. This is the boundary Ocsigen/Eliom
    (Radanne, Vouillon, Balat) draws with its [~%x] injections + typed converters, and the
    leak-proofing Ur/Web gets from abstract server-only types. We are deliberately NOT Phoenix
    LiveView (server holds page state, ships render diffs): that trades a permanent latency + state
    tax for minimal wire bytes — fennec has autonomous Meteor-style clients (minimongo,
    offline-for-free), so the resumable/CSP pole is the right one.

    {2 How fennec realizes it}

    - The CONVERTER is the {!Codec}: the very codec that powers Mongo + DDP + forms encodes the
      payload server-side ([render]) and decodes it client-side ([resource]). A value crosses ONLY
      through a codec.
    - BEHAVIOUR + LIVE DATA cross as HANDLES, never serialized: interactivity is the view's own Fur
      signals, and live/realtime data is a Pulse subscription the client RECONSTRUCTS (re-subscribes)
      rather than something seeded — seed the cheap static-derived values, reconstruct the live ones.

    {2 Safety — leak-proof by construction}

    The only thing that crosses is [codec.enc payload], in the {!Fur.Data} seed. Everything else in the
    conn block's scope (the Conn, full records, Pulse server handles) stays server-side and CANNOT
    cross — a value with no {!Codec} cannot be seeded. {!Server_only} closes the residual gap: a secret
    wrapped in it has no codec at all, so putting it in a payload is a COMPILE error (Eliom's
    no-identity-converter, by type — unlike React's taint, which is opt-in and defeated by clone). *)

(** What a page's conn block decides — render the page, or any other HTTP response. Conn-free (the
    block CLOSES OVER the Conn; the type does not name it), so it lives in the isomorphic core. *)
type 'p outcome = Render of 'p | Redirect of string | Not_found | Error of int

(** Server-only values: NO {!Codec}, so a secret held in the conn block can NEVER be seeded — putting
    one in a payload record is a COMPILE error. Use it to keep API keys / tokens un-sendable by type. *)
module Server_only : sig
  type 'a t

  val wrap : 'a -> 'a t
  val get : 'a t -> 'a
end

(** The cross-stage read — decode the seeded payload with the SAME [codec] the server encoded with
    (server: from the SSR seed; client: from [window.__FUR_DATA__]), so the view hydrates byte-for-
    byte. CREATE IT INSIDE the view (per render) so it reads the current request's seed. *)
val resource : 'a Codec.t -> key:string -> fallback:'a -> 'a Fur.Data.t

(** The default page document shell: head + scoped styles + the [#app] hydration root (the SSR'd
    body), then the seed + the page's own [bundle] <script>. No app shell, no router. *)
val default_template : string -> Fur.Doc.ctx -> Fur.vnode

(** PURE render (no Conn): seed ONLY [codec.enc value], SSR the [view], wrap it in the shell -> the
    HTML document string. The facade's [serve] runs the conn block, then calls this on [Render].
    [~template] overrides the shell; [~styles] inlines scoped CSS. *)
val render :
  key:string ->
  codec:'p Codec.t ->
  bundle:string ->
  ?styles:string ->
  ?template:(Fur.Doc.ctx -> Fur.vnode) ->
  'p ->
  (unit -> Fur.vnode) ->
  string
