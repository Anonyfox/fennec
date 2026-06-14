(** A standalone PAGE — an app with no sub-router. An isomorphic [view] (a component that reads its
    payload from the seed, so it SSRs then hydrates) fused with a server-side [data] handler (the conn
    block) that runs with the {!Conn} (+ Pulse + Accounts), decides the response, and on [Render
    payload] hands exactly that — a {!Codec}-typed value — to the view and SEEDS it for the client.

    {2 The model: cross-stage persistence (the named science behind this)}

    A page is a TWO-STAGE computation: stage 1 = the server [data] handler; stage 2 = the client
    [view]/hydration. The values that must travel from stage 1 to stage 2 are exactly what
    multi-stage programming (MetaML/MetaOCaml — Taha, Kiselyov; "MetaOCaml server pages: web
    publishing as staged computation") calls the CROSS-STAGE-PERSISTENT values: the serializable
    values the later stage actually consumes. Primitives/records lift by value; closures, refs, and
    server-only handles cannot lift (no external representation) and must cross as a HANDLE, not a
    serialization. This is the same boundary Ocsigen/Eliom (OCaml's tierless web framework — Radanne,
    Vouillon, Balat) draws with its [~%x] INJECTIONS and typed CONVERTERS; and the leak-proofing
    Ur/Web gets from abstract server-only types. We are deliberately NOT Phoenix LiveView (server
    holds the page state, ships render diffs over a socket): that trades a permanent latency + state
    tax for minimal wire bytes — fennec instead has autonomous Meteor-style clients (minimongo,
    offline-for-free), so the resumable/CSP pole is the right one. (LiveView's static/dynamic
    change-tracking is still a good idea if done right — noted as future inspiration for a
    socket-connected page mode, since we already have the DDP socket.)

    {2 How fennec realizes it (three primitives we already have)}

    - The CONVERTER is the {!Codec}: the very codec that powers Mongo + DDP + forms encodes the
      payload server-side and decodes it client-side (JSON the server can re-validate). A value
      crosses ONLY through a codec.
    - BEHAVIOR + LIVE DATA cross as HANDLES, never serialized: interactivity is the view's own Fur
      signals, and live/realtime data is a Pulse subscription the client RECONSTRUCTS (re-subscribes)
      rather than something seeded — so seed the cheap static-derived values, reconstruct the live
      ones (Links' lesson: never serialize closures/continuations).

    {2 Safety — leak-proof by construction}

    The only thing that crosses is [codec.enc payload], embedded in the {!Fur.Data} seed. Everything
    else in the conn block's scope (secrets, full DB records, the Conn, Pulse server handles) stays on
    the server — and CANNOT cross, because a value with no {!Codec} cannot be seeded (the {!Conn} and
    server handles have none). Keep the payload to exactly what the view renders: it is public,
    plaintext in the page; do not put secrets or raw records in it. (Unlike React's taint APIs — opt-in
    and defeated by clone/concat — the absence of a codec is a compile error, not a courtesy.)

    Reuses the exact same seed + hydration machinery an app uses; the page just ships its own jsoo
    bundle (a true tiny SPA), and all links are real links (no client router).

    {[ let res () = Page.resource Greeting.codec ~key:"page:greet" ~fallback:Greeting.empty
       let view () = let g = Fur.Data.value (res ()) in <main><h1>(text g.who)</h1><Counter/></main>
       let page : Greeting.t Page.t =
         { key = "page:greet"; codec = Greeting.codec; view; bundle = "/_pages/greet/main.js";
           data = (fun conn -> match Accounts.current_user conn with
                               | None     -> Redirect "/login"
                               | Some uid -> Render (Greetings.for_user uid)) } ]} *)

module Conn = Fennec_paw.Conn

(** What a page's server handler (the conn block) decides — render the page, or any other response. *)
type 'p outcome = Render of 'p | Redirect of string | Not_found | Error of int

(** A page: isomorphic [view] + server [data] handler + the [codec] that transports the payload + the
    URL of the page's own jsoo [bundle] (its standalone-SPA JS, for hydration). *)
type 'p t = { key : string; codec : 'p Codec.t; view : unit -> Fur.vnode; data : Conn.t -> 'p outcome; bundle : string }

(** The payload as an isomorphic resource the [view] reads — CREATE IT INSIDE the view (per render)
    so it resolves the current request's seed. Server and client resolve it identically. Use the same
    [key]+[codec] the page's record carries. *)
val resource : 'a Codec.t -> key:string -> fallback:'a -> 'a Fur.Data.t

(** The default page document shell (head + scoped styles + body, then the seed + the page bundle
    script). [bundle] is the page's jsoo URL. *)
val default_template : string -> Fur.Doc.ctx -> Fur.vnode

(** Run the page's conn block and answer: render the seeded view, or the chosen HTTP response.
    [~template] overrides the document shell; [~styles] inlines scoped CSS. *)
val serve : 'a t -> ?styles:string -> ?template:(Fur.Doc.ctx -> Fur.vnode) -> Conn.t -> Conn.t
