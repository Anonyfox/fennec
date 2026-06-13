(** A standalone PAGE — an app with no sub-router. An isomorphic [view] (a component that reads its
    payload from the seed, so it SSRs then hydrates) fused with a server-side [data] handler (the conn
    block) that runs with the {!Conn} (+ Pulse + Accounts), decides the response, and on [Render
    payload] hands exactly that — a {!Codec}-typed value — to the view and SEEDS it for the client.

    SAFETY: the only thing that crosses to the client is [codec.enc payload], embedded in the
    {!Fur.Data} seed. Everything else in the conn block's scope (secrets, full records, the Conn)
    stays on the server — the boundary is the typed payload, by construction. Reuses the exact same
    seed + hydration machinery an app uses; the page just ships its own jsoo bundle (a true tiny SPA),
    and all links are real links (no client router).

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
