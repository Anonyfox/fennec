(* A standalone PAGE — the endstate: an app with no sub-router. An isomorphic [view] (a component
   that reads its payload from the seed, so it SSRs and then hydrates) fused with a server-side [data]
   handler (the conn block) that runs with the Conn (+ Pulse + Accounts), DECIDES the response, and on
   [Render payload] hands exactly that — a Codec-typed value — to the view and SEEDS it for the client.

   SAFETY: the ONLY thing that crosses to the client is [p.codec.enc payload], embedded in the seed.
   Everything else in the conn block's scope (secrets, full records, the Conn) stays on the server —
   the boundary is the typed payload, by construction (you can only seed what you can encode). This
   reuses the exact same {!Fur.Data} seed + hydration machinery an app uses. *)

module Conn = Fennec_paw.Conn
module Bson_json = Fennec_mongo_bson_json.Bson_json

(* what a page's server handler (the conn block) decides — render the page, or any other HTTP response *)
type 'p outcome = Render of 'p | Redirect of string | Not_found | Error of int

(* a page = isomorphic [view] + server [data] handler + the [codec] that transports the payload, and
   the URL of the page's own jsoo bundle (its standalone-SPA JS, for hydration + interactivity) *)
type 'p t = { key : string; codec : 'p Codec.t; view : unit -> Fur.vnode; data : Conn.t -> 'p outcome; bundle : string }

(* The payload as an isomorphic resource the [view] reads (declare it at module scope, like an app's
   data resource, then [Fur.Data.value] it in the view). Server (seeded by {!serve}) and client
   (seeded from [window.__FUR_DATA__]) resolve it identically, so they hydrate byte-for-byte. Use the
   SAME [key]+[codec] the page's record carries. *)
let resource (codec : 'a Codec.t) ~key ~fallback : 'a Fur.Data.t =
  Fur.Data.resource ~key ~fallback
    ~decode:(fun s -> match Bson_json.of_string_opt s with Some b -> ( match Codec.decode codec b with Ok v -> v | Error _ -> fallback) | None -> fallback)
    ()

(* the default page document shell: head + scoped styles + the SSR'd body, then the seed + the page's
   own bundle <script> (so the page becomes alive client-side). No app shell, no router. *)
let default_template (bundle : string) (ctx : Fur.Doc.ctx) : Fur.vnode =
  Fur.h "html"
    [ Fur.attr "lang" "en" ]
    [ Fur.h "head" [] [ Fur.Doc.head ctx; Fur.Doc.styles ctx ];
      Fur.h "body" []
        [ Fur.Doc.outlet ctx; Fur.Doc.data ctx; Fur.h "script" [ Fur.attr "src" bundle; Fur.attr "defer" "true" ] [] ] ]

(* run the page's conn block and answer: render the seeded view, or the chosen HTTP response. The
   conn block (which may yield on Pulse/Eio) runs FIRST; the seed manipulation + render is a single
   yield-free tail, so it is safe on the shared data context under the framework's one-domain Eio (no
   per-request [with_data_context] needed — and that effect is unavailable outside an Eio run). *)
let serve (p : 'a t) ?(styles = "") ?template (conn : Conn.t) : Conn.t =
  match p.data conn with
  | Redirect url -> Conn.redirect conn url
  | Not_found -> Conn.text ~status:404 conn "Not found"
  | Error s -> Conn.text ~status:s conn ("Error " ^ string_of_int s)
  | Render value ->
      Fur.Data.clear_seed ();
      Fur.Data.put_seed p.key (Bson_json.to_string (p.codec.Codec.enc value));
      let body = Fur.to_html (p.view ()) in
      let ctx = { Fur.Doc.head = Fur.Head.to_ssr (); data = Fur.Data.to_script (); body; styles; client_js = "" } in
      let tmpl = Option.value template ~default:(default_template p.bundle) in
      Conn.html conn (Fur.document (tmpl ctx))

(* ──────────────────────────── tests ──────────────────────────── *)

module H = Fennec_core.Http

let contains hay needle =
  let nh = String.length hay and nn = String.length needle in
  let rec go i = if i + nn > nh then false else if String.sub hay i nn = needle then true else go (i + 1) in
  nn = 0 || go 0

type greeting = { who : string; count : int }

let greeting_codec =
  Codec.(seal (record (fun who count -> { who; count }) |> field (req "who" string) (fun g -> g.who) |> field (req "count" int) (fun g -> g.count)))

(* the resource is created INSIDE the view (per render), so it reads the seed that {!serve} placed for
   this request — exactly what the fur.ppx does by wrapping a page's top-level lets into the render *)
let greet_view () =
  let g = resource greeting_codec ~key:"page:greet" ~fallback:{ who = "?"; count = 0 } in
  Fur.h "main" [] [ Fur.h "h1" [] [ Fur.text ("Hi " ^ (Fur.Data.value g).who) ] ]

let demo : greeting t =
  { key = "page:greet"; codec = greeting_codec; bundle = "/_pages/greet/main.js"; view = greet_view;
    data = (fun conn -> match Conn.query conn "who" with Some "" | None -> Redirect "/" | Some who -> Render { who; count = String.length who }) }

let%test "serve Render seeds ONLY the codec payload + emits the page bundle script" =
  let c = Conn.make (H.make_request ~meth:H.GET ~path:"/greet" ~query_string:"who=Ada" ()) in
  let out = (Option.get (Conn.resp (serve demo c))).H.body in
  contains out "Hi Ada" (* SSR'd from the seeded payload *)
  && contains out "__FUR_DATA__" && contains out "page:greet" (* the seed, the only thing transported *)
  && contains out "/_pages/greet/main.js" (* the page's own bundle for hydration *)

let%test "serve dispatches the conn block's non-render outcomes" =
  let redirect = Conn.make (H.make_request ~meth:H.GET ~path:"/greet" ()) in
  (Option.get (Conn.resp (serve demo redirect))).H.status = 302
