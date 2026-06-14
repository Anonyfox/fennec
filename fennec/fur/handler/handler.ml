(* A HANDLER — a standalone, full HTTP handler authored as ONE .mlx (frontend/handlers/<name>.mlx):
   a server [load] (conn -> outcome) fused with an isomorphic [view] (payload -> vnode). On [render
   payload] the framework seeds exactly that — a Codec-typed value — SSRs the view, and ships the
   handler's OWN jsoo bundle so it hydrates into a tiny SPA. [load] can also [redirect]/[error] — a
   handler does more than serve HTML, hence the name.

   This module is the ISOMORPHIC runtime (jsoo-safe: Fur + Codec + Bson_json) shared by the server SSR
   and the bundle. The fur ppx (-handler) turns a handler file's [payload]/[load]/[view] into a server
   [serve] (using {!render_doc}) and a client [boot] (using {!payload} + {!Fur_csr.start_page}); the
   only Conn-aware code lives inside the generated server [serve].

   {2 The model: cross-stage persistence}

   A handler is a TWO-STAGE computation: stage 1 = server [load]; stage 2 = client [view]/hydration.
   The values crossing 1->2 are the CROSS-STAGE-PERSISTENT ones (multi-stage programming — MetaOCaml;
   Eliom's injections + converters): the serializable values the view consumes. The CONVERTER is the
   {!Codec}; behaviour (Fur signals) + live data (Pulse subscriptions) cross as HANDLES, never
   serialized. We are NOT LiveView (no server-held state) — autonomous Meteor-style clients instead.

   {2 Safety — leak-proof by construction}

   The only thing that crosses is [codec.enc payload], in the {!Fur.Data} seed. The Conn, full records,
   and Pulse handles have no Codec, so they cannot be seeded. {!Server_only} closes the gap: a secret
   wrapped in it has no codec, so putting it in a payload is a COMPILE error. *)

module Bson_json = Fennec_mongo_bson_json.Bson_json

(* What [load] decides — a FULL HTTP response. Conn-free (the block CLOSES OVER the Conn; the type does
   not name it), so it lives in the isomorphic runtime. The same [view]/[payload] can be served as a
   hydrated SPA, as static no-JS HTML, or content-negotiated to JSON — a handler is a full handler. *)
type 'p outcome =
  | Render of 'p (* the rich default: hydrated SPA — seed the payload + SSR the view + ship the bundle *)
  | Html of 'p (* the SAME view as plain static HTML — no seed, no bundle, no JS (a simpler representation) *)
  | Json of string (* the payload as a plain JSON body (already encoded) *)
  | Text of string (* a plain-text body *)
  | Redirect of string
  | Not_found
  | Error of int

(* smart constructors so userland [load] reads as plain verbs: [render p] (SPA) / [html p] (plain HTML)
   / [json codec v] (data) / … — one [view]/[payload], negotiated into the representation the caller asked for *)
let render (p : 'p) : 'p outcome = Render p
let html (p : 'p) : 'p outcome = Html p
let json (codec : 'a Codec.t) (value : 'a) : 'p outcome = Json (Bson_json.to_string (codec.Codec.enc value))
let text (s : string) : 'p outcome = Text s
let redirect (url : string) : 'p outcome = Redirect url
let not_found : 'p outcome = Not_found
let error (status : int) : 'p outcome = Error status

(* SERVER-ONLY values: NO {!Codec}, so a secret held in [load] can NEVER be seeded — putting one in a
   payload is a COMPILE error (Eliom's no-identity-converter, by type). *)
module Server_only = struct
  type 'a t = Hold of 'a

  let wrap (x : 'a) : 'a t = Hold x
  let get (Hold x : 'a t) : 'a = x
end

(* CLIENT read of the cross-stage payload: decode the seed with the SAME codec the server encoded
   with. On a rendered handler the seed is ALWAYS present (the server seeds before shipping the
   bundle), so there is no fallback — a missing/garbled seed is a corrupt page, which raises. *)
let payload (codec : 'a Codec.t) ~key : 'a =
  match Hashtbl.find_opt (Fur.Data.seed_table ()) key with
  | None -> failwith ("handler: no seed for " ^ key)
  | Some s -> (
      match Bson_json.of_string_opt s with
      | Some b -> ( match Codec.decode codec b with Ok v -> v | Error _ -> failwith ("handler: bad seed for " ^ key))
      | None -> failwith ("handler: bad seed json for " ^ key))

(* The default handler document shell: head + scoped styles + the #app hydration root (the SSR'd
   body), then the seed + the handler's OWN bundle <script>. No app shell, no router. *)
let default_template (bundle : string) (ctx : Fur.Doc.ctx) : Fur.vnode =
  Fur.h "html"
    [ Fur.attr "lang" "en" ]
    [ Fur.h "head" [] [ Fur.Doc.head ctx; Fur.Doc.styles ctx ];
      Fur.h "body" []
        [ Fur.h "div" [ Fur.attr "id" "app" ] [ Fur.Doc.outlet ctx ] (* the hydration root start_page adopts *);
          Fur.Doc.data ctx; Fur.h "script" [ Fur.attr "src" bundle; Fur.attr "defer" "true" ] [] ] ]

(* PURE render (no Conn): seed ONLY [codec.enc value], SSR [view value], wrap it in the shell -> the
   HTML document string. The ppx-generated [serve] runs [load], then calls this on [render]. *)
let render_doc ~key ~(codec : 'p Codec.t) ~bundle ?(styles = "") ?template (value : 'p) (view : 'p -> Fur.vnode) : string =
  Fur.Data.clear_seed ();
  Fur.Data.put_seed key (Bson_json.to_string (codec.Codec.enc value));
  let body = Fur.to_html (view value) in
  let ctx = { Fur.Doc.head = Fur.Head.to_ssr (); data = Fur.Data.to_script (); body; styles; client_js = "" } in
  Fur.document ((Option.value template ~default:(default_template bundle)) ctx)

(* STATIC render — SSR a vnode into a plain document: head + styles + body, but NO #app root, NO seed,
   NO bundle <script>. The output is final HTML with no JS (the [Static] outcome / no-JS + SEO). *)
let render_static ?(styles = "") (body : Fur.vnode) : string =
  let body_html = Fur.to_html body (* runs the view first, so Fur.Head is populated before to_ssr *) in
  let ctx = { Fur.Doc.head = Fur.Head.to_ssr (); data = ""; body = body_html; styles; client_js = "" } in
  Fur.document
    (Fur.h "html" [ Fur.attr "lang" "en" ]
       [ Fur.h "head" [] [ Fur.Doc.head ctx; Fur.Doc.styles ctx ]; Fur.h "body" [] [ Fur.Doc.outlet ctx ] ])

(* ──────────────────────────── tests ──────────────────────────── *)

let contains hay needle =
  let nh = String.length hay and nn = String.length needle in
  let rec go i = if i + nn > nh then false else if String.sub hay i nn = needle then true else go (i + 1) in
  nn = 0 || go 0

type greeting = { who : string; count : int }

let greeting_codec =
  Codec.(seal (record (fun who count -> { who; count }) |> field (req "who" string) (fun g -> g.who) |> field (req "count" int) (fun g -> g.count)))

let greet_view (g : greeting) = Fur.h "main" [] [ Fur.h "h1" [] [ Fur.text ("Hi " ^ g.who) ] ]

let%test "render_doc seeds ONLY the codec payload + emits the bundle script + the #app root" =
  let out = render_doc ~key:"page:greet" ~codec:greeting_codec ~bundle:"/_handlers/greet/main.js" { who = "Ada"; count = 3 } greet_view in
  contains out "Hi Ada" && contains out {|id="app"|} && contains out "__FUR_DATA__" && contains out "page:greet"
  && contains out "/_handlers/greet/main.js"

let%test "smart constructors build the right full-HTTP outcomes" =
  render { who = "x"; count = 0 } = Render { who = "x"; count = 0 }
  && html { who = "x"; count = 0 } = Html { who = "x"; count = 0 }
  && redirect "/" = Redirect "/" && not_found = Not_found && text "hi" = Text "hi"
  && (match json greeting_codec { who = "a"; count = 1 } with Json s -> contains s {|"who"|} && contains s "a" | _ -> false)

let%test "render_static SSRs the view but emits NO seed and NO bundle" =
  let out = render_static (greet_view { who = "Ada"; count = 3 }) in
  contains out "Hi Ada" && (not (contains out "__FUR_DATA__")) && (not (contains out "main.js")) && (not (contains out {|id="app"|}))

let%test "Server_only holds a value but exposes no Codec — it cannot be seeded" =
  Server_only.get (Server_only.wrap "sk-secret") = "sk-secret"
