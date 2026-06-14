(* A standalone PAGE — the endstate: an app with no sub-router. An isomorphic [view] (reads its
   payload from the seed, so it SSRs and then hydrates) fused with a server [data] handler (the conn
   block) that runs with the Conn (+ Pulse + Accounts), DECIDES the response, and on [Render payload]
   hands exactly that — a Codec-typed value — to the view and SEEDS it for the client.

   This module is the ISOMORPHIC essence (jsoo-safe): the cross-stage read, the pure SSR render, the
   document shell, and the Conn-free outcome. The Conn glue ([serve] + the [t] record) is added by the
   app facade (Fennec.Fur.Page); the client boot is {!Fur_csr.start_page}.

   SAFETY: the ONLY thing that crosses to the client is [codec.enc payload], embedded in the seed.
   Everything else in the conn block's scope stays on the server — the boundary is the typed payload,
   by construction (you can only seed what you can encode). {!Server_only} closes the residual gap. *)

module Bson_json = Fennec_mongo_bson_json.Bson_json

(* What a page's server conn block decides — render the page, or any other HTTP response. Conn-free,
   so it lives in the isomorphic core (the conn block CLOSES OVER the Conn; the type does not name it). *)
type 'p outcome = Render of 'p | Redirect of string | Not_found | Error of int

(* SERVER-ONLY values: a wrapper with NO {!Codec}, so a secret held in the conn block can NEVER be
   seeded — there is no [Codec.t] for ['a t], so putting one in a payload record is a COMPILE error.
   This is Eliom's no-identity-converter, by type: leaks are rejected by the compiler, not a courtesy. *)
module Server_only = struct
  type 'a t = Hold of 'a

  let wrap (x : 'a) : 'a t = Hold x
  let get (Hold x : 'a t) : 'a = x
end

(* The cross-stage read: decode the server-seeded payload with the SAME codec the server encoded with
   (server: from the SSR seed; client: from [window.__FUR_DATA__]) — resolved identically, so the view
   hydrates byte-for-byte. Declare it INSIDE the view (per render) so it reads the current seed. *)
let resource (codec : 'a Codec.t) ~key ~fallback : 'a Fur.Data.t =
  Fur.Data.resource ~key ~fallback
    ~decode:(fun s ->
      match Bson_json.of_string_opt s with
      | Some b -> ( match Codec.decode codec b with Ok v -> v | Error _ -> fallback)
      | None -> fallback)
    ()

(* The default page document shell: head + scoped styles + the #app hydration root (the SSR'd body),
   then the seed + the page's OWN bundle <script>. No app shell, no router. *)
let default_template (bundle : string) (ctx : Fur.Doc.ctx) : Fur.vnode =
  Fur.h "html"
    [ Fur.attr "lang" "en" ]
    [ Fur.h "head" [] [ Fur.Doc.head ctx; Fur.Doc.styles ctx ];
      Fur.h "body" []
        [ Fur.h "div" [ Fur.attr "id" "app" ] [ Fur.Doc.outlet ctx ] (* the hydration root start_page adopts *);
          Fur.Doc.data ctx; Fur.h "script" [ Fur.attr "src" bundle; Fur.attr "defer" "true" ] [] ] ]

(* PURE render (no Conn): seed ONLY [codec.enc value], SSR the [view], wrap it in the shell -> the HTML
   document string. The facade's [serve] runs the conn block, then calls this on [Render]. *)
let render ~key ~(codec : 'p Codec.t) ~bundle ?(styles = "") ?template (value : 'p) (view : unit -> Fur.vnode) : string =
  Fur.Data.clear_seed ();
  Fur.Data.put_seed key (Bson_json.to_string (codec.Codec.enc value));
  let body = Fur.to_html (view ()) in
  let ctx = { Fur.Doc.head = Fur.Head.to_ssr (); data = Fur.Data.to_script (); body; styles; client_js = "" } in
  Fur.document ((Option.value template ~default:(default_template bundle)) ctx)

(* ──────────────────────────── tests ──────────────────────────── *)

let contains hay needle =
  let nh = String.length hay and nn = String.length needle in
  let rec go i = if i + nn > nh then false else if String.sub hay i nn = needle then true else go (i + 1) in
  nn = 0 || go 0

type greeting = { who : string; count : int }

let greeting_codec =
  Codec.(seal (record (fun who count -> { who; count }) |> field (req "who" string) (fun g -> g.who) |> field (req "count" int) (fun g -> g.count)))

(* the resource is created INSIDE the view (per render), so it reads the seed render placed for this
   request — exactly what the fur.ppx [%%conn] split does by keeping the view isomorphic *)
let greet_view () =
  let g = resource greeting_codec ~key:"page:greet" ~fallback:{ who = "?"; count = 0 } in
  Fur.h "main" [] [ Fur.h "h1" [] [ Fur.text ("Hi " ^ (Fur.Data.value g).who) ] ]

let%test "render seeds ONLY the codec payload + emits the page bundle script + the #app root" =
  let out = render ~key:"page:greet" ~codec:greeting_codec ~bundle:"/_pages/greet/main.js" { who = "Ada"; count = 3 } greet_view in
  contains out "Hi Ada" (* SSR'd from the seeded payload *)
  && contains out {|id="app"|} (* the hydration root *)
  && contains out "__FUR_DATA__" && contains out "page:greet" (* the seed, the only thing transported *)
  && contains out "/_pages/greet/main.js" (* the page's own bundle for hydration *)

let%test "Server_only holds a value but exposes no Codec — it cannot be seeded" =
  (* there is deliberately no [Codec.t] for ['a Server_only.t]; this just proves wrap/get round-trip *)
  Server_only.get (Server_only.wrap "sk-secret") = "sk-secret"
