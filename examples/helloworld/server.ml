(* The helloworld server — the leanest fullstack fennec app, and our DX taste
   check. A PLAIN dune executable: `dune exec` runs it, `fennec dev` runs it with
   livereload.

   Everything operational — the Eio entry point, the dev/prod web-root flip,
   static serving, livereload, the server lifecycle — lives in the `Fennec`
   facade. This file is just the app's intent: render a page, serve it. *)

let name = "world"

(* SSR the SAME <App> the client hydrates; Fennec.document wraps it in a full HTML
   document (client bundles + stylesheet + hydration props) for us. *)
let render _req =
  Fennec.document ~title:"fennec — hello world"
    ~description:"A server-rendered, hydrated fennec app."
    ~props_json:(Printf.sprintf {|{"name":%S}|} name)
    ~body_html:(ReactDOM.renderToString (App_native.App.make ~name ()))
    ()

let () =
  Fennec.app ~assets:Webroot_assets.lookup ()
  |> Fennec.get "/" render
  |> Fennec.serve ~port:8200
