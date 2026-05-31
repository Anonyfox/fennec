(* The helloworld server — the leanest fullstack fennec app, and our DX taste
   check. A PLAIN dune executable: `dune exec` runs it, `fennec dev` runs it with
   livereload.

   Everything operational — the Eio entry point, the dev/prod web-root flip,
   static serving, livereload, the server lifecycle — lives in the `Fennec`
   facade. This file is just the app's intent: render a page, serve it. *)

let name = "world"

(* SSR the SAME <App> the client hydrates. Fennec.render builds the element via a
   thunk, collecting the <Head> tags the tree sets (title/description/og), and
   renders them into the document's <head> — no hardcoded title here. *)
let render _req =
  Fennec.render
    ~props_json:(Printf.sprintf {|{"name":%S}|} name)
    ~body:(fun () -> App_native.App.make ~name ())
    ()

let () =
  Fennec.app ~assets:Webroot_assets.lookup ()
  |> Fennec.get "/" render
  |> Fennec.serve ~port:8200
