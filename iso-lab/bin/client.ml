open Js_of_ocaml
let () =
  let render = App.make () in
  let app = Js.Opt.get (Dom_html.document##getElementById (Js.string "app")) (fun () -> failwith "no #app") in
  Iso_dom.hydrate_root app render;  (* hydrate body: re-runs setups -> re-registers head *)
  Iso_head.start ()                 (* adopt SSR'd head tags, then keep them reactive *)
