open Js_of_ocaml
let () =
  Iso_client_data.install ();              (* is_browser; load __ISO_DATA__; SOURCE *)
  Iso_client_router.install Routes.router; (* relative path from URL + nav listeners *)
  let render = App.make () in
  let app = Js.Opt.get (Dom_html.document##getElementById (Js.string "app")) (fun () -> failwith "no #app") in
  Iso_dom.hydrate_root app render;
  Iso.Data.clear_seed ();
  Iso_head.start ();
  Iso.flush_mounts ()
