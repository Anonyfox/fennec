open Js_of_ocaml
let () =
  Iso_client_data.install ();   (* is_browser := true; load __ISO_DATA__; set SOURCE *)
  let render = App.make () in
  let app = Js.Opt.get (Dom_html.document##getElementById (Js.string "app")) (fun () -> failwith "no #app") in
  Iso_dom.hydrate_root app render;  (* initial render reads seeds synchronously -> no flash *)
  Iso.Data.clear_seed ();           (* consume: subsequent/dynamic fetches hit the network *)
  Iso_head.start ();
  Iso.flush_mounts ()               (* run browser-only on_mount handlers, post-hydration *)
