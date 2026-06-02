(* Client runtime boot — the whole hydration dance behind one call.
   [start ~root ~router ()] installs the data SOURCE + seed, activates and wires the
   router, hydrates the SSR'd #app, consumes the seed, starts the head reconciler,
   and runs on_mount handlers. The app's client entry is then a single line. *)
open Js_of_ocaml

let start ~(root : unit -> unit -> Iso.vnode) ~(router : Iso.Router.t) () =
  Iso_client_data.install ();        (* is_browser; load __ISO_DATA__; SOURCE *)
  Iso.Router.activate router;        (* ambient: p/param/outlet resolve to this app *)
  Iso_client_router.install router;  (* relative path from URL + nav listeners *)
  let app = Js.Opt.get (Dom_html.document##getElementById (Js.string "app")) (fun () -> failwith "no #app") in
  Iso_dom.hydrate_root app (root ());
  Iso.Data.clear_seed ();            (* consume: later/dynamic fetches hit the network *)
  Iso_head.start ();
  Iso.flush_mounts ()
