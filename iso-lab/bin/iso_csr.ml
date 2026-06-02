(* Client runtime boot. Picks the app whose base matches the URL, activates it,
   hydrates the SSR'd #app, then consumes the seed + runs head/mounts. Generic over
   the generated [mount list] — no per-app code. *)
open Js_of_ocaml

let starts_with p s = String.length s >= String.length p && String.sub s 0 (String.length p) = p
let dispatch (mounts : Iso.mount list) path =
  List.filter (fun (m : Iso.mount) -> m.base = "" || path = m.base || starts_with (m.base ^ "/") path) mounts
  |> List.sort (fun (a : Iso.mount) b -> compare (String.length b.base) (String.length a.base))
  |> function m :: _ -> Some m | [] -> None

let start (mounts : Iso.mount list) =
  Iso_client_data.install ();
  let path = Js.to_string Dom_html.window##.location##.pathname in
  match dispatch mounts path with
  | None -> ()
  | Some m ->
    Iso.Router.activate m.router;
    Iso_client_router.install m.router;
    let app = Js.Opt.get (Dom_html.document##getElementById (Js.string "app")) (fun () -> failwith "no #app") in
    Iso_dom.hydrate_root app (m.root ());
    Iso.Data.clear_seed ();
    Iso_head.start ();
    Iso.flush_mounts ()
