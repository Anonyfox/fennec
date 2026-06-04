(* Client runtime boot. Picks the app whose base matches the URL, activates it,
   hydrates the SSR'd #app, then consumes the seed + runs head/mounts. Generic over
   the generated [mount list] — no per-app code. *)
open Js_of_ocaml

let starts_with p s = String.length s >= String.length p && String.sub s 0 (String.length p) = p
let dispatch (mounts : Fur.mount list) path =
  List.filter (fun (m : Fur.mount) -> m.base = "" || path = m.base || starts_with (m.base ^ "/") path) mounts
  |> List.sort (fun (a : Fur.mount) b -> compare (String.length b.base) (String.length a.base))
  |> function m :: _ -> Some m | [] -> None

let start (mounts : Fur.mount list) =
  Fur_client_data.install ();
  let path = Js.to_string Dom_html.window##.location##.pathname in
  match dispatch mounts path with
  | None -> ()
  | Some m ->
    Fur.Router.activate m.router;
    Fur_client_router.install m.router;
    let app = Js.Opt.get (Dom_html.document##getElementById (Js.string "app")) (fun () -> failwith "no #app") in
    Fur_dom.hydrate_root app (m.root ());
    Fur.Data.clear_seed ();
    Fur_head.start ();
    Fur.flush_mounts ();
    (* a stable, app-agnostic "client booted + hydrated" signal, so e2e drivers can await
       hydration deterministically instead of guessing at app-specific DOM. *)
    Js.Unsafe.set Dom_html.window (Js.string "__fur_hydrated") Js._true
