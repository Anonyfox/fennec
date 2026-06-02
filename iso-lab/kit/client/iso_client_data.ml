(* Client SOURCE + seed loader. The only browser-specific half of the data layer:
   it loads window.__ISO_DATA__ into the seed table and installs a real (here faked
   with setTimeout) async fetch as Iso.Data.source. Server impl lives in the SSR
   driver; both satisfy the same hook signature (the SOURCE functor, as a ref). *)
open Js_of_ocaml

(* stands in for a real network fetch. Values DIFFER from the server's so the E2E
   can distinguish a seeded value (free, from SSR) from a real client fetch. *)
let client_remote = function
  | "/api/greeting" -> "Hello again, from the client \xf0\x9f\x94\x81"
  | "/api/browser-only" -> "Loaded in the browser \xe2\x9c\xa8"
  | k -> "client:" ^ k

let install () =
  Iso.is_browser := true;
  (* real Browser facade over js_of_ocaml localStorage (no-op if unavailable) *)
  Iso.Browser.install {
    Iso.Browser.local_get = (fun k ->
      Js.Optdef.case (Dom_html.window##.localStorage) (fun () -> None)
        (fun s -> Js.Opt.case (s##getItem (Js.string k)) (fun () -> None) (fun v -> Some (Js.to_string v))));
    local_set = (fun k v -> Js.Optdef.iter (Dom_html.window##.localStorage) (fun s -> s##setItem (Js.string k) (Js.string v)));
    local_remove = (fun k -> Js.Optdef.iter (Dom_html.window##.localStorage) (fun s -> s##removeItem (Js.string k)));
  };
  let d : 'a Js.Optdef.t = Js.Unsafe.get Dom_html.window (Js.string "__ISO_DATA__") in
  Js.Optdef.iter d (fun obj ->
    let keys = Js.to_array (Js.object_keys obj) in
    Array.iter (fun k ->
      let v = Js.to_string (Js.Unsafe.get obj k) in
      Iso.Data.put_seed (Js.to_string k) v) keys);
  (* a real async fetch: resolves on a later tick, exercising loading -> ready *)
  Iso.Data.source := (fun key k ->
    let cb = Js.wrap_callback (fun () -> k (client_remote key)) in
    ignore (Dom_html.window##setTimeout cb (Js.number_of_float 30.)))
