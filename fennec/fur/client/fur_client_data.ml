(* Client SOURCE + seed loader — the only browser-specific half of the data layer. It
   loads window.__FUR_DATA__ (the SSR fast-render seed) into the seed table so seeded
   resources resolve synchronously on hydration (no loading flash), and installs a REAL
   network fetch (XHR GET key) as Fur.Data.source for client-only data and explicit
   refetches. The server impl lives in the SSR driver; both satisfy the same hook
   signature (the SOURCE functor, as a ref). *)
open Js_of_ocaml

let install () =
  Fur.is_browser := true;
  (* localStorage lives in the browser Platform impl — nothing to install here. *)
  let d : 'a Js.Optdef.t = Js.Unsafe.get Dom_html.window (Js.string "__FUR_DATA__") in
  Js.Optdef.iter d (fun obj ->
      let keys = Js.to_array (Js.object_keys obj) in
      Array.iter
        (fun k ->
          let v = Js.to_string (Js.Unsafe.get obj k) in
          Fur.Data.put_seed (Js.to_string k) v)
        keys);
  (* a real async GET: the key IS the URL. Resolves on a later tick (the response),
     exercising the loading -> ready transition; same-origin, so it reaches the app's
     own /api/* routes. *)
  Fur.Data.set_source
    (fun key k ->
      let xhr = XmlHttpRequest.create () in
      xhr##_open (Js.string "GET") (Js.string key) Js._true;
      xhr##.onreadystatechange :=
        Js.wrap_callback (fun () ->
            if xhr##.readyState = XmlHttpRequest.DONE && xhr##.status = 200 then
              k (Js.to_string (Js.Opt.get xhr##.responseText (fun () -> Js.string ""))));
      xhr##send Js.null)
