(* The browser bridge for the generated PWA registration: a Fur signal that flips when a NEW build's
   service worker is installed and waiting (the `fennec:sw-update` window event), and the
   user-confirmed apply (SKIP_WAITING + reload on controllerchange, both wired by the generated
   registration script). The UI's whole update flow is:
     if Fur.get (Pwa_client.update_available ()) then <button onClick=Pwa_client.apply_update>… *)

open Js_of_ocaml

let _update_sig : bool Fur.signal option ref = ref None

(* a Fur signal that flips to [true] once a new app version is installed and waiting *)
let update_available () : bool Fur.signal =
  match !_update_sig with
  | Some s -> s
  | None ->
      let s = Fur.signal false in
      _update_sig := Some s;
      ignore
        (Js.Unsafe.meth_call Dom_html.window "addEventListener"
           [| Js.Unsafe.inject (Js.string "fennec:sw-update");
              Js.Unsafe.inject (Js.wrap_callback (fun _ -> Fur.set s true)) |]);
      s

(* swap to the waiting worker and reload — call only on user confirmation (a silent mid-session
   swap could mix bundle versions) *)
let apply_update () : unit =
  ignore (Js.Unsafe.eval_string "window.__fennecApplyUpdate && window.__fennecApplyUpdate()")
