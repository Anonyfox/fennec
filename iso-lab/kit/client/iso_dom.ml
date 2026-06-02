(* The js_of_ocaml BACKEND for the core reconciler, plus the hydrate/render entry. The
   diff algorithm itself lives in Iso.Reconcile (core, platform-agnostic + unit-tested
   with a fake backend); here we only map its operations onto the real DOM. *)
open Js_of_ocaml

module Jsoo : Iso.BACKEND with type node = Dom.node Js.t = struct
  type node = Dom.node Js.t
  let u = Js.Unsafe.inject
  let doc = Dom_html.document
  let create_text s = (doc##createTextNode (Js.string s) :> Dom.node Js.t)
  let create_element tag = (doc##createElement (Js.string tag) :> Dom.node Js.t)
  let get_text n = (try Js.to_string (Js.Unsafe.get n (Js.string "data")) with _ -> "")
  let set_text n s = Js.Unsafe.set n (Js.string "data") (Js.string s)
  let get_attr n k =
    let o = Js.Unsafe.meth_call n "getAttribute" [| u (Js.string k) |] in
    Js.Opt.case o (fun () -> None) (fun v -> Some (Js.to_string v))
  let set_attr n k v = ignore (Js.Unsafe.meth_call n "setAttribute" [| u (Js.string k); u (Js.string v) |])
  let remove_attr n k = ignore (Js.Unsafe.meth_call n "removeAttribute" [| u (Js.string k) |])
  let set_prop n k v =
    if k = "checked" then Js.Unsafe.set n (Js.string "checked") (Js.bool (v <> ""))
    else Js.Unsafe.set n (Js.string k) (Js.string v)
  let get_prop n k = (try Js.to_string (Js.Unsafe.get n (Js.string k)) with _ -> "")
  let append p c = Dom.appendChild p c
  let remove p c = Dom.removeChild p c
  let replace p nw od = Dom.replaceChild p nw od
  let parent n = Js.Opt.to_option n##.parentNode
  let listen n ev r =
    ignore (Dom.addEventListener n (Dom.Event.make ev)
      (Dom.handler (fun e -> Dispatch.set (Js.Unsafe.inject e);
        Fun.protect ~finally:Dispatch.clear (fun () -> !r ()); Js._true)) Js._false)
  let child n i = Js.Opt.to_option (n##.childNodes##item i)
  let first_child n = Js.Opt.to_option n##.firstChild
end

module D = Iso.Reconcile (Jsoo)

let hydrate_root (container : Dom_html.element Js.t) (render : unit -> Iso.vnode) =
  ignore (D.mount_root (container :> Dom.node Js.t) render)
