(* Client navigation for Iso.Router: sets the initial relative path from the URL,
   wires pushState + popstate, and intercepts in-scope same-origin anchor clicks so
   navigation within the mount never does a full reload. The only browser half of
   the router — the matching/reverse-routing is shared, isomorphic code. *)
open Js_of_ocaml

let u = Js.Unsafe.inject
let pathname () = Js.to_string Dom_html.window##.location##.pathname
let starts_with ~prefix s =
  String.length s >= String.length prefix && String.sub s 0 (String.length prefix) = prefix

let install (t : Iso.Router.t) =
  (* initial relative path BEFORE hydration, so the first client render == SSR *)
  Iso.Router.set_path t (pathname ());
  (* navigate: pushState the absolute URL + update the relative current path *)
  (* a navigation may mount new pages/components; flush their on_mount handlers once
     they're attached (the initial flush in the client entry only covers the first
     render — components mounted later via navigation need their own flush) *)
  let nav abs = Iso.Router.set_path t abs; Iso.flush_mounts () in
  Iso.Router.nav_hook := (fun abs ->
    ignore (Js.Unsafe.meth_call (Js.Unsafe.get Dom_html.window (Js.string "history"))
              "pushState" [| u Js.null; u (Js.string ""); u (Js.string abs) |]);
    nav abs);
  (* back/forward *)
  ignore (Dom_html.addEventListener Dom_html.window Dom_html.Event.popstate
            (Dom_html.handler (fun _ -> nav (pathname ()); Js._true)) Js._false);
  (* hijack plain left-clicks on in-scope, same-origin anchors *)
  ignore (Dom_html.addEventListener Dom_html.document Dom_html.Event.click
            (Dom_html.handler (fun ev ->
               let m = Js.Unsafe.coerce ev in
               if Js.to_bool m##.metaKey || Js.to_bool m##.ctrlKey
                  || Js.to_bool m##.shiftKey || Js.to_bool m##.altKey then Js._true
               else
                 let target = (Js.Unsafe.coerce ev)##.target in
                 if not (Js.Optdef.test (Js.Unsafe.get target (Js.string "closest"))) then Js._true
                 else
                   let a : Dom_html.element Js.t Js.opt =
                     Js.Unsafe.meth_call target "closest" [| u (Js.string "a") |] in
                   Js.Opt.case a (fun () -> Js._true) (fun a ->
                     let hopt : Js.js_string Js.t Js.opt =
                       Js.Unsafe.meth_call a "getAttribute" [| u (Js.string "href") |] in
                     Js.Opt.case hopt (fun () -> Js._true) (fun h ->
                       let href = Js.to_string h in
                       let base = t.Iso.Router.base in
                       let in_scope =
                         starts_with ~prefix:"/" href && not (starts_with ~prefix:"//" href)
                         && (base = "" || href = base || starts_with ~prefix:(base ^ "/") href)
                       in
                       if in_scope then begin
                         ignore (Js.Unsafe.meth_call ev "preventDefault" [||]);
                         Iso.Router.navigate href;
                         Js._false
                       end else Js._true))))
            Js._false)
