(* Namespaced persistent K/V over localStorage — the PWA persistence store. SYNCHRONOUS on purpose:
   restores happen at exact points in the client's boot/subscribe flow (no async races to reason
   about), which is what makes the offline-restore semantics provable. The tradeoff is the ~5MB
   browser quota — right-sized for "the data you were subscribed to + the write outbox"; an
   IndexedDB backend is the named seam if an app outgrows it (it would need this interface to go
   async, so it is a deliberate later decision, not a swap). All ops are total: quota overflows and
   privacy-mode failures degrade to no-ops (persistence is an enhancement, never a crash). *)

open Js_of_ocaml

let lskey ~ns key = "fennec:" ^ ns ^ ":" ^ key

let get ~ns key : string option =
  try
    Js.Optdef.case
      (Dom_html.window##.localStorage)
      (fun () -> None)
      (fun st -> Js.Opt.case (st##getItem (Js.string (lskey ~ns key))) (fun () -> None) (fun v -> Some (Js.to_string v)))
  with _ -> None

let put ~ns key (v : string) : unit =
  try
    Js.Optdef.iter (Dom_html.window##.localStorage) (fun st ->
        st##setItem (Js.string (lskey ~ns key)) (Js.string v))
  with _ -> () (* quota / privacy mode: persistence silently off *)

let del ~ns key : unit =
  try Js.Optdef.iter (Dom_html.window##.localStorage) (fun st -> st##removeItem (Js.string (lskey ~ns key)))
  with _ -> ()

(* wipe the whole namespace — the identity-change hook (logout must not leak a user's cache) *)
let purge ~ns : unit =
  try
    Js.Optdef.iter (Dom_html.window##.localStorage) (fun st ->
        let prefix = "fennec:" ^ ns ^ ":" in
        let plen = String.length prefix in
        let doomed = ref [] in
        let n = st##.length in
        for i = 0 to n - 1 do
          Js.Opt.iter (st##key i) (fun k ->
              let k = Js.to_string k in
              if String.length k >= plen && String.sub k 0 plen = prefix then doomed := k :: !doomed)
        done;
        List.iter (fun k -> st##removeItem (Js.string k)) !doomed)
  with _ -> ()
