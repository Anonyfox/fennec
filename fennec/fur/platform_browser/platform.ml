(* Browser platform: real js_of_ocaml implementations of the virtual interface. *)
open Js_of_ocaml

let target name =
  match !Dispatch.cur with
  | None -> None
  | Some e -> (try Some (Js.Unsafe.get (Js.Unsafe.get e (Js.string "target")) (Js.string name)) with _ -> None)
let event_value () = match target "value" with Some v -> Js.to_string v | None -> ""
let event_checked () = match target "checked" with Some v -> Js.to_bool v | None -> false
let event_key () = match !Dispatch.cur with
  | Some e -> (try Js.to_string (Js.Unsafe.get e (Js.string "key")) with _ -> "") | None -> ""
let event_prevent_default () = match !Dispatch.cur with
  | Some e -> (try ignore (Js.Unsafe.meth_call e "preventDefault" [||]) with _ -> ()) | None -> ()

let local_get k =
  Js.Optdef.case (Dom_html.window##.localStorage) (fun () -> None)
    (fun s -> Js.Opt.case (s##getItem (Js.string k)) (fun () -> None) (fun v -> Some (Js.to_string v)))
let local_set k v = Js.Optdef.iter (Dom_html.window##.localStorage) (fun s -> s##setItem (Js.string k) (Js.string v))
let local_remove k = Js.Optdef.iter (Dom_html.window##.localStorage) (fun s -> s##removeItem (Js.string k))

let push_state abs =
  ignore (Js.Unsafe.meth_call (Js.Unsafe.get Dom_html.window (Js.string "history"))
            "pushState" [| Js.Unsafe.inject Js.null; Js.Unsafe.inject (Js.string ""); Js.Unsafe.inject (Js.string abs) |])

(* per-request data context — one document, single-threaded, so a single global suffices *)
let _seed : (string, string) Hashtbl.t = Hashtbl.create 16
let _source : (string -> (string -> unit) -> unit) ref = ref (fun _ _ -> ())
let with_data_context f = f ()
let seed_table () = _seed
let data_source () = !_source
let set_data_source s = _source := s
