(* the event currently being dispatched — set by the reconciler around each handler,
   read by the Platform event accessors. (Kept here, in the impl lib, so the reconciler
   and the readers share it without the core ever seeing a js_of_ocaml type.) *)
let cur : Js_of_ocaml.Js.Unsafe.any option ref = ref None
let set e = cur := Some e
let clear () = cur := None
