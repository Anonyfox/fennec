(* The browser DDP client. Opens a WebSocket to the server's /websocket, sends [connect], and on
   each frame feeds the sub-tagged data deltas into a live merge store ({!Fennec_live}). [find] is
   the reactive Fur query over the merged collection; [call] invokes a server method. The data a
   method changes returns through the open subscription as a normal delta — no request/response
   plumbing for live data. Browser-only (Js_of_ocaml WebSocket). *)

open Js_of_ocaml
module Msg = Fennec_ddp.Message
module MS = Fennec_live.Merge_store
module Live = Fennec_live.Live

type t = {
  live : Live.t;
  send : string -> unit;
  mutable subc : int;
  mutable methodc : int;
}

(* route one decoded message: data deltas → merge store; ping → pong; rest is currently informational *)
let handle t raw =
  match try Some (Msg.decode raw) with _ -> None with
  | None -> ()
  | Some m ->
      let box = Live.store t.live in
      let s = function Some s -> s | None -> "" in
      (match m with
      | Msg.Added { collection; id; fields; sub } -> MS.added box ~sub:(s sub) ~collection ~id ~fields
      | Msg.Changed { collection; id; fields; cleared; sub } ->
          MS.changed box ~sub:(s sub) ~collection ~id ~fields ~cleared
      | Msg.Removed { collection; id; sub } -> MS.removed box ~sub:(s sub) ~collection ~id
      | Msg.Ping { id } -> t.send (Msg.encode (Msg.Pong { id }))
      | _ -> ())

let connect ?(path = "/websocket") () : t =
  let loc = Js.Unsafe.get Dom_html.window (Js.string "location") in
  let protocol = Js.to_string (Js.Unsafe.get loc (Js.string "protocol")) in
  let host = Js.to_string (Js.Unsafe.get loc (Js.string "host")) in
  let scheme = if protocol = "https:" then "wss://" else "ws://" in
  let url = scheme ^ host ^ path in
  let ws = Js.Unsafe.new_obj (Js.Unsafe.pure_js_expr "WebSocket") [| Js.Unsafe.inject (Js.string url) |] in
  let raw str = ignore (Js.Unsafe.meth_call ws "send" [| Js.Unsafe.inject (Js.string str) |]) in
  (* queue sends until the socket is open — subscribe/call can fire (on_mount) before [onopen] *)
  let is_open = ref false in
  let pending = Queue.create () in
  let send str = if !is_open then raw str else Queue.add str pending in
  let t = { live = Live.create (); send; subc = 0; methodc = 0 } in
  Js.Unsafe.set ws (Js.string "onopen")
    (Dom.handler (fun _ ->
         is_open := true;
         raw (Msg.encode (Msg.Connect { session = None; version = "1"; support = [ "1" ] }));
         Queue.iter raw pending;
         Queue.clear pending;
         Js._true));
  Js.Unsafe.set ws (Js.string "onmessage")
    (Dom.handler (fun ev ->
         handle t (Js.to_string (Js.Unsafe.get ev (Js.string "data")));
         Js._true));
  t

let subscribe t ~name ?(params = []) () =
  t.subc <- t.subc + 1;
  t.send (Msg.encode (Msg.Sub { id = "s" ^ string_of_int t.subc; name; params }))

let call t ~name ?(params = []) () =
  t.methodc <- t.methodc + 1;
  t.send (Msg.encode (Msg.Method { method_ = name; params; id = "m" ^ string_of_int t.methodc; random_seed = None }))

let find t = Live.find t.live
