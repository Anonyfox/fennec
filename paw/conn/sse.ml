(* Server-Sent Events — a long-lived [text/event-stream] response that pushes events to the browser's
   [EventSource], for live updates without the ceremony of websockets (notifications, progress, a
   tailing log, a price ticker). A thin, correct layer over {!Conn.send_chunked}: it formats the wire
   protocol (one [data:] line per line of payload, optional [event]/[id]/[retry], [:] comment
   heartbeats) and sets [content-type: text/event-stream] + [cache-control: no-cache].

   {[
     let events c =
       Sse.stream c (fun ~push ->
         while true do
           push (Sse.data ~event:"tick" (string_of_float (now ())));
           sleep 1.0
         done)
   ]}

   The producer runs in the server's streaming context and keeps the connection open until it returns
   (so it typically loops, blocking on whatever source it tails). [push] is the only allocation per
   event — the wire string. *)

module Conn = Conn

(* a data event (optionally named / id'd / carrying a reconnect hint) or a comment-only keepalive *)
type event = Data of { data : string; event : string option; id : string option; retry : int option } | Comment of string

let data ?event ?id ?retry data = Data { data; event; id; retry }
let comment text = Comment text

(* strip a trailing CR so a "\r\n"-delimited payload doesn't emit blank [data:] lines *)
let chomp_cr s = let n = String.length s in if n > 0 && s.[n - 1] = '\r' then String.sub s 0 (n - 1) else s

(* the SSE wire form. A field value must not contain a newline, so multi-line [data] becomes one
   [data:] line per line; the trailing blank line dispatches the event. *)
let to_wire = function
  | Comment text -> ": " ^ text ^ "\n\n"
  | Data { data; event; id; retry } ->
    let b = Buffer.create (String.length data + 16) in
    Option.iter (fun e -> Buffer.add_string b "event: "; Buffer.add_string b e; Buffer.add_char b '\n') event;
    Option.iter (fun i -> Buffer.add_string b "id: "; Buffer.add_string b i; Buffer.add_char b '\n') id;
    Option.iter (fun r -> Buffer.add_string b "retry: "; Buffer.add_string b (string_of_int r); Buffer.add_char b '\n') retry;
    List.iter (fun line -> Buffer.add_string b "data: "; Buffer.add_string b (chomp_cr line); Buffer.add_char b '\n') (String.split_on_char '\n' data);
    Buffer.add_char b '\n';
    Buffer.contents b

(* answer with an event stream; [f ~push] pushes events as they occur until it returns *)
let stream (c : Conn.t) (f : push:(event -> unit) -> unit) : Conn.t =
  let c = Conn.set_header c "cache-control" "no-cache" in
  Conn.send_chunked c ~content_type:"text/event-stream" (fun emit -> f ~push:(fun ev -> emit (to_wire ev)))

(* ──── sse tests ──── *)

let%test "plain data event" = to_wire (data "hello") = "data: hello\n\n"
let%test "named event with id" = to_wire (data ~event:"tick" ~id:"7" "x") = "event: tick\nid: 7\ndata: x\n\n"
let%test "multi-line data is one data: per line" = to_wire (data "a\nb") = "data: a\ndata: b\n\n"
let%test "CRLF payload doesn't leak a CR" = to_wire (data "a\r\nb") = "data: a\ndata: b\n\n"
let%test "retry hint" = to_wire (data ~retry:3000 "x") = "retry: 3000\ndata: x\n\n"
let%test "comment keepalive" = to_wire (comment "ping") = ": ping\n\n"

let%test "stream sets the event-stream content type and answers" =
  let c = stream (Conn.make (Http.make_request ~meth:Http.GET ~path:"/events" ())) (fun ~push:_ -> ()) in
  match Conn.stream c with Some (Conn.Chunked (ct, _)) -> ct = "text/event-stream" | _ -> false
let%test "stream marks cache-control no-cache" =
  let c = stream (Conn.make (Http.make_request ~meth:Http.GET ~path:"/events" ())) (fun ~push:_ -> ()) in
  Headers.get (Conn.resp_skeleton c).Http.headers "cache-control" = Some "no-cache"
