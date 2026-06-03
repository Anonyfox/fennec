(* A minimal Chrome DevTools Protocol client over Eio — no Lwt, no chromedriver, no npm,
   no external automation library. It speaks WebSocket (RFC 6455, client side) straight to
   a headless Chrome's debug port, then drives the page over CDP: navigate, evaluate JS,
   read results. Enough for a real-browser e2e in pure OCaml/Eio.

   Only yojson (JSON) and base64 (the handshake key) are pulled in — both stdlib-grade. *)

module J = Yojson.Safe

(* ----------------------------------------------------------------- WebSocket frames *)
type ws = { sink : Eio.Flow.sink_ty Eio.Resource.t; r : Eio.Buf_read.t }

let contains hay needle =
  let nh = String.length hay and nn = String.length needle in
  let rec go i = i + nn <= nh && (String.sub hay i nn = needle || go (i + 1)) in
  nn = 0 || go 0

(* open a TCP connection, perform the HTTP Upgrade handshake, return a frame channel *)
let ws_connect ~sw net ~port ~path : ws =
  let flow = Eio.Net.connect ~sw net (`Tcp (Eio.Net.Ipaddr.V4.loopback, port)) in
  let key = Base64.encode_string "fennec-cdp-0123" in (* 16 bytes; value is not verified *)
  let req =
    Printf.sprintf
      "GET %s HTTP/1.1\r\nHost: localhost:%d\r\nUpgrade: websocket\r\nConnection: Upgrade\r\n\
       Sec-WebSocket-Key: %s\r\nSec-WebSocket-Version: 13\r\n\r\n"
      path port key
  in
  Eio.Flow.copy_string req (flow :> _ Eio.Flow.sink);
  let r = Eio.Buf_read.of_flow (flow :> _ Eio.Flow.source) ~max_size:(64 * 1024 * 1024) in
  let status = Eio.Buf_read.line r in
  if not (contains status "101") then failwith ("cdp: ws handshake failed: " ^ status);
  let rec drain () = match Eio.Buf_read.line r with "" -> () | _ -> drain () in
  drain ();
  { sink = (flow :> Eio.Flow.sink_ty Eio.Resource.t); r }

let ws_send ws (s : string) =
  let n = String.length s in
  let b = Buffer.create (n + 8) in
  Buffer.add_char b '\x81'; (* FIN + text opcode *)
  if n < 126 then Buffer.add_char b (Char.chr (0x80 lor n))
  else if n < 65536 then begin
    Buffer.add_char b (Char.chr (0x80 lor 126));
    Buffer.add_char b (Char.chr ((n lsr 8) land 0xff));
    Buffer.add_char b (Char.chr (n land 0xff))
  end
  else begin
    Buffer.add_char b (Char.chr (0x80 lor 127));
    for i = 7 downto 0 do Buffer.add_char b (Char.chr ((n lsr (i * 8)) land 0xff)) done
  end;
  Buffer.add_string b "\x00\x00\x00\x00"; (* mask key = 0 → masked payload == payload *)
  Buffer.add_string b s;
  Eio.Flow.copy_string (Buffer.contents b) ws.sink

(* read one logical message, reassembling fragments and answering pings *)
let ws_recv ws : string =
  let byte () = Char.code (Eio.Buf_read.take 1 ws.r).[0] in
  let buf = Buffer.create 256 in
  let rec loop () =
    let b0 = byte () in
    let fin = b0 land 0x80 <> 0 and opcode = b0 land 0x0f in
    let b1 = byte () in
    let masked = b1 land 0x80 <> 0 and len0 = b1 land 0x7f in
    let len =
      if len0 < 126 then len0
      else if len0 = 126 then (let s = Eio.Buf_read.take 2 ws.r in (Char.code s.[0] lsl 8) lor Char.code s.[1])
      else begin
        let s = Eio.Buf_read.take 8 ws.r in
        let v = ref 0 in
        for i = 0 to 7 do v := (!v lsl 8) lor Char.code s.[i] done;
        !v
      end
    in
    let mask = if masked then Eio.Buf_read.take 4 ws.r else "" in
    let payload = Eio.Buf_read.take len ws.r in
    let payload =
      if masked then String.mapi (fun i c -> Char.chr (Char.code c lxor Char.code mask.[i land 3])) payload
      else payload
    in
    match opcode with
    | 0x8 -> failwith "cdp: ws closed by browser"
    | 0x9 -> loop () (* ping — Chrome doesn't expect a pong for our short-lived test *)
    | 0xA -> loop ()
    | _ ->
      Buffer.add_string buf payload;
      if fin then Buffer.contents buf else loop ()
  in
  loop ()

(* ----------------------------------------------------------------- CDP commands *)
type t = { ws : ws; mutable id : int; clock : float Eio.Time.clock_ty Eio.Resource.t }

let attach ws clock = { ws; id = 0; clock }

let call ?sess t meth params : J.t =
  t.id <- t.id + 1;
  let id = t.id in
  let fields =
    [ ("id", `Int id); ("method", `String meth); ("params", params) ]
    @ (match sess with Some s -> [ ("sessionId", `String s) ] | None -> [])
  in
  ws_send t.ws (J.to_string (`Assoc fields));
  let rec wait () =
    match J.from_string (ws_recv t.ws) with
    | `Assoc f -> (
      match List.assoc_opt "id" f with
      | Some (`Int i) when i = id -> (
        match List.assoc_opt "error" f with
        | Some e -> failwith ("cdp: " ^ meth ^ " → " ^ J.to_string e)
        | None -> ( match List.assoc_opt "result" f with Some r -> r | None -> `Null ))
      | _ -> wait () (* an event or another id — skip *))
    | _ -> wait ()
  in
  wait ()

let field k = function `Assoc f -> List.assoc_opt k f | _ -> None
let str = function Some (`String s) -> s | _ -> ""

(* attach to a fresh page target (flatten mode: one socket, sessionId-tagged) *)
let new_page t : string =
  let tgt = call t "Target.createTarget" (`Assoc [ ("url", `String "about:blank") ]) in
  let target_id = str (field "targetId" tgt) in
  let att =
    call t "Target.attachToTarget"
      (`Assoc [ ("targetId", `String target_id); ("flatten", `Bool true) ])
  in
  str (field "sessionId" att)

let navigate t sess url = ignore (call ~sess t "Page.navigate" (`Assoc [ ("url", `String url) ]))

(* evaluate a JS expression in the page, awaiting promises, returning the value by value *)
let eval t sess expr : J.t =
  let r =
    call ~sess t "Runtime.evaluate"
      (`Assoc
         [ ("expression", `String expr); ("returnByValue", `Bool true); ("awaitPromise", `Bool true) ])
  in
  match field "result" r with Some inner -> ( match field "value" inner with Some v -> v | None -> `Null ) | None -> `Null

let eval_str t sess expr = match eval t sess expr with `String s -> s | `Null -> "" | v -> J.to_string v
let eval_bool t sess expr = match eval t sess expr with `Bool b -> b | _ -> false

(* poll an expression until its string result contains [want] (or time out) *)
let poll_contains t sess ~desc ~want ~timeout expr =
  let deadline = Eio.Time.now t.clock +. timeout in
  let rec loop () =
    (* tolerate transient CDP errors: right after a navigation the execution context is
       being recreated, so an evaluate can fail for a few ms — treat as "not ready yet". *)
    let got = try eval_str t sess expr with _ -> "" in
    if contains got want then got
    else if Eio.Time.now t.clock > deadline then
      failwith (Printf.sprintf "cdp: %s timed out (wanted %S, last %S)" desc want got)
    else ( Eio.Time.sleep t.clock 0.05; loop () )
  in
  loop ()
