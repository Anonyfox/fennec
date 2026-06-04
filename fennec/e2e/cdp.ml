(* A minimal Chrome DevTools Protocol client over Eio — no Lwt, no chromedriver, no
   automation library. It speaks WebSocket (RFC 6455, client side) straight to a headless
   Chrome's debug port, then issues CDP commands (JSON request/response over that socket).

   Every [call] is bounded by a timeout, so a wedged or dead browser surfaces as a clean
   [Protocol_error] instead of hanging the caller forever. Only yojson (JSON) and base64
   (the handshake key) are pulled in, both stdlib-grade. *)

module J = Yojson.Safe

exception Protocol_error of string

let failf fmt = Printf.ksprintf (fun s -> raise (Protocol_error s)) fmt

(* substring test, stdlib only — also used by callers for assertions *)
let contains hay needle =
  let nh = String.length hay and nn = String.length needle in
  let rec go i = i + nn <= nh && (String.sub hay i nn = needle || go (i + 1)) in
  nn = 0 || go 0

(* ----------------------------------------------------------------- WebSocket frames *)
type ws = { sink : Eio.Flow.sink_ty Eio.Resource.t; r : Eio.Buf_read.t }

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
  if not (contains status "101") then failf "ws handshake failed: %s" status;
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

(* read one logical message, reassembling fragments and skipping control frames *)
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
    | 0x8 -> failf "ws closed by browser"
    | 0x9 | 0xA -> loop () (* ping / pong — ignore for a short-lived driver *)
    | _ -> Buffer.add_string buf payload; if fin then Buffer.contents buf else loop ()
  in
  loop ()

(* ----------------------------------------------------------------- CDP commands *)
type t = {
  ws : ws;
  mutable id : int;
  clock : float Eio.Time.clock_ty Eio.Resource.t;
  timeout : float;                                            (* per-command round-trip bound *)
  pending : (int, (J.t, string) result Eio.Promise.u) Hashtbl.t; (* in-flight command id -> resolver *)
  mutable handlers : (string * (J.t -> unit)) list;          (* event method -> handler (params) *)
}

(* A single daemon reader fiber owns the socket and DISPATCHES every message: a reply
   resolves the matching command promise; an event is delivered to all registered handlers.
   This is what lets us track navigation/context events continuously instead of skipping
   them — the basis for deterministic, race-free synchronisation. Callers never read the
   socket directly, so there is exactly one reader and no interleaving. *)
let attach ~sw ?(timeout = 15.0) ws clock =
  let t = { ws; id = 0; clock; timeout; pending = Hashtbl.create 32; handlers = [] } in
  Eio.Fiber.fork_daemon ~sw (fun () ->
      let rec loop () =
        match (try Some (ws_recv ws) with _ -> None) with
        | None -> `Stop_daemon (* socket closed / cancelled *)
        | Some raw ->
          (match (try J.from_string raw with _ -> `Null) with
           | `Assoc f -> (
             match List.assoc_opt "id" f with
             | Some (`Int i) -> (
               match Hashtbl.find_opt t.pending i with
               | None -> ()
               | Some u ->
                 Hashtbl.remove t.pending i;
                 (match List.assoc_opt "error" f with
                  | Some e -> Eio.Promise.resolve u (Error (J.to_string e))
                  | None -> Eio.Promise.resolve u (Ok (match List.assoc_opt "result" f with Some r -> r | None -> `Null))))
             | _ -> (
               match List.assoc_opt "method" f with
               | Some (`String m) ->
                 let p = match List.assoc_opt "params" f with Some p -> p | None -> `Null in
                 List.iter (fun (meth, h) -> if meth = m then (try h p with _ -> ())) t.handlers
               | _ -> ()))
           | _ -> ());
          loop ()
      in
      loop ());
  t

(* register a persistent handler for a CDP event method (handlers fire on the reader fiber) *)
let on t meth (h : J.t -> unit) = t.handlers <- (meth, h) :: t.handlers

let call ?timeout ?sess t meth params : J.t =
  let tmo = match timeout with Some x -> x | None -> t.timeout in
  t.id <- t.id + 1;
  let id = t.id in
  let p, u = Eio.Promise.create () in
  Hashtbl.replace t.pending id u;
  let fields =
    [ ("id", `Int id); ("method", `String meth); ("params", params) ]
    @ (match sess with Some s -> [ ("sessionId", `String s) ] | None -> [])
  in
  ws_send t.ws (J.to_string (`Assoc fields));
  match Eio.Time.with_timeout t.clock tmo (fun () -> Ok (Eio.Promise.await p)) with
  | Ok (Ok r) -> r
  | Ok (Error e) -> failf "%s → %s" meth e
  | Error `Timeout -> Hashtbl.remove t.pending id; failf "%s timed out after %.0fs (browser unresponsive)" meth tmo

(* small JSON helpers for callers *)
let field k = function `Assoc f -> List.assoc_opt k f | _ -> None
let as_string = function Some (`String s) -> s | _ -> ""
let as_int = function Some (`Int i) -> i | Some (`Float f) -> int_of_float f | _ -> 0
let as_bool = function Some (`Bool b) -> b | _ -> false
