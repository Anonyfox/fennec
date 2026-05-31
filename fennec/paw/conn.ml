(* The connection a request flows through — the single value every paw touches.
   Inspired by Plug's conn, with TYPED assigns. A conn carries the request, an
   optional response (set => the pipeline halts), and request-scoped assigns. It
   is threaded immutably: a paw returns a new conn.

   Server-side only (no Melange story needed — conns never cross to the client). *)

module H = Fennec_core.Http

type t = {
  req : H.request;
  resp : H.response option; (* set => answered => pipeline short-circuits *)
  upgrade : (Fennec_core.Ws_channel.t -> unit) option;
      (* a websocket upgrade: when set, the server performs the RFC 6455 handshake
         and runs this setup fn on the live channel instead of writing [resp].
         This is how the websocket is itself a paw. *)
  before_send : (H.response -> H.response) list;
      (* hooks applied to the final response just before it is written, in
         registration order. How a paw that must touch the RESPONSE (compression,
         logging, security headers) works without wrapping the pipeline. *)
  assigns : Assigns.t;
  halted : bool; (* explicit halt (independent of resp, e.g. to stop early) *)
}

(* a fresh conn for an incoming request *)
let make (req : H.request) : t =
  { req; resp = None; upgrade = None; before_send = []; assigns = Assigns.empty; halted = false }

(* register a hook to run on the final response just before sending (FIFO) *)
let before_send (c : t) (f : H.response -> H.response) : t =
  { c with before_send = c.before_send @ [ f ] }

(* apply all before_send hooks to a response (server calls this) *)
let apply_before_send (c : t) (r : H.response) : H.response =
  List.fold_left (fun r f -> f r) r c.before_send

let req (c : t) : H.request = c.req
let resp (c : t) : H.response option = c.resp
let upgrade_handler (c : t) = c.upgrade

(* answered = has a response, a pending ws upgrade, OR was explicitly halted; the
   runner stops feeding further paws once a conn is answered *)
let answered (c : t) : bool = c.resp <> None || c.upgrade <> None || c.halted

(* answer by upgrading to a websocket; [setup] receives the live channel *)
let upgrade (c : t) (setup : Fennec_core.Ws_channel.t -> unit) : t = { c with upgrade = Some setup }

(* set the response (the terminal move of an answering paw) *)
let respond (c : t) (r : H.response) : t = { c with resp = Some r }

(* explicitly halt without (yet) a response — rare; mostly resp implies halt *)
let halt (c : t) : t = { c with halted = true }

(* ---- typed assigns passthrough ---- *)
let assign (c : t) (k : 'a Assigns.key) (v : 'a) : t = { c with assigns = Assigns.set c.assigns k v }
let get (c : t) (k : 'a Assigns.key) : 'a option = Assigns.get c.assigns k
let get_exn (c : t) (k : 'a Assigns.key) : 'a = Assigns.get_exn c.assigns k

(* ---- response helpers (answer + set content-type) ---- *)
let text ?(status = 200) ?(headers = []) (c : t) (body : string) : t =
  respond c (H.text ~status ~headers body)

let html ?(status = 200) ?(headers = []) (c : t) (body : string) : t =
  respond c (H.html ~status ~headers body)

let json ?(status = 200) ?(headers = []) (c : t) (body : string) : t =
  respond c (H.json ~status ~headers body)

let status code (c : t) : t =
  (* set/override the status of the current response (or an empty one) *)
  match c.resp with
  | Some r -> respond c { r with H.status = code }
  | None -> respond c (H.respond ~status:code "")

(* add a response header (on the current or an empty response) *)
let put_header (c : t) (k : string) (v : string) : t =
  match c.resp with
  | Some r -> respond c { r with H.headers = (k, v) :: r.H.headers }
  | None -> respond c (H.respond ~headers:[ (k, v) ] "")

(* read a request header (case-insensitive) *)
let req_header (c : t) (k : string) : string option =
  Fennec_core.Http_semantics.header c.req.H.headers k

let path (c : t) : string = c.req.H.path
let meth (c : t) : H.meth = c.req.H.meth
