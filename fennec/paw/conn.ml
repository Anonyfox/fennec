(* The connection a request flows through — the single value every paw touches.
   Inspired by Plug's conn, with two deliberate departures that OCaml lets us make
   where the BEAM could not:

   1. TYPED assigns (see {!Assigns}) — no untyped map, no casts, and no need for
      Plug's separate [private] store (type identity already isolates keys).

   2. MUTABLE-backed, with the SAME [conn -> conn] API. A paw still returns a conn
      and pipelines still read as a |> chain, but a setter mutates in place and
      returns the same physical value, so a request flowing through N paws does not
      allocate N conn records. This is safe by construction under Eio: one conn per
      request, handled sequentially in its own fiber, never shared across fibers.
      (Caveat for contributors: because it mutates, a held reference to an "earlier"
      conn sees later changes. A linear pipeline never does this; the public type is
      abstract so callers can't depend on the fields.)

   Model: building the response (status / headers / cookies) does NOT answer — the
   pipeline keeps running; only an ANSWERER (a body, redirect, stream, halt, or
   upgrade) sets the [state] away from [Unset] and short-circuits the rest of the pipe.

   This file mirrors conn.mli's sections: consumption, readers, assigns, builders,
   answerers. Server-side only — conns never cross to the client. *)

module H = Fennec_core.Http

(* The connection's lifecycle. [Unset] = still flowing (the pipeline continues);
   anything else = answered (downstream paws are skipped). A sum type instead of a
   handful of booleans makes [answered] one comparison and keeps illegal combinations
   (a body on an upgrade, say) unrepresentable. *)
type state =
  | Unset      (* no response yet — keep running paws *)
  | Set        (* a response body is set — answered *)
  | Halted     (* explicitly halted with no response — answered (=> 404 if nothing else) *)
  | Upgraded   (* a websocket upgrade is pending — answered *)
  | Streaming  (* a streamed response (file / chunks) is pending — answered *)

(* a response whose body the server streams rather than buffering into [resp_body] *)
type stream =
  | File of string * string                          (* path, content-type *)
  | Chunked of string * ((string -> unit) -> unit)   (* content-type, producer fed an [emit] *)

type t = {
  req : H.request;
  mutable status : int;                                  (* response status (default 200) *)
  mutable resp_headers : (string * string) list;         (* accumulated, most-recent first *)
  mutable resp_body : string;
  mutable state : state;
  mutable upgrade : (Fennec_core.Ws_channel.t -> unit) option;
  mutable stream : stream option;                         (* a pending streamed response *)
  mutable before_send : (H.response -> H.response) list;  (* prepended (O(1)); applied FIFO *)
  mutable assigns : Assigns.t;
  (* request views parsed on first read and cached (safe: one fiber per conn) *)
  mutable query_params : (string * string) list option;
  mutable cookies : (string * string) list option;
  mutable body_params : (string * string) list option;   (* form fields *)
  mutable files : Fennec_core.Multipart.part list option; (* multipart uploads *)
  mutable meth_override : H.meth option;                   (* set by a method-override paw *)
  mutable path_params : (string * string) list;           (* captured by a :param / *splat route *)
}

(* a fresh conn for an incoming request *)
let make (req : H.request) : t =
  { req; status = 200; resp_headers = []; resp_body = ""; state = Unset;
    upgrade = None; stream = None; before_send = []; assigns = Assigns.empty; query_params = None;
    cookies = None; body_params = None; files = None; meth_override = None; path_params = [] }

(* ============================ server-facing consumption ====================== *)
(* What the server reads off a finished conn to write the response. Not usually
   needed in userland, but part of the contract (the server is a legitimate caller). *)

let req (c : t) : H.request = c.req

(* the buffered response the conn answered with, if any *)
let resp (c : t) : H.response option =
  match c.state with
  | Set -> Some { H.status = c.status; headers = c.resp_headers; body = c.resp_body }
  | Unset | Halted | Upgraded | Streaming -> None

(* the status + headers with an empty body — for running before_send over a streamed
   or headers-only response *)
let resp_skeleton (c : t) : H.response = { H.status = c.status; headers = c.resp_headers; body = "" }

let upgrade_handler (c : t) = c.upgrade
let stream (c : t) : stream option = c.stream

(* apply all before_send hooks to a response, in registration order (FIFO) — the
   server calls this once the final response is materialized *)
let apply_before_send (c : t) (r : H.response) : H.response =
  List.fold_left (fun r f -> f r) r (List.rev c.before_send)

(* answered = anything but Unset; the runner stops feeding paws once answered *)
let answered (c : t) : bool = c.state <> Unset

(* ============================ request readers ================================ *)

let path (c : t) : string = c.req.H.path

(* the effective method — a method-override paw may have replaced it *)
let meth (c : t) : H.meth = match c.meth_override with Some m -> m | None -> c.req.H.meth

let host (c : t) : string = c.req.H.host
let scheme (c : t) : string = c.req.H.scheme
let remote_ip (c : t) : string option = c.req.H.remote_ip
let version (c : t) : string = c.req.H.version

(* a request header, case-insensitive (the first value if repeated) *)
let req_header (c : t) (k : string) : string option = Fennec_core.Headers.get c.req.H.headers k

(* all values for a (possibly repeated) request header, in order *)
let req_headers (c : t) (k : string) : string list = Fennec_core.Headers.get_all c.req.H.headers k

(* query params, percent-decoded, parsed + cached on first read *)
let query_params (c : t) : (string * string) list =
  match c.query_params with
  | Some p -> p
  | None ->
    let p = H.parse_query c.req.H.query_string in
    c.query_params <- Some p;
    p

let query (c : t) (k : string) : string option = List.assoc_opt k (query_params c)

(* request cookies, parsed + cached on first read *)
let cookies (c : t) : (string * string) list =
  match c.cookies with
  | Some ck -> ck
  | None ->
    let ck =
      match Fennec_core.Headers.get c.req.H.headers "cookie" with
      | Some h -> Fennec_core.Cookie.parse_header h
      | None -> []
    in
    c.cookies <- Some ck;
    ck

let cookie (c : t) (name : string) : string option = List.assoc_opt name (cookies c)

(* parse the body's form fields + file parts by content type, once, into the caches *)
let ensure_body (c : t) : unit =
  if c.body_params = None then begin
    let ct = Option.value (Fennec_core.Headers.get c.req.H.headers "content-type") ~default:"" in
    let lct = String.lowercase_ascii ct in
    let params, files =
      if String.starts_with ~prefix:"application/x-www-form-urlencoded" lct then
        (H.parse_query c.req.H.body, [])
      else if String.starts_with ~prefix:"multipart/form-data" lct then
        match Fennec_core.Multipart.boundary_of_content_type ct with
        | Some b ->
          let parts = Fennec_core.Multipart.parse ~boundary:b c.req.H.body in
          let fields =
            List.filter_map
              (fun (p : Fennec_core.Multipart.part) ->
                if p.filename = None then Some (p.name, p.data) else None)
              parts
          in
          let files = List.filter (fun (p : Fennec_core.Multipart.part) -> p.filename <> None) parts in
          (fields, files)
        | None -> ([], [])
      else ([], [])
    in
    c.body_params <- Some params;
    c.files <- Some files
  end

let body_params (c : t) : (string * string) list = ensure_body c; Option.value c.body_params ~default:[]
let body_param (c : t) (k : string) : string option = List.assoc_opt k (body_params c)

let files (c : t) : Fennec_core.Multipart.part list = ensure_body c; Option.value c.files ~default:[]
let file (c : t) (name : string) : Fennec_core.Multipart.part option =
  List.find_opt (fun (p : Fennec_core.Multipart.part) -> p.name = name) (files c)

let path_params (c : t) : (string * string) list = c.path_params
let path_param (c : t) (k : string) : string option = List.assoc_opt k c.path_params

(* the value for [k], checked in order: path param, then query string, then form body *)
let param (c : t) (k : string) : string option =
  match List.assoc_opt k c.path_params with
  | Some v -> Some v
  | None -> ( match query c k with Some v -> Some v | None -> body_param c k)

(* ============================ typed assigns ================================== *)
(* request-scoped, type-safe key/value storage; no casts (see {!Assigns}) *)

let assign (c : t) (k : 'a Assigns.key) (v : 'a) : t =
  c.assigns <- Assigns.set c.assigns k v;
  c

let get (c : t) (k : 'a Assigns.key) : 'a option = Assigns.get c.assigns k
let get_exn (c : t) (k : 'a Assigns.key) : 'a = Assigns.get_exn c.assigns k

(* ============================ response builders ============================== *)
(* These mutate the response but do NOT answer (state stays Unset), so a middleware
   can set a header/cookie and let the pipeline keep running; the value survives a
   later answering paw. The sole exception is documented on [set_status]. *)

(* set/override the response status. With no prior response this DOES answer (an empty
   body) — the one set_* that's terminal; after an answering paw it just overrides. *)
let set_status code (c : t) : t =
  c.status <- code;
  if c.state = Unset then c.state <- Set;
  c

(* add a response header (assoc list; accumulates, most-recent first) *)
let set_header (c : t) (k : string) (v : string) : t =
  c.resp_headers <- (k, v) :: c.resp_headers;
  c

(* set a response cookie — a Set-Cookie header (does NOT answer) *)
let set_cookie (c : t) ?path ?domain ?max_age ?expires ?secure ?http_only ?same_site
    (name : string) (value : string) : t =
  let sc =
    Fennec_core.Cookie.to_set_cookie ~name ~value ?path ?domain ?max_age ?expires ?secure
      ?http_only ?same_site ()
  in
  c.resp_headers <- ("set-cookie", sc) :: c.resp_headers;
  c

(* expire a cookie now (empty value, Max-Age=0, Expires in the past) *)
let delete_cookie (c : t) ?path ?domain (name : string) : t =
  let sc =
    Fennec_core.Cookie.to_set_cookie ~name ~value:"" ?path ?domain ~max_age:0 ~expires:0.0 ()
  in
  c.resp_headers <- ("set-cookie", sc) :: c.resp_headers;
  c

(* override the effective method (used by a method-override paw) *)
let override_method (c : t) (m : H.meth) : t = c.meth_override <- Some m; c

(* set the captured path params (used by a :param/route) *)
let set_path_params (c : t) (ps : (string * string) list) : t = c.path_params <- ps; c

(* register a hook run on the final response just before sending. O(1): we prepend and
   reverse on apply, so hooks run in registration order (FIFO). This is how a paw
   touches the RESPONSE (compression, security headers, logging) without answering. *)
let before_send (c : t) (f : H.response -> H.response) : t =
  c.before_send <- f :: c.before_send;
  c

(* ============================ answerers ===================================== *)
(* These set a response and short-circuit the rest of the pipeline (state <> Unset). *)

(* answer from a full {!H.response}. Pre-set headers (from set_header) are preserved;
   the answer's content-type wins so exactly one ships. *)
let respond (c : t) (r : H.response) : t =
  c.status <- r.H.status;
  let prior =
    if Fennec_core.Headers.mem r.H.headers "content-type" then
      Fennec_core.Headers.delete c.resp_headers "content-type"
    else c.resp_headers
  in
  c.resp_headers <- r.H.headers @ prior;
  c.resp_body <- r.H.body;
  c.state <- Set;
  c

let text ?(status = 200) ?(headers = []) (c : t) (body : string) : t =
  respond c (H.text ~status ~headers body)

let html ?(status = 200) ?(headers = []) (c : t) (body : string) : t =
  respond c (H.html ~status ~headers body)

let json ?(status = 200) ?(headers = []) (c : t) (body : string) : t =
  respond c (H.json ~status ~headers body)

(* answer with a redirect: a Location header + a 3xx status (302 by default) *)
let redirect ?(status = 302) (c : t) (location : string) : t =
  c.resp_headers <- ("location", location) :: c.resp_headers;
  c.status <- status;
  if c.state = Unset then (c.resp_body <- ""; c.state <- Set);
  c

(* stream a file from disk; the content type defaults to the path's MIME type *)
let send_file (c : t) ?content_type ~(path : string) () : t =
  let ct = match content_type with Some t -> t | None -> Fennec_core.Mime.of_path path in
  c.stream <- Some (File (path, ct));
  c.state <- Streaming;
  c

(* stream a chunked (Transfer-Encoding: chunked) body: [produce emit] is run by the
   server, calling [emit] for each chunk. Use content-type "text/event-stream" for SSE. *)
let send_chunked (c : t) ?(content_type = "application/octet-stream")
    (produce : (string -> unit) -> unit) : t =
  c.stream <- Some (Chunked (content_type, produce));
  c.state <- Streaming;
  c

(* answer by upgrading to a websocket; [setup] receives the live channel *)
let upgrade (c : t) (setup : Fennec_core.Ws_channel.t -> unit) : t =
  c.upgrade <- Some setup;
  c.state <- Upgraded;
  c

(* explicitly halt without a response — rare; mostly answering implies a halt *)
let halt (c : t) : t =
  if c.state = Unset then c.state <- Halted;
  c
