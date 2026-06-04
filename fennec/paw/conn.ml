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

   The connection carries the request, a response being built up (status/headers/
   body), request-scoped assigns, before_send hooks, and a [state]. Building the
   response (status/headers) does NOT answer; only setting a body (respond/text/…)
   or halting/upgrading does — an answered conn short-circuits the rest of the pipe.

   Server-side only — conns never cross to the client. *)

module H = Fennec_core.Http

(* The connection's lifecycle. [Unset] = still flowing (the pipeline continues);
   anything else = answered (downstream paws are skipped). A sum type instead of
   three booleans makes [answered] one comparison and makes illegal states (a body
   on an upgrade, say) unrepresentable as we grow toward streaming. *)
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
  mutable query_params : (string * string) list option;   (* lazily parsed on first read *)
  mutable cookies : (string * string) list option;        (* lazily parsed request cookies *)
  mutable body_params : (string * string) list option;    (* lazily parsed form fields *)
  mutable files : Fennec_core.Multipart.part list option; (* lazily parsed file parts *)
  mutable meth_override : H.meth option;                   (* set by a method-override plug *)
  mutable path_params : (string * string) list;            (* captured by a :param / *splat route *)
}

(* a fresh conn for an incoming request *)
let make (req : H.request) : t =
  { req; status = 200; resp_headers = []; resp_body = ""; state = Unset;
    upgrade = None; stream = None; before_send = []; assigns = Assigns.empty; query_params = None;
    cookies = None; body_params = None; files = None; meth_override = None; path_params = [] }

(* register a hook to run on the final response just before sending. O(1): we
   prepend and reverse on apply, so the hooks run in registration order (FIFO).
   This is how a paw touches the RESPONSE (compression, security headers, logging)
   without answering and short-circuiting the chain. *)
let before_send (c : t) (f : H.response -> H.response) : t =
  c.before_send <- f :: c.before_send;
  c

(* apply all before_send hooks to a response, in registration order (the server
   calls this once the final response is materialized) *)
let apply_before_send (c : t) (r : H.response) : H.response =
  List.fold_left (fun r f -> f r) r (List.rev c.before_send)

let req (c : t) : H.request = c.req

(* materialize the response the conn has built, if it has answered with a buffered body *)
let resp (c : t) : H.response option =
  match c.state with
  | Set -> Some { H.status = c.status; headers = c.resp_headers; body = c.resp_body }
  | Unset | Halted | Upgraded | Streaming -> None

(* the status + headers the conn has built, with an empty body — for the server to run
   before_send over a streamed/headers-only response *)
let resp_skeleton (c : t) : H.response = { H.status = c.status; headers = c.resp_headers; body = "" }

let upgrade_handler (c : t) = c.upgrade
let stream (c : t) : stream option = c.stream

(* answered = anything but Unset; the runner stops feeding paws once answered *)
let answered (c : t) : bool = c.state <> Unset

(* answer by upgrading to a websocket; [setup] receives the live channel *)
let upgrade (c : t) (setup : Fennec_core.Ws_channel.t -> unit) : t =
  c.upgrade <- Some setup;
  c.state <- Upgraded;
  c

(* set the response from a full {!H.response} (the terminal move of an answering
   paw). Pre-set headers (from set_header) are preserved — the answer's headers are
   merged in front of them. *)
let respond (c : t) (r : H.response) : t =
  c.status <- r.H.status;
  (* the answer's content-type wins: drop any one pre-set via set_header, so exactly one
     content-type ships *)
  let prior =
    if Fennec_core.Headers.mem r.H.headers "content-type" then
      Fennec_core.Headers.delete c.resp_headers "content-type"
    else c.resp_headers
  in
  c.resp_headers <- r.H.headers @ prior;
  c.resp_body <- r.H.body;
  c.state <- Set;
  c

(* explicitly halt without a response — rare; mostly answering implies a halt *)
let halt (c : t) : t =
  if c.state = Unset then c.state <- Halted;
  c

(* ---- typed assigns passthrough ---- *)
let assign (c : t) (k : 'a Assigns.key) (v : 'a) : t =
  c.assigns <- Assigns.set c.assigns k v;
  c

let get (c : t) (k : 'a Assigns.key) : 'a option = Assigns.get c.assigns k
let get_exn (c : t) (k : 'a Assigns.key) : 'a = Assigns.get_exn c.assigns k

(* ---- response building ---- *)

(* answer with a body + content type *)
let text ?(status = 200) ?(headers = []) (c : t) (body : string) : t =
  respond c (H.text ~status ~headers body)

let html ?(status = 200) ?(headers = []) (c : t) (body : string) : t =
  respond c (H.html ~status ~headers body)

let json ?(status = 200) ?(headers = []) (c : t) (body : string) : t =
  respond c (H.json ~status ~headers body)

(* set/override the response status. Standalone (no body) this answers with that
   status and an empty body; after an answering paw it just overrides the code. *)
let set_status code (c : t) : t =
  c.status <- code;
  if c.state = Unset then c.state <- Set;
  c

(* add a response header. Unlike Plug-on-BEAM's cost model we keep an assoc list;
   this does NOT answer — headers accumulate and survive a later answering paw. *)
let set_header (c : t) (k : string) (v : string) : t =
  c.resp_headers <- (k, v) :: c.resp_headers;
  c

(* answer with a redirect: a Location header + a 3xx status (302 by default) *)
let redirect ?(status = 302) (c : t) (location : string) : t =
  c.resp_headers <- ("location", location) :: c.resp_headers;
  c.status <- status;
  if c.state = Unset then (c.resp_body <- ""; c.state <- Set);
  c

(* ---- streamed answers (the server writes the body without buffering it) ---- *)

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

(* read a request header (case-insensitive); the first value if repeated *)
let req_header (c : t) (k : string) : string option =
  Fennec_core.Headers.get c.req.H.headers k

(* all values for a (possibly repeated) request header, in order *)
let req_headers (c : t) (k : string) : string list =
  Fennec_core.Headers.get_all c.req.H.headers k

let path (c : t) : string = c.req.H.path

(* the effective method — a method-override plug may have replaced it *)
let meth (c : t) : H.meth = match c.meth_override with Some m -> m | None -> c.req.H.meth

(* override the effective method (used by a method-override plug) *)
let override_method (c : t) (m : H.meth) : t = c.meth_override <- Some m; c

(* ---- request metadata ---- *)
let host (c : t) : string = c.req.H.host
let scheme (c : t) : string = c.req.H.scheme
let remote_ip (c : t) : string option = c.req.H.remote_ip
let version (c : t) : string = c.req.H.version

(* ---- query params (parsed + percent-decoded lazily, cached) ---- *)
let query_params (c : t) : (string * string) list =
  match c.query_params with
  | Some p -> p
  | None ->
    let p = H.parse_query c.req.H.query_string in
    c.query_params <- Some p;
    p

(* the first value for query key [k], if present *)
let query (c : t) (k : string) : string option = List.assoc_opt k (query_params c)

(* ---- request cookies (parsed lazily, cached) ---- *)
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

(* ---- body params (form fields) + file uploads, parsed lazily by content type ---- *)
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

(* ---- path params (captured by a :param / *splat route) ---- *)
let path_params (c : t) : (string * string) list = c.path_params
let path_param (c : t) (k : string) : string option = List.assoc_opt k c.path_params
let set_path_params (c : t) (ps : (string * string) list) : t = c.path_params <- ps; c

(* the value for [k], checked in order: path param, then query string, then form body *)
let param (c : t) (k : string) : string option =
  match List.assoc_opt k c.path_params with
  | Some v -> Some v
  | None -> ( match query c k with Some v -> Some v | None -> body_param c k)

let files (c : t) : Fennec_core.Multipart.part list = ensure_body c; Option.value c.files ~default:[]
let file (c : t) (name : string) : Fennec_core.Multipart.part option =
  List.find_opt (fun (p : Fennec_core.Multipart.part) -> p.name = name) (files c)

(* ---- response cookies (a Set-Cookie header; does NOT answer) ---- *)
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
