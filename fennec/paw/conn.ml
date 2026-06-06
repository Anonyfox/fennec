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

(* ──── helpers ──── *)

let contains_ hay sub =
  let n = String.length hay and m = String.length sub in
  let rec go i = i + m <= n && (String.sub hay i m = sub || go (i + 1)) in
  m = 0 || go 0

let req_ ?(meth = H.GET) path = H.make_request ~meth ~path ()

(* ──── conn basics ──── *)

let%test "fresh not answered" =
  not (answered (make (req_ "/x")))

let%test "text answers" =
  answered (text (make (req_ "/x")) "hi")

let%test_unit "text body" =
  let c = text (make (req_ "/x")) "hi" in
  Fennec_hunt_unit.check_eq "text body"
    ~expected:"hi" ~got:(match resp c with Some r -> r.H.body | None -> "")

let%test "json status" =
  let c = json ~status:201 (make (req_ "/y")) "{}" in
  (match resp c with Some r -> r.H.status | None -> 0) = 201

let%test "explicit halt answers" =
  answered (halt (make (req_ "/z")))

let%test "conn assign/get" =
  let k : int Assigns.key = Assigns.key "k" in
  let c = assign (make (req_ "/")) k 5 in
  get c k = Some 5

let%test "req_header ci" =
  let c = make { (req_ "/") with H.headers = [ ("X-Foo", "bar") ] } in
  req_header c "x-foo" = Some "bar"

(* ──── query params ──── *)

let%test "conn query decoded" =
  let c = make (H.make_request ~meth:H.GET ~path:"/s" ~query_string:"q=a+b&n=2" ()) in
  query c "q" = Some "a b"

let%test "conn query other" =
  let c = make (H.make_request ~meth:H.GET ~path:"/s" ~query_string:"q=a+b&n=2" ()) in
  query c "n" = Some "2"

let%test "conn query missing" =
  let c = make (H.make_request ~meth:H.GET ~path:"/s" ~query_string:"q=a+b&n=2" ()) in
  query c "z" = None

(* ──── request metadata ──── *)

let%test_unit "conn host" =
  let c = make (H.make_request ~meth:H.GET ~path:"/" ~host:"example.com" ~scheme:"https"
                  ~remote_ip:(Some "1.2.3.4") ()) in
  Fennec_hunt_unit.check_eq "conn host" ~expected:"example.com" ~got:(host c)

let%test_unit "conn scheme" =
  let c = make (H.make_request ~meth:H.GET ~path:"/" ~host:"example.com" ~scheme:"https"
                  ~remote_ip:(Some "1.2.3.4") ()) in
  Fennec_hunt_unit.check_eq "conn scheme" ~expected:"https" ~got:(scheme c)

let%test "conn remote_ip" =
  let c = make (H.make_request ~meth:H.GET ~path:"/" ~host:"example.com" ~scheme:"https"
                  ~remote_ip:(Some "1.2.3.4") ()) in
  remote_ip c = Some "1.2.3.4"

(* ──── cookies ──── *)

let%test "conn cookie read" =
  let c = make { (req_ "/") with H.headers = [ ("Cookie", "sid=abc; theme=dark") ] } in
  cookie c "sid" = Some "abc"

let%test "conn cookie other" =
  let c = make { (req_ "/") with H.headers = [ ("Cookie", "sid=abc; theme=dark") ] } in
  cookie c "theme" = Some "dark"

let%test "conn cookie missing" =
  let c = make { (req_ "/") with H.headers = [ ("Cookie", "sid=abc; theme=dark") ] } in
  cookie c "nope" = None

(* ──── response cookies ──── *)

let%test "set_cookie does not answer" =
  not (answered (set_cookie (make (req_ "/")) "sid" "xyz"))

let%test "one Set-Cookie emitted" =
  let c = json (set_cookie (make (req_ "/")) "sid" "xyz") "{}" in
  let setcs = match resp c with
    | Some r -> Fennec_core.Headers.get_all r.H.headers "set-cookie"
    | None -> []
  in
  List.length setcs = 1

let%test "Set-Cookie carries the value" =
  let c = json (set_cookie (make (req_ "/")) "sid" "xyz") "{}" in
  let setcs = match resp c with
    | Some r -> Fennec_core.Headers.get_all r.H.headers "set-cookie"
    | None -> []
  in
  match setcs with [ s ] -> contains_ s "sid=xyz" | _ -> false

(* ──── form body params ──── *)

let%test "body_param decoded" =
  let c = make (H.make_request ~meth:H.POST ~path:"/"
                  ~headers:[ ("content-type", "application/x-www-form-urlencoded") ]
                  ~body:"a=1&b=hello+world" ()) in
  body_param c "b" = Some "hello world"

let%test "param falls back to the body" =
  let c = make (H.make_request ~meth:H.POST ~path:"/"
                  ~headers:[ ("content-type", "application/x-www-form-urlencoded") ]
                  ~body:"a=1&b=hello+world" ()) in
  param c "a" = Some "1"

let%test "param prefers the query string" =
  let c = make (H.make_request ~meth:H.POST ~path:"/" ~query_string:"a=q"
                  ~headers:[ ("content-type", "application/x-www-form-urlencoded") ] ~body:"a=b" ()) in
  param c "a" = Some "q"

(* ──── multipart file upload ──── *)

let%test_unit "uploaded file" =
  let mp =
    "--B\r\nContent-Disposition: form-data; name=\"f\"; filename=\"x.txt\"\r\n\
     Content-Type: text/plain\r\n\r\nDATA\r\n--B--\r\n"
  in
  let c = make (H.make_request ~meth:H.POST ~path:"/"
                  ~headers:[ ("content-type", "multipart/form-data; boundary=B") ] ~body:mp ()) in
  match file c "f" with
  | Some p ->
    Fennec_hunt_unit.check "uploaded filename" (p.Fennec_core.Multipart.filename = Some "x.txt");
    Fennec_hunt_unit.check_eq "uploaded data" ~expected:"DATA" ~got:p.Fennec_core.Multipart.data
  | None -> Fennec_hunt_unit.check "uploaded file present" false

(* ──── build-vs-answer + state ──── *)

let%test "set_header does not answer" =
  not (answered (set_header (make (req_ "/")) "X-A" "1"))

let%test "answered after json" =
  answered (json (set_header (make (req_ "/")) "X-A" "1") "{}")

let%test "pre-set header preserved through the answer" =
  let c = json (set_header (make (req_ "/")) "X-A" "1") "{}" in
  let hdrs = match resp c with Some r -> r.H.headers | None -> [] in
  List.mem ("X-A", "1") hdrs

let%test "answer's content-type merged in too" =
  let c = json (set_header (make (req_ "/")) "X-A" "1") "{}" in
  let hdrs = match resp c with Some r -> r.H.headers | None -> [] in
  List.exists (fun (k, _) -> String.lowercase_ascii k = "content-type") hdrs

let%test "before_send FIFO order" =
  let order = ref [] in
  let c = make (req_ "/") in
  let c = before_send c (fun r -> order := "a" :: !order; r) in
  let c = before_send c (fun r -> order := "b" :: !order; r) in
  let c = text c "x" in
  let _ = apply_before_send c (Option.value (resp c) ~default:(H.text "")) in
  List.rev !order = [ "a"; "b" ]

let%test "status answers" =
  answered (set_status 204 (make (req_ "/")))

let%test "status code" =
  let c = set_status 204 (make (req_ "/")) in
  (match resp c with Some r -> r.H.status | None -> 0) = 204

let%test "halt answers" =
  answered (halt (make (req_ "/")))

let%test "halt has no response" =
  resp (halt (make (req_ "/"))) = None

let%test "header added post-answer is present" =
  let c = set_header (text (make (req_ "/")) "body") "X-Late" "y" in
  match resp c with Some r -> List.mem ("X-Late", "y") r.H.headers | None -> false

let%test "exactly one content-type after answer" =
  let c = json (set_header (make (req_ "/")) "content-type" "text/plain") "{}" in
  let cts = match resp c with
    | Some r -> List.filter (fun (k, _) -> String.lowercase_ascii k = "content-type") r.H.headers
    | None -> []
  in
  List.length cts = 1

let%test "the answerer's content-type wins" =
  let c = json (set_header (make (req_ "/")) "content-type" "text/plain") "{}" in
  let cts = match resp c with
    | Some r -> List.filter (fun (k, _) -> String.lowercase_ascii k = "content-type") r.H.headers
    | None -> []
  in
  List.assoc_opt "content-type" cts = Some "application/json"

(* ──── full surface ──── *)

let%test_unit "req returns the request" =
  let base = H.make_request ~meth:H.GET ~path:"/p" ~query_string:"a=1" ~host:"h" ~scheme:"https"
               ~remote_ip:(Some "1.2.3.4") ~version:"HTTP/1.1"
               ~headers:[ ("X-M", "a"); ("X-M", "b"); ("Cookie", "c=1") ] () in
  Fennec_hunt_unit.check_eq "req returns the request" ~expected:"/p" ~got:(req (make base)).H.path

let%test_unit "path" =
  let base = H.make_request ~meth:H.GET ~path:"/p" () in
  Fennec_hunt_unit.check_eq "path" ~expected:"/p" ~got:(path (make base))

let%test_unit "version" =
  let base = H.make_request ~meth:H.GET ~path:"/p" ~version:"HTTP/1.1" () in
  Fennec_hunt_unit.check_eq "version" ~expected:"HTTP/1.1" ~got:(version (make base))

let%test "req_headers (all values)" =
  let base = H.make_request ~meth:H.GET ~path:"/p"
               ~headers:[ ("X-M", "a"); ("X-M", "b") ] () in
  req_headers (make base) "x-m" = [ "a"; "b" ]

let%test "query_params (list)" =
  let base = H.make_request ~meth:H.GET ~path:"/p" ~query_string:"a=1" () in
  query_params (make base) = [ ("a", "1") ]

let%test "cookies (list)" =
  let base = H.make_request ~meth:H.GET ~path:"/p"
               ~headers:[ ("Cookie", "c=1") ] () in
  cookies (make base) = [ ("c", "1") ]

let%test "body_params (list)" =
  let c = make (H.make_request ~meth:H.POST ~path:"/"
                  ~headers:[ ("content-type", "application/x-www-form-urlencoded") ] ~body:"k=v" ()) in
  body_params c = [ ("k", "v") ]

let%test "files (list, none)" =
  let c = make (H.make_request ~meth:H.POST ~path:"/"
                  ~headers:[ ("content-type", "application/x-www-form-urlencoded") ] ~body:"k=v" ()) in
  files c = []

let%test "set_path_params + path_params" =
  let c = set_path_params (make (req_ "/")) [ ("id", "7") ] in
  path_params c = [ ("id", "7") ]

let%test "get_exn returns the value" =
  let k : int Assigns.key = Assigns.key "n" in
  get_exn (assign (make (req_ "/")) k 9) k = 9

let%test "get_exn raises on a missing key" =
  let k : int Assigns.key = Assigns.key "n" in
  try ignore (get_exn (make (req_ "/")) k); false with Invalid_argument _ -> true

let%test "override_method changes the effective method" =
  let c = override_method (make (req_ "/")) H.DELETE in
  meth c = H.DELETE

let%test "respond sets status + body" =
  let c = respond (make (req_ "/")) (H.text ~status:418 "teapot") in
  (match resp c with Some r -> (r.H.status, r.H.body) | None -> (0, "")) = (418, "teapot")

let%test "resp_skeleton keeps headers, empties the body" =
  let c = set_header (make (req_ "/")) "X-S" "1" in
  let sk = resp_skeleton c in
  sk.H.body = "" && List.mem ("X-S", "1") sk.H.headers

let%test "redirect: 301 + Location + answered" =
  let c = redirect ~status:301 (make (req_ "/")) "/new" in
  answered c
  && (match resp c with Some r -> r.H.status = 301 && List.mem ("location", "/new") r.H.headers | None -> false)

let%test "delete_cookie expires the cookie (Max-Age=0)" =
  let c = text (delete_cookie (make (req_ "/")) "sid") "x" in
  match resp c with
  | Some r ->
    (match Fennec_core.Headers.get_all r.H.headers "set-cookie" with
     | [ s ] -> contains_ s "Max-Age=0"
     | _ -> false)
  | None -> false

let%test "upgrade: handler set, answered, no buffered resp" =
  let c = upgrade (make (req_ "/")) (fun _ -> ()) in
  upgrade_handler c <> None && answered c && resp c = None

let%test "send_file sets a File stream + answers" =
  let c = send_file (make (req_ "/")) ~path:"/tmp/x.txt" () in
  answered c && (match stream c with Some (File (p, _)) -> p = "/tmp/x.txt" | _ -> false)

let%test_unit "send_chunked content-type" =
  let c = send_chunked (make (req_ "/")) ~content_type:"text/event-stream" (fun emit -> emit "a"; emit "b") in
  match stream c with
  | Some (Chunked (ct, _produce)) ->
    Fennec_hunt_unit.check_eq "send_chunked content-type" ~expected:"text/event-stream" ~got:ct
  | _ -> Fennec_hunt_unit.check "send_chunked sets a Chunked stream" false

let%test "send_chunked producer emits in order" =
  let c = send_chunked (make (req_ "/")) ~content_type:"text/event-stream" (fun emit -> emit "a"; emit "b") in
  let got = ref [] in
  (match stream c with
   | Some (Chunked (_ct, produce)) -> produce (fun s -> got := s :: !got)
   | _ -> ());
  List.rev !got = [ "a"; "b" ]
