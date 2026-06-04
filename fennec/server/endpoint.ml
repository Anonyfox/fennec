(* An Endpoint binds a (host pattern, port) to a paw pipeline. A server runs many
   endpoints; in production they may share a port and are selected by Host pattern
   (Phoenix-style), so one process serves arbitrary subdomains/wildcards. In dev
   each endpoint also has a [dev_port] so every endpoint is reachable on localhost
   with no /etc/hosts or proxy.

   The builders (pipe/get/post/plug/…) just append paws — an endpoint's handler is
   one composed paw. *)

module Paw = Fennec_paw.Paw
module Conn = Fennec_paw.Conn
module H = Fennec_core.Http

type t = {
  host : string; (* pattern: "example.com" | "*.example.com" | "*" *)
  port : int; (* production port *)
  dev_port : int; (* localhost dev port *)
  paws : Paw.t list; (* the pipeline, in order *)
}

let make ?(host = "*") ?(port = 80) ?dev_port () : t =
  { host; port; dev_port = Option.value dev_port ~default:port; paws = [] }

let add (p : Paw.t) (t : t) : t = { t with paws = t.paws @ [ p ] }

(* mount a reusable pipeline (a paw list) *)
let pipe (paws : Paw.t list) (t : t) : t = { t with paws = t.paws @ paws }

(* a single paw (e.g. a prebuilt Plug.* ) *)
let plug (p : Paw.t) (t : t) : t = add p t

(* prepend a paw so it runs BEFORE the rest of the pipeline. Needed for a paw that
   must register a before_send hook before an answering paw short-circuits the
   chain (e.g. the dev livereload script injector). *)
let prepend (p : Paw.t) (t : t) : t = { t with paws = p :: t.paws }

(* route verbs — each is a paw *)
let get path h t = add (Paw.get path h) t
let post path h t = add (Paw.post path h) t
let put path h t = add (Paw.put path h) t
let delete path h t = add (Paw.delete path h) t
let patch path h t = add (Paw.patch path h) t

(* Mount an SSR app: a [render : path -> string option] (the universal router's
   render) becomes a paw answering with an HTML document when it matches, else
   declining (so static/404 follow). Kept generic (a function, not a Router type)
   so fennec.server needn't depend on the heavy router/react libs. *)
let app ?(at = "/") (render : string -> string option) (t : t) : t =
  let prefix_ok path =
    at = "/" || path = at
    || (String.length path > String.length at && String.sub path 0 (String.length at) = at)
  in
  add
    (fun c ->
      if (Conn.meth c = H.GET || Conn.meth c = H.HEAD) && prefix_ok (Conn.path c) then
        match render (Conn.path c) with Some html -> Conn.html c html | None -> c
      else c)
    t

(* the composed handler paw for this endpoint *)
let handler (t : t) : Paw.t = Paw.seq t.paws

(* the port this endpoint listens on for a given mode *)
let listen_port ~dev (t : t) : int = if dev then t.dev_port else t.port

let host_matches (t : t) (host : string) : bool = Host.matches ~pattern:t.host host
