(* An Endpoint is an app's IDENTITY — a [name] plus the host pattern(s) it answers — and its
   BEHAVIOR (a paw pipeline). Ports live nowhere here: the runtime routes by Host in prod (see
   {!Host_router}) and assigns localhost ports in dev (see {!Port_plan}). A server runs many
   endpoints; one is selected per request by Host pattern (Phoenix-style), so a single process
   serves arbitrary subdomains/wildcards with no /etc/hosts or proxy.

   The builders (pipe/get/post/use/…) just append paws — an endpoint's handler is one composed paw. *)

module Paw = Fennec_paw.Paw
module Conn = Fennec_paw.Conn
module H = Fennec_core.Http

type t = {
  name : string; (* a stable handle — for the dev banner, tests, tooling (people + LLMs read names) *)
  hosts : string list; (* host PATTERNS this endpoint answers (validated by Host_router); default ["*"] *)
  paws : Paw.t list; (* the pipeline, in order *)
}

let make ~name ?(hosts = [ "*" ]) () : t = { name; hosts; paws = [] }

(* append a single paw — the one implementation path for both [use] and the verb shortcuts *)
let use (p : Paw.t) (t : t) : t = { t with paws = t.paws @ [ p ] }

(* mount a reusable pipeline (a paw list), defined in terms of [use] *)
let pipe (paws : Paw.t list) (t : t) : t = List.fold_left (Fun.flip use) t paws

(* prepend a paw so it runs BEFORE the rest of the pipeline. Needed for a paw that must register a
   before_send hook before an answering paw short-circuits the chain (e.g. the dev livereload
   script injector). *)
let prepend (p : Paw.t) (t : t) : t = { t with paws = p :: t.paws }

(* route verbs — each is a paw *)
let get path h t = use (Paw.get path h) t
let post path h t = use (Paw.post path h) t
let put path h t = use (Paw.put path h) t
let delete path h t = use (Paw.delete path h) t
let patch path h t = use (Paw.patch path h) t

(* Mount an SSR app: a [render : path -> string option] (the universal router's render) becomes a
   paw answering with an HTML document when it matches, else declining (so static/404 follow). Kept
   generic (a function, not a Router type) so fennec.server needn't depend on the heavy router libs. *)
let app ?(at = "/") (render : string -> string option) (t : t) : t =
  let prefix_ok path = at = "/" || path = at || (String.length path > String.length at && String.sub path 0 (String.length at) = at) in
  use (fun c -> if (Conn.meth c = H.GET || Conn.meth c = H.HEAD) && prefix_ok (Conn.path c) then (match render (Conn.path c) with Some html -> Conn.html c html | None -> c) else c) t

(* the composed handler paw for this endpoint *)
let handler (t : t) : Paw.t = Paw.seq t.paws

let name (t : t) : string = t.name
let hosts (t : t) : string list = t.hosts
