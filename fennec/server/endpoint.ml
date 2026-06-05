(* An Endpoint is an app's IDENTITY — a [name] plus the host pattern(s) it answers — and its
   BEHAVIOR (a two-phase paw pipeline). Ports live nowhere here: the runtime routes by Host in
   prod (see {!Host_router}) and assigns localhost ports in dev (see {!Port_plan}).

   Two pipeline phases prevent the "404 becomes 401" bug class:
   - ALWAYS paws: run on every request, matched or not. Logger, CORS, security headers, static
     file serving, route verbs, and SSR app mounts belong here.
   - MATCHED paws: run ONLY when an always-phase paw answered the conn (i.e. a route matched).
     Auth, rate limiting, and other business middleware belong here — they should never fire on
     a request that didn't match any route.

   For simple apps (no pipe_matched), the matched list is empty and behavior is identical to a
   flat pipeline — zero DX cost for the common case. *)

module Paw = Fennec_paw.Paw
module Conn = Fennec_paw.Conn
module H = Fennec_core.Http

type t = {
  name : string;
  hosts : string list;
  paws : Paw.t list; (* always-phase *)
  matched : Paw.t list; (* matched-phase: only runs when an always paw answered *)
}

let make ~name ?(hosts = [ "*" ]) () : t = { name; hosts; paws = []; matched = [] }

(* ---- always-phase (runs on every request) ---- *)

let use (p : Paw.t) (t : t) : t = { t with paws = t.paws @ [ p ] }
let pipe (paws : Paw.t list) (t : t) : t = List.fold_left (Fun.flip use) t paws
let prepend (p : Paw.t) (t : t) : t = { t with paws = p :: t.paws }

let get path h t = use (Paw.get path h) t
let post path h t = use (Paw.post path h) t
let put path h t = use (Paw.put path h) t
let delete path h t = use (Paw.delete path h) t
let patch path h t = use (Paw.patch path h) t

let app ?(at = "/") (render : string -> string option) (t : t) : t =
  let prefix_ok path = at = "/" || path = at || (String.length path > String.length at && String.sub path 0 (String.length at) = at) in
  use (fun c -> if (Conn.meth c = H.GET || Conn.meth c = H.HEAD) && prefix_ok (Conn.path c) then (match render (Conn.path c) with Some html -> Conn.html c html | None -> c) else c) t

(* ---- matched-phase (runs only after a route matched) ---- *)

let use_matched (p : Paw.t) (t : t) : t = { t with matched = t.matched @ [ p ] }
let pipe_matched (paws : Paw.t list) (t : t) : t = List.fold_left (Fun.flip use_matched) t paws

(* ---- composition ---- *)

let handler (t : t) : Paw.t =
  let always = Paw.seq t.paws in
  match t.matched with
  | [] -> always (* no matched-phase paws: flat pipeline, zero overhead *)
  | matched_paws ->
    (* the matched phase runs UNCONDITIONALLY on the (already-answered) conn — it's
       post-processing (auth checks, header stamps, logging), not route matching. We use a
       plain fold, not Paw.seq (which short-circuits on answered and would skip them). *)
    fun conn ->
      let c = always conn in
      if Conn.answered c then List.fold_left (fun c p -> p c) c matched_paws else c

let name (t : t) : string = t.name
let hosts (t : t) : string list = t.hosts
