(* Paw — THE primitive. A [paw] touches a connection: [Conn.t -> Conn.t]. It either
   passes the conn through (declines) or answers it (sets a response, which short-circuits
   the rest). Middleware, a route, static serving, the websocket upgrade, the whole SSR app
   — ALL are paws. (Named for the desert fox: it touches connections, sometimes repeatedly.)

   This module is the connection's verb layer, in two halves:
   - the ALGEBRA — compose paws into a pipeline ({!seq}) and run a pipeline ({!run_conn} /
     {!run}); the identity is {!pass};
   - the CONSTRUCTORS — make a paw from raw matching: the route verbs ({!get}/{!post}/… and
     the general {!on}) and a [request -> response] {!fallthrough}. (Routes are not a
     separate concept — a route is just a paw guarded by method + path.)

   This is Plug's model with one refinement: an answered conn auto-skips downstream paws, so
   a pipeline reads as a clean top-to-bottom |> chain and precedence is just order. The
   higher-level batteries (logger, auth, …) live in {!Fennec_server.Plug}. *)

module H = Fennec_core.Http

(* ============================ the primitive + its algebra =================== *)

type t = Conn.t -> Conn.t

(* compose paws left-to-right into one paw; each runs only while the conn is unanswered, so
   order is precedence and the first to answer wins *)
let seq (paws : t list) : t =
 fun c -> List.fold_left (fun c p -> if Conn.answered c then c else p c) c paws

(* the identity paw (declines everything) — the unit of {!seq}; handy as a placeholder *)
let pass : t = fun c -> c

(* run a pipeline over a request, returning the final conn (the server inspects it for an
   HTTP response or a websocket upgrade) *)
let run_conn (p : t) (req : H.request) : Conn.t = p (Conn.make req)

(* run a pipeline to an HTTP response (an unanswered conn becomes a 404). Handy for pure
   tests of HTTP-answering pipelines. *)
let run (p : t) (req : H.request) : H.response =
  match Conn.resp (run_conn p req) with Some r -> r | None -> H.text ~status:404 "404 Not Found"

(* ============================ constructors ================================== *)

(* HEAD is matched as GET (the responder strips the body downstream) *)
let meth_matches (want : H.meth) (got : H.meth) =
  got = want || (want = H.GET && got = H.HEAD)

(* path -> non-empty segments (a leading/trailing/double slash is ignored) *)
let segments (s : string) : string list = String.split_on_char '/' s |> List.filter (fun x -> x <> "")

(* match a pattern with [:name] (one segment) and a trailing [*name] (the rest) against a
   path, returning the captured params, or [None] if it doesn't match *)
let match_pattern (pattern : string) (path : string) : (string * string) list option =
  let rec go ps xs acc =
    match (ps, xs) with
    | [], [] -> Some (List.rev acc)
    | [ p ], _ when String.length p > 0 && p.[0] = '*' ->
      Some (List.rev ((String.sub p 1 (String.length p - 1), String.concat "/" xs) :: acc))
    | p :: ps', x :: xs' ->
      if String.length p > 0 && p.[0] = ':' then
        go ps' xs' ((String.sub p 1 (String.length p - 1), x) :: acc)
      else if p = x then go ps' xs' acc
      else None
    | _ -> None
  in
  go (segments pattern) (segments path) []

let has_params (pattern : string) = String.contains pattern ':' || String.contains pattern '*'

(* a method+path route; [h] is run when it matches. A pattern with [:name]/[*name] captures
   path params onto the conn (read with {!Conn.path_param}); a plain pattern is an exact
   string match. *)
let on (m : H.meth) (pattern : string) (h : t) : t =
 fun c ->
  if not (meth_matches m (Conn.meth c)) then c
  else if has_params pattern then
    match match_pattern pattern (Conn.path c) with Some ps -> h (Conn.set_path_params c ps) | None -> c
  else if Conn.path c = pattern then h c
  else c

let get path h = on H.GET path h
let post path h = on H.POST path h
let put path h = on H.PUT path h
let delete path h = on H.DELETE path h
let patch path h = on H.PATCH path h

(* a fallthrough paw from a [request -> response option] (e.g. static files): answers when
   it yields Some, else declines *)
let fallthrough (f : H.request -> H.response option) : t =
 fun c -> match f (Conn.req c) with Some r -> Conn.respond c r | None -> c
