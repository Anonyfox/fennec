(* Paw — the one primitive. A [paw] touches a connection: [conn -> conn]. It
   either passes the conn through (declining) or answers it (sets a response,
   which short-circuits the rest). Middleware, an API route, static serving, the
   websocket upgrade, the whole SSR app — ALL are paws. A pipeline is a list of
   paws folded into one. (Named for the desert fox: it touches connections,
   sometimes repeatedly.)

   This is Elixir/Plug's model with one refinement: an answered conn auto-skips
   downstream paws, so a pipeline reads as a clean top-to-bottom |> chain and
   precedence is just order. *)

type t = Conn.t -> Conn.t

(* run a paw only if the conn isn't already answered *)
let guarded (p : t) : t = fun c -> if Conn.answered c then c else p c

(* compose paws left-to-right into one paw; each is auto-guarded so order is
   precedence and the first to answer wins *)
let seq (paws : t list) : t =
 fun c -> List.fold_left (fun c p -> if Conn.answered c then c else p c) c paws

(* the identity paw (declines everything) *)
let pass : t = fun c -> c

(* run a pipeline over a request, returning the final conn (the server inspects it
   for an HTTP response or a websocket upgrade) *)
let run_conn (p : t) (req : Fennec_core.Http.request) : Conn.t = p (Conn.make req)

(* run a pipeline to an HTTP response (an unanswered conn becomes a 404). Handy
   for pure tests of HTTP-answering pipelines. *)
let run (p : t) (req : Fennec_core.Http.request) : Fennec_core.Http.response =
  match Conn.resp (run_conn p req) with
  | Some r -> r
  | None -> Fennec_core.Http.text ~status:404 "404 Not Found"
