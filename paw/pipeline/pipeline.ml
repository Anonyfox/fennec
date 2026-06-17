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
   higher-level batteries (logger, auth, …) live in their own modules under the [Paw]
   namespace ([Paw.Logger], [Paw.Session], …), each a [make] returning one of these paws. *)

module H = Http

(* ============================ the primitive + its algebra =================== *)

type t = Conn.t -> Conn.t

(* walk the paws, applying each only while the conn is unanswered. A top-level recursive function (not
   a [List.fold_left] with an inline closure) so running a compiled pipeline allocates no closure per
   request — only the [seq] wrapper, built once. *)
let rec run_seq c = function [] -> c | p :: rest -> if Conn.answered c then c else run_seq (p c) rest

(* compose paws left-to-right into one paw; each runs only while the conn is unanswered, so
   order is precedence and the first to answer wins *)
let seq (paws : t list) : t = fun c -> run_seq c paws

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

(* a method+path route; [h] is run when it matches. A pattern with [:name]/[*name] captures path
   params onto the conn (read with {!Conn.path_param}); a plain pattern is an exact string match.
   The matching rules live in {!Route_match} (shared with the compiled {!Route_table}); the pattern's
   shape — whether it has params, and (if so) its segments — is computed ONCE here at construction,
   never per request, so dispatch is a method check plus either one string compare (static) or a
   segment match (dynamic), with no re-scan of the pattern on the hot path. *)
let on (m : H.meth) (pattern : string) (h : t) : t =
  if Route_match.has_params pattern then
    let pat_segs = Route_match.segments pattern in
    fun c ->
      if not (Route_match.meth_matches m (Conn.meth c)) then c
      else match Route_match.match_segments pat_segs (Route_match.segments (Conn.path c)) [] with Some ps -> h (Conn.set_path_params c ps) | None -> c
  else fun c -> if Route_match.meth_matches m (Conn.meth c) && Conn.path c = pattern then h c else c

let get path h = on H.GET path h
let post path h = on H.POST path h
let put path h = on H.PUT path h
let delete path h = on H.DELETE path h
let patch path h = on H.PATCH path h

(* a fallthrough paw from a [request -> response option] (e.g. static files): answers when
   it yields Some, else declines *)
let fallthrough (f : H.request -> H.response option) : t =
 fun c -> match f (Conn.req c) with Some r -> Conn.respond c r | None -> c

(* ──── helpers ──── *)

let req_ ?(meth = H.GET) path = H.make_request ~meth ~path ()

(* ──── pipeline short-circuit ──── *)

let%test_unit "answered body" =
  let hits = ref [] in
  let tap name : t = fun c -> hits := name :: !hits; c in
  let answer : t = fun c -> Conn.text c "answered" in
  let p = seq [ tap "a"; answer; tap "b" ] in
  let r = run p (req_ "/") in
  Fennec_hunt_unit.check_eq "answered body" ~expected:"answered" ~got:r.H.body

let%test "downstream skipped after answer" =
  let hits = ref [] in
  let tap name : t = fun c -> hits := name :: !hits; c in
  let answer : t = fun c -> Conn.text c "answered" in
  let _ = run (seq [ tap "a"; answer; tap "b" ]) (req_ "/") in
  List.rev !hits = [ "a" ]

let%test "empty pipeline 404" =
  (run (seq []) (req_ "/")).H.status = 404

let%test "all-decline 404" =
  let tap _name : t = fun c -> c in
  (run (seq [ tap "x"; tap "y" ]) (req_ "/")).H.status = 404

(* ──── route matching ──── *)

let%test_unit "GET route" =
  let app = seq
    [ get "/api/ping" (fun c -> Conn.json c {|{"pong":true}|});
      post "/api/ping" (fun c -> Conn.text c "posted");
      get "/" (fun c -> Conn.html c "<h1>home</h1>") ] in
  Fennec_hunt_unit.check_eq "GET route"
    ~expected:{|{"pong":true}|} ~got:(run app (req_ "/api/ping")).H.body

let%test_unit "POST route (same path, diff method)" =
  let app = seq
    [ get "/api/ping" (fun c -> Conn.json c {|{"pong":true}|});
      post "/api/ping" (fun c -> Conn.text c "posted");
      get "/" (fun c -> Conn.html c "<h1>home</h1>") ] in
  Fennec_hunt_unit.check_eq "POST route"
    ~expected:"posted" ~got:(run app (req_ ~meth:H.POST "/api/ping")).H.body

let%test_unit "HEAD matches GET" =
  let app = seq
    [ get "/" (fun c -> Conn.html c "<h1>home</h1>") ] in
  Fennec_hunt_unit.check_eq "HEAD matches GET"
    ~expected:"<h1>home</h1>" ~got:(run app (req_ ~meth:H.HEAD "/")).H.body

let%test "no match -> 404" =
  let app = seq
    [ get "/api/ping" (fun c -> Conn.json c {|{"pong":true}|}) ] in
  (run app (req_ "/nope")).H.status = 404

let%test "wrong method -> 404" =
  let app = seq
    [ get "/api/ping" (fun c -> Conn.json c {|{"pong":true}|}) ] in
  (run app (req_ ~meth:H.DELETE "/api/ping")).H.status = 404

let%test_unit "fallthrough hit" =
  let ft = fallthrough (fun r -> if r.H.path = "/f" then Some (H.text "F") else None) in
  Fennec_hunt_unit.check_eq "fallthrough hit"
    ~expected:"F" ~got:(run (seq [ ft ]) (req_ "/f")).H.body

let%test "fallthrough miss -> 404" =
  let ft = fallthrough (fun r -> if r.H.path = "/f" then Some (H.text "F") else None) in
  (run (seq [ ft ]) (req_ "/g")).H.status = 404

(* ──── path params ──── *)

let%test_unit "captures :id" =
  let app = seq
    [ get "/users/:id" (fun c -> Conn.text c (Option.value (Conn.path_param c "id") ~default:"?")) ] in
  Fennec_hunt_unit.check_eq "captures :id"
    ~expected:"42" ~got:(run app (req_ "/users/42")).H.body

let%test_unit "splat captures the rest" =
  let app = seq
    [ get "/files/*rest" (fun c -> Conn.text c (Option.value (Conn.path_param c "rest") ~default:"?")) ] in
  Fennec_hunt_unit.check_eq "splat captures the rest"
    ~expected:"a/b/c.txt" ~got:(run app (req_ "/files/a/b/c.txt")).H.body

let%test_unit "two params" =
  let app = seq
    [ get "/a/:x/b/:y" (fun c ->
        Conn.text c (Option.value (Conn.param c "x") ~default:"?" ^ "-"
                    ^ Option.value (Conn.param c "y") ~default:"?")) ] in
  Fennec_hunt_unit.check_eq "two params"
    ~expected:"1-2" ~got:(run app (req_ "/a/1/b/2")).H.body

let%test "param count mismatch -> 404" =
  let app = seq
    [ get "/users/:id" (fun c -> Conn.text c (Option.value (Conn.path_param c "id") ~default:"?")) ] in
  (run app (req_ "/users/42/extra")).H.status = 404

let%test "path param no match -> 404" =
  let app = seq
    [ get "/users/:id" (fun c -> Conn.text c (Option.value (Conn.path_param c "id") ~default:"?")) ] in
  (run app (req_ "/nope")).H.status = 404

let%test_unit "path param beats query in param" =
  let app = seq
    [ get "/p/:id" (fun c -> Conn.text c (Option.value (Conn.param c "id") ~default:"?")) ] in
  Fennec_hunt_unit.check_eq "path param beats query in param"
    ~expected:"path"
    ~got:(run app (H.make_request ~meth:H.GET ~path:"/p/path" ~query_string:"id=query" ())).H.body
