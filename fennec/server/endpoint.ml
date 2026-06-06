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

let req_ ?(meth = H.GET) path = H.make_request ~meth ~path ()

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

(* ──── always-phase tests ──── *)

let%test "api route" =
  let e = make ~name:"app" ~hosts:[ "app.example.com" ] ()
          |> get "/api/health" (fun c -> Conn.json c {|{"ok":true}|})
          |> get "/" (fun c -> Conn.html c "<h1>home</h1>") in
  (Paw.run (handler e) (req_ "/api/health")).H.body = {|{"ok":true}|}

let%test "home route" =
  let e = make ~name:"app" ~hosts:[ "app.example.com" ] ()
          |> get "/api/health" (fun c -> Conn.json c {|{"ok":true}|})
          |> get "/" (fun c -> Conn.html c "<h1>home</h1>") in
  (Paw.run (handler e) (req_ "/")).H.body = "<h1>home</h1>"

let%test "unmatched 404" =
  let e = make ~name:"app" ~hosts:[ "app.example.com" ] ()
          |> get "/api/health" (fun c -> Conn.json c {|{"ok":true}|})
          |> get "/" (fun c -> Conn.html c "<h1>home</h1>") in
  (Paw.run (handler e) (req_ "/nope")).H.status = 404

let%test "name is carried" =
  let e = make ~name:"app" ~hosts:[ "app.example.com" ] () in
  name e = "app"

let%test "hosts are carried" =
  let e = make ~name:"app" ~hosts:[ "app.example.com" ] () in
  hosts e = [ "app.example.com" ]

let%test "guard halts" =
  let guard : Paw.t = fun c -> if Conn.path c = "/blocked" then Conn.text ~status:403 c "no" else c in
  let e2 = make ~name:"guarded" () |> use guard
           |> get "/blocked" (fun c -> Conn.text c "should-not-reach")
           |> get "/ok" (fun c -> Conn.text c "ok") in
  (Paw.run (handler e2) (req_ "/blocked")).H.status = 403

let%test "guard passes others" =
  let guard : Paw.t = fun c -> if Conn.path c = "/blocked" then Conn.text ~status:403 c "no" else c in
  let e2 = make ~name:"guarded" () |> use guard
           |> get "/blocked" (fun c -> Conn.text c "should-not-reach")
           |> get "/ok" (fun c -> Conn.text c "ok") in
  (Paw.run (handler e2) (req_ "/ok")).H.body = "ok"

let%test "hosts default to the catch-all" =
  let e2 = make ~name:"guarded" () in
  hosts e2 = [ "*" ]

(* ──── matched-phase tests (the 404-stays-404 property) ──── *)

let%test "matched route -> auth runs, gets 401" =
  let auth_paw : Paw.t = fun c -> Conn.text ~status:401 c "unauthorized" in
  let e3 = make ~name:"secured" ()
           |> get "/api/secret" (fun c -> Conn.text c "top secret")
           |> pipe_matched [ auth_paw ] in
  (Paw.run (handler e3) (req_ "/api/secret")).H.status = 401

let%test_unit "auth DID run on a matched route" =
  let auth_ran = ref false in
  let auth_paw : Paw.t = fun c -> auth_ran := true; Conn.text ~status:401 c "unauthorized" in
  let e3 = make ~name:"secured" ()
           |> get "/api/secret" (fun c -> Conn.text c "top secret")
           |> pipe_matched [ auth_paw ] in
  let _ = Paw.run (handler e3) (req_ "/api/secret") in
  Fennec_hunt_unit.check "auth ran" !auth_ran

let%test "unmatched -> 404 (not 401 from auth)" =
  let auth_paw : Paw.t = fun c -> Conn.text ~status:401 c "unauthorized" in
  let e3 = make ~name:"secured" ()
           |> get "/api/secret" (fun c -> Conn.text c "top secret")
           |> pipe_matched [ auth_paw ] in
  (Paw.run (handler e3) (req_ "/nonexistent")).H.status = 404

let%test_unit "auth did NOT run on an unmatched route" =
  let auth_ran = ref false in
  let auth_paw : Paw.t = fun c -> auth_ran := true; Conn.text ~status:401 c "unauthorized" in
  let e3 = make ~name:"secured" ()
           |> get "/api/secret" (fun c -> Conn.text c "top secret")
           |> pipe_matched [ auth_paw ] in
  auth_ran := false;
  let _ = Paw.run (handler e3) (req_ "/nonexistent") in
  Fennec_hunt_unit.check "auth did not run" (not !auth_ran)

let%test_unit "matched route gets the header stamp" =
  let stamp : Paw.t = fun c -> Conn.before_send c (fun r -> { r with H.headers = ("X-Auth", "ok") :: r.H.headers }) in
  let e4 = make ~name:"stamped" ()
           |> get "/api/data" (fun c -> Conn.text c "data")
           |> pipe_matched [ stamp ] in
  let conn4 = Paw.run_conn (handler e4) (req_ "/api/data") in
  let resp4 = Conn.apply_before_send conn4 (Option.get (Conn.resp conn4)) in
  Fennec_hunt_unit.check "X-Auth header stamp" (List.assoc_opt "X-Auth" resp4.H.headers = Some "ok")

(* ──── flat pipeline (backward compat) ──── *)

let%test "flat (no pipe_matched) still works" =
  let e5 = make ~name:"flat" () |> get "/ok" (fun c -> Conn.text c "ok") in
  (Paw.run (handler e5) (req_ "/ok")).H.body = "ok"

let%test "flat unmatched -> 404" =
  let e5 = make ~name:"flat" () |> get "/ok" (fun c -> Conn.text c "ok") in
  (Paw.run (handler e5) (req_ "/nope")).H.status = 404
