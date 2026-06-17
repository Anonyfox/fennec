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

module Conn = Conn
module H = Http

(* An endpoint's always-phase is a sequence of ITEMS in declaration order: opaque middleware paws
   (use / app / static / a guard) and DESCRIBED routes (the verbs). Keeping routes described — not
   pre-wrapped into opaque paws — lets {!handler} compile each contiguous run of them into one O(1)
   {!Route_table} at serve, instead of a per-request linear scan, with no change to the verb DX. *)
type item = Mw of Pipeline.t | Route of Route_table.route

type t = {
  name : string;
  hosts : string list;
  items : item list; (* always-phase, declaration order *)
  matched : Pipeline.t list; (* matched-phase: only runs when an always-phase item answered *)
}

let make ~name ?(hosts = [ "*" ]) () : t = { name; hosts; items = []; matched = [] }

let req_ ?(meth = H.GET) path = H.make_request ~meth ~path ()

(* ---- always-phase (runs on every request) ---- *)

let use (p : Pipeline.t) (t : t) : t = { t with items = t.items @ [ Mw p ] }
let pipe (paws : Pipeline.t list) (t : t) : t = List.fold_left (Fun.flip use) t paws
let prepend (p : Pipeline.t) (t : t) : t = { t with items = Mw p :: t.items }

(* a described route — recorded, not wrapped — so {!handler} can compile a run of them into a table *)
let route meth path (h : Pipeline.t) (t : t) : t = { t with items = t.items @ [ Route { Route_table.meth; path; handler = h } ] }
let get path h t = route H.GET path h t
let post path h t = route H.POST path h t
let put path h t = route H.PUT path h t
let delete path h t = route H.DELETE path h t
let patch path h t = route H.PATCH path h t

(* mount a server-rendered FORM handler at [path] in one call: its [serve] dispatches GET (render the
   form) vs POST (validate -> redirect or re-render) internally, so a single registration covers both
   verbs. Pair with Session + Csrf paws for flash + token. *)
let form path h t = t |> get path h |> post path h

let app ?(at = "/") (render : string -> string option) (t : t) : t =
  let prefix_ok path = at = "/" || path = at || (String.length path > String.length at && String.sub path 0 (String.length at) = at) in
  use (fun c -> if (Conn.meth c = H.GET || Conn.meth c = H.HEAD) && prefix_ok (Conn.path c) then (match render (Conn.path c) with Some html -> Conn.html c html | None -> c) else c) t

(* ---- matched-phase (runs only after a route matched) ---- *)

let use_matched (p : Pipeline.t) (t : t) : t = { t with matched = t.matched @ [ p ] }
let pipe_matched (paws : Pipeline.t list) (t : t) : t = List.fold_left (Fun.flip use_matched) t paws

(* ---- composition (compiled ONCE at serve, not per request) ---- *)

(* Fold the items into a paw list, collapsing each MAXIMAL RUN of described routes into one compiled
   {!Route_table} dispatch paw and leaving middleware paws in place. Ordering is preserved exactly —
   middleware declared after a route still only runs on fall-through — while a run of routes
   dispatches in O(1). The common shape (all middleware, then all routes) becomes [mw; …; one table]. *)
let compile_items (items : item list) : Pipeline.t =
  let paws = ref [] and run = ref [] in
  let flush () = match !run with [] -> () | rs -> paws := Route_table.dispatch (Route_table.build (List.rev rs)) :: !paws; run := [] in
  List.iter (function Route r -> run := r :: !run | Mw p -> flush (); paws := p :: !paws) items;
  flush ();
  Pipeline.seq (List.rev !paws)

(* matched-phase paws run UNCONDITIONALLY (post-processing) on the already-answered conn — a plain
   walk, NOT a short-circuiting seq (which would skip them). Top-level: no per-request closure. *)
let rec run_all c = function [] -> c | p :: rest -> run_all (p c) rest

let handler (t : t) : Pipeline.t =
  let always = compile_items t.items in
  match t.matched with [] -> always | matched_paws -> fun conn -> let c = always conn in if Conn.answered c then run_all c matched_paws else c

(* duplicate (method, exact path) route declarations anywhere in the endpoint — for fail-at-boot
   validation by {!Fennec.serve} / {!Paw.serve}; [] when the endpoint is clean *)
let conflicts (t : t) : string list = Route_table.conflicts (List.filter_map (function Route r -> Some r | Mw _ -> None) t.items)

let name (t : t) : string = t.name
let hosts (t : t) : string list = t.hosts

(* ──── always-phase tests ──── *)

let%test "api route" =
  let e = make ~name:"app" ~hosts:[ "app.example.com" ] ()
          |> get "/api/health" (fun c -> Conn.json c {|{"ok":true}|})
          |> get "/" (fun c -> Conn.html c "<h1>home</h1>") in
  (Pipeline.run (handler e) (req_ "/api/health")).H.body = {|{"ok":true}|}

let%test "home route" =
  let e = make ~name:"app" ~hosts:[ "app.example.com" ] ()
          |> get "/api/health" (fun c -> Conn.json c {|{"ok":true}|})
          |> get "/" (fun c -> Conn.html c "<h1>home</h1>") in
  (Pipeline.run (handler e) (req_ "/")).H.body = "<h1>home</h1>"

let%test "unmatched 404" =
  let e = make ~name:"app" ~hosts:[ "app.example.com" ] ()
          |> get "/api/health" (fun c -> Conn.json c {|{"ok":true}|})
          |> get "/" (fun c -> Conn.html c "<h1>home</h1>") in
  (Pipeline.run (handler e) (req_ "/nope")).H.status = 404

let%test "name is carried" =
  let e = make ~name:"app" ~hosts:[ "app.example.com" ] () in
  name e = "app"

let%test "hosts are carried" =
  let e = make ~name:"app" ~hosts:[ "app.example.com" ] () in
  hosts e = [ "app.example.com" ]

let%test "guard halts" =
  let guard : Pipeline.t = fun c -> if Conn.path c = "/blocked" then Conn.text ~status:403 c "no" else c in
  let e2 = make ~name:"guarded" () |> use guard
           |> get "/blocked" (fun c -> Conn.text c "should-not-reach")
           |> get "/ok" (fun c -> Conn.text c "ok") in
  (Pipeline.run (handler e2) (req_ "/blocked")).H.status = 403

let%test "guard passes others" =
  let guard : Pipeline.t = fun c -> if Conn.path c = "/blocked" then Conn.text ~status:403 c "no" else c in
  let e2 = make ~name:"guarded" () |> use guard
           |> get "/blocked" (fun c -> Conn.text c "should-not-reach")
           |> get "/ok" (fun c -> Conn.text c "ok") in
  (Pipeline.run (handler e2) (req_ "/ok")).H.body = "ok"

let%test "hosts default to the catch-all" =
  let e2 = make ~name:"guarded" () in
  hosts e2 = [ "*" ]

(* ──── matched-phase tests (the 404-stays-404 property) ──── *)

let%test "matched route -> auth runs, gets 401" =
  let auth_paw : Pipeline.t = fun c -> Conn.text ~status:401 c "unauthorized" in
  let e3 = make ~name:"secured" ()
           |> get "/api/secret" (fun c -> Conn.text c "top secret")
           |> pipe_matched [ auth_paw ] in
  (Pipeline.run (handler e3) (req_ "/api/secret")).H.status = 401

let%test_unit "auth DID run on a matched route" =
  let auth_ran = ref false in
  let auth_paw : Pipeline.t = fun c -> auth_ran := true; Conn.text ~status:401 c "unauthorized" in
  let e3 = make ~name:"secured" ()
           |> get "/api/secret" (fun c -> Conn.text c "top secret")
           |> pipe_matched [ auth_paw ] in
  let _ = Pipeline.run (handler e3) (req_ "/api/secret") in
  Fennec_hunt_unit.check "auth ran" !auth_ran

let%test "unmatched -> 404 (not 401 from auth)" =
  let auth_paw : Pipeline.t = fun c -> Conn.text ~status:401 c "unauthorized" in
  let e3 = make ~name:"secured" ()
           |> get "/api/secret" (fun c -> Conn.text c "top secret")
           |> pipe_matched [ auth_paw ] in
  (Pipeline.run (handler e3) (req_ "/nonexistent")).H.status = 404

let%test_unit "auth did NOT run on an unmatched route" =
  let auth_ran = ref false in
  let auth_paw : Pipeline.t = fun c -> auth_ran := true; Conn.text ~status:401 c "unauthorized" in
  let e3 = make ~name:"secured" ()
           |> get "/api/secret" (fun c -> Conn.text c "top secret")
           |> pipe_matched [ auth_paw ] in
  auth_ran := false;
  let _ = Pipeline.run (handler e3) (req_ "/nonexistent") in
  Fennec_hunt_unit.check "auth did not run" (not !auth_ran)

let%test_unit "matched route gets the header stamp" =
  let stamp : Pipeline.t = fun c -> Conn.before_send c (fun r -> { r with H.headers = ("X-Auth", "ok") :: r.H.headers }) in
  let e4 = make ~name:"stamped" ()
           |> get "/api/data" (fun c -> Conn.text c "data")
           |> pipe_matched [ stamp ] in
  let conn4 = Pipeline.run_conn (handler e4) (req_ "/api/data") in
  let resp4 = Conn.apply_before_send conn4 (Option.get (Conn.resp conn4)) in
  Fennec_hunt_unit.check "X-Auth header stamp" (List.assoc_opt "X-Auth" resp4.H.headers = Some "ok")

(* ──── flat pipeline (backward compat) ──── *)

let%test "flat (no pipe_matched) still works" =
  let e5 = make ~name:"flat" () |> get "/ok" (fun c -> Conn.text c "ok") in
  (Pipeline.run (handler e5) (req_ "/ok")).H.body = "ok"

let%test "flat unmatched -> 404" =
  let e5 = make ~name:"flat" () |> get "/ok" (fun c -> Conn.text c "ok") in
  (Pipeline.run (handler e5) (req_ "/nope")).H.status = 404

(* ──── overlap: two endpoints on the SAME host, the server's dispatch (try each on a fresh conn,
   first to ANSWER wins, a decliner falls through). These prove the airtight properties the
   Host_router ordering relies on. ──── *)

(* mirror the server's [handle_conn] loop: a fresh conn per attempt, first answered wins *)
let first_answer req endpoints =
  let rec go = function
    | [] -> None
    | e :: rest ->
      let c = Pipeline.run_conn (handler e) req in
      if Conn.answered c then Some c else go rest
  in
  go endpoints

let%test_unit "overlap: request for B's route falls through A; A's matched auth never fires" =
  let auth_ran = ref false in
  let a = make ~name:"auth" ~hosts:[ "app.com" ] ()
          |> get "/admin" (fun c -> Conn.text c "admin")
          |> use_matched (fun c -> auth_ran := true; Conn.text ~status:401 c "no") in
  let b = make ~name:"public" ~hosts:[ "app.com" ] () |> get "/public" (fun c -> Conn.text c "public") in
  let body = match first_answer (req_ "/public") [ a; b ] with Some c -> (Option.get (Conn.resp c)).H.body | None -> "none" in
  Fennec_hunt_unit.check "B answered /public" (body = "public");
  Fennec_hunt_unit.check "A's matched auth did NOT fire on the fall-through" (not !auth_ran)

let%test "overlap: request for A's own route is answered by A (auth fires)" =
  let a = make ~name:"auth" ~hosts:[ "app.com" ] ()
          |> get "/admin" (fun c -> Conn.text c "admin")
          |> use_matched (fun c -> Conn.text ~status:401 c "no") in
  let b = make ~name:"public" ~hosts:[ "app.com" ] () |> get "/public" (fun c -> Conn.text c "public") in
  (match first_answer (req_ "/admin") [ a; b ] with Some c -> (Option.get (Conn.resp c)).H.status | None -> 0) = 401

let%test "overlap: a declining endpoint's before_send leaves no trace on the next" =
  let a = make ~name:"a" ~hosts:[ "app.com" ] ()
          |> use (fun c -> Conn.before_send c (fun r -> { r with H.headers = ("X-From-A", "1") :: r.H.headers }))
          |> get "/a" (fun c -> Conn.text c "a") in
  let b = make ~name:"b" ~hosts:[ "app.com" ] () |> get "/b" (fun c -> Conn.text c "b") in
  match first_answer (req_ "/b") [ a; b ] with
  | Some c -> List.assoc_opt "X-From-A" (Conn.apply_before_send c (Option.get (Conn.resp c))).H.headers = None
  | None -> false

let%test "overlap: all decline -> no answer (the server then 404s)" =
  let a = make ~name:"a" ~hosts:[ "app.com" ] () |> get "/a" (fun c -> Conn.text c "a") in
  let b = make ~name:"b" ~hosts:[ "app.com" ] () |> get "/b" (fun c -> Conn.text c "b") in
  first_answer (req_ "/neither") [ a; b ] = None

(* ──── boot conflict detection (a (method, exact path) declared twice) ──── *)

let%test "conflicts: duplicate GET path is flagged" =
  let e = make ~name:"x" () |> get "/a" (fun c -> c) |> get "/a" (fun c -> c) in
  List.length (conflicts e) = 1
let%test "conflicts: same path, different methods is clean (incl. form = GET+POST)" =
  let e = make ~name:"x" () |> get "/a" (fun c -> c) |> post "/a" (fun c -> c) |> form "/f" (fun c -> c) in
  conflicts e = []
let%test "conflicts: distinct routes + middleware are clean" =
  let e = make ~name:"x" () |> use (fun c -> c) |> get "/a" (fun c -> c) |> get "/b" (fun c -> c) in
  conflicts e = []
let%test "conflicts: a duplicate split across middleware is still flagged" =
  let e = make ~name:"x" () |> get "/a" (fun c -> c) |> use (fun c -> c) |> get "/a" (fun c -> c) in
  List.length (conflicts e) = 1
