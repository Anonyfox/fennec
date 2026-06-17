(* The compiled within-endpoint route dispatcher. An endpoint's declared routes are compiled ONCE (at
   serve, see {!Endpoint.handler}) into this table; the per-request hot path is then:

     - a single O(1) hash lookup keyed on the request path STRING itself — zero allocation, and
       independent of how many routes the endpoint has, then
     - only when there are parameterised routes AND the static lookup didn't answer, an ordered
       segment match over the (few) [:param]/[*splat] routes.

   So a static-only endpoint dispatches in O(1) with no allocation on the routing step. Every helper
   is top-level (no per-request closures). The table DECLINES (returns the conn untouched) when no
   route matches, so the endpoint falls through to the next paw / endpoint exactly as before.

   Precedence: a static (exact) route beats a parameterised one for the same path — most-specific
   wins, as every mainstream router does — and among parameterised routes, declaration order. A
   duplicate (method, exact path) keeps the first declared (the old first-match-wins behaviour);
   {!conflicts} surfaces such duplicates for fail-at-boot. *)

module H = Http

type handler = Conn.t -> Conn.t

(* a route as DECLARED by a verb: its method, its literal pattern, and the userland handler (which may
   read captured path params off the conn) *)
type route = { meth : H.meth; path : string; handler : handler }

type t = {
  static : (string, (H.meth * handler) list) Hashtbl.t; (* exact path -> handlers by method, decl order *)
  dynamic : (H.meth * string list * handler) list; (* method, pre-split pattern segments, handler — decl order *)
}

(* compile a run of declared routes into the table. A duplicate (method, exact path) keeps the FIRST
   declared (matching the old scan); use {!conflicts} to reject the ambiguity at boot instead. *)
let build (routes : route list) : t =
  let static = Hashtbl.create (max 1 (List.length routes)) in
  let dynamic = ref [] in
  List.iter
    (fun r ->
      if Route_match.has_params r.path then dynamic := (r.meth, Route_match.segments r.path, r.handler) :: !dynamic
      else
        let cur = match Hashtbl.find_opt static r.path with Some l -> l | None -> [] in
        if not (List.exists (fun (m, _) -> m = r.meth) cur) then Hashtbl.replace static r.path (cur @ [ (r.meth, r.handler) ]))
    routes;
  { static; dynamic = List.rev !dynamic }

(* duplicate (method, exact path) declarations — for fail-at-boot validation; [] if the run is clean *)
let conflicts (routes : route list) : string list =
  let seen = Hashtbl.create 16 in
  List.filter_map
    (fun r ->
      if Route_match.has_params r.path then None
      else if Hashtbl.mem seen (r.meth, r.path) then Some (Printf.sprintf "route %s %s is declared more than once" (H.string_of_meth r.meth) r.path)
      else (Hashtbl.replace seen (r.meth, r.path) (); None))
    routes

(* run the handler whose method satisfies [m] among a path's routes; decline (return the conn) if none
   does. Top-level + option-free, for the static fast path. *)
let rec run_meth m conn = function [] -> conn | (rm, h) :: rest -> if Route_match.meth_matches rm m then h conn else run_meth m conn rest

(* like {!run_meth} but reports "no method matched" so the caller can fall through to dynamic routes *)
let rec find_meth m = function [] -> None | (rm, h) :: rest -> if Route_match.meth_matches rm m then Some h else find_meth m rest

(* the first parameterised route matching method + pre-split path; declines (returns conn) if none *)
let rec match_dynamic m segs conn = function
  | [] -> conn
  | (rm, pat, h) :: rest ->
    if Route_match.meth_matches rm m then (match Route_match.match_segments pat segs [] with Some ps -> h (Conn.set_path_params conn ps) | None -> match_dynamic m segs conn rest)
    else match_dynamic m segs conn rest

(* the dispatch paw — built once, run per request *)
let dispatch (t : t) : handler =
  if t.dynamic = [] then
    (* the common case: no parameterised routes — pure O(1) static dispatch, option-free on a hit *)
    fun conn -> ( match Hashtbl.find_opt t.static (Conn.path conn) with Some hs -> run_meth (Conn.meth conn) conn hs | None -> conn)
  else fun conn ->
    let p = Conn.path conn and m = Conn.meth conn in
    match Hashtbl.find_opt t.static p with
    | Some hs -> ( match find_meth m hs with Some h -> h conn | None -> match_dynamic m (Route_match.segments p) conn t.dynamic)
    | None -> match_dynamic m (Route_match.segments p) conn t.dynamic

(* ──── route_table ──── *)

let req_ ?(meth = H.GET) path = H.make_request ~meth ~path ()
let run_ table req = Conn.resp (dispatch table (Conn.make req))
let body_ table req = match run_ table req with Some r -> r.H.body | None -> "<decline>"

let routes_ =
  [
    { meth = H.GET; path = "/"; handler = (fun c -> Conn.text c "home") };
    { meth = H.GET; path = "/users/me"; handler = (fun c -> Conn.text c "me") };
    { meth = H.GET; path = "/users/:id"; handler = (fun c -> Conn.text c ("user:" ^ Option.value (Conn.path_param c "id") ~default:"?")) };
    { meth = H.POST; path = "/users/:id"; handler = (fun c -> Conn.text c "updated") };
    { meth = H.GET; path = "/files/*rest"; handler = (fun c -> Conn.text c ("file:" ^ Option.value (Conn.path_param c "rest") ~default:"?")) };
  ]

let table_ = build routes_

let%test "static O(1) hit" = body_ table_ (req_ "/") = "home"
let%test "static beats param (most-specific) regardless of declaration" = body_ table_ (req_ "/users/me") = "me"
let%test "param capture" = body_ table_ (req_ "/users/42") = "user:42"
let%test "param + method" = body_ table_ (req_ ~meth:H.POST "/users/42") = "updated"
let%test "splat captures the rest" = body_ table_ (req_ "/files/a/b/c.txt") = "file:a/b/c.txt"
let%test "HEAD answered by GET" = run_ table_ (req_ ~meth:H.HEAD "/") <> None
let%test "no match declines" = body_ table_ (req_ "/nope") = "<decline>"
let%test "wrong method on static path declines (no param fallback here)" = body_ table_ (req_ ~meth:H.DELETE "/") = "<decline>"

let%test "static-only table takes the zero-alloc fast path" = (build [ { meth = H.GET; path = "/a"; handler = Fun.id } ]).dynamic = []

let%test "conflicts: clean run has none" = conflicts routes_ = []
let%test "conflicts: duplicate method+path reported" =
  match conflicts [ { meth = H.GET; path = "/x"; handler = Fun.id }; { meth = H.GET; path = "/x"; handler = Fun.id } ] with
  | [ msg ] -> Fennec_hunt_unit.str_contains msg "/x"
  | _ -> false
let%test "conflicts: same path different methods is NOT a conflict" =
  conflicts [ { meth = H.GET; path = "/x"; handler = Fun.id }; { meth = H.POST; path = "/x"; handler = Fun.id } ] = []
