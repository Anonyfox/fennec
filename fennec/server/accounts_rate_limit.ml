(* Brute-force throttle for the account auth methods. A token bucket per (rule, key): up to
   [capacity] tokens, refilled at [per_second] tokens/sec, one attempt spends one. Login is charged
   against TWO dimensions at once — the client IP and the login selector (account) — so it is denied
   the moment EITHER is exhausted; createUser is charged per IP. Same algorithm as [Rate_limit] (the
   HTTP middleware), but keyed on application identity (account selector) rather than the socket, and
   surfaced as a result the method handler turns into a DDP error instead of an HTTP 429.

   The table is mutex-guarded (correct across worker domains) and a gated GC sweep — at most once per
   [sweep_interval] — drops every fully-recovered bucket (one whose tokens would have refilled to its
   own capacity), keeping the request path amortized O(1) and bounding the table against a
   spoofed-key DoS. *)

type limit = { capacity : float; per_second : float }

let limit ~max_attempts ~window =
  if max_attempts <= 0 then invalid_arg "Accounts_rate_limit.limit: ~max_attempts must be positive";
  if window <= 0. then invalid_arg "Accounts_rate_limit.limit: ~window must be positive";
  { capacity = float_of_int max_attempts; per_second = float_of_int max_attempts /. window }

(* Meteor's DDPRateLimiter default for login/createUser/resetPassword: 5 attempts / 10 s. *)
let default_login = limit ~max_attempts:5 ~window:10.
let default_create_user = limit ~max_attempts:5 ~window:10.

(* each bucket carries its own rule so the shared table can hold buckets from differently-configured
   rules and still refill / sweep each by its own capacity and rate *)
type bucket = { mutable tokens : float; mutable last : float; lim : limit }

type t = {
  enabled : bool;
  login : limit;
  create_user : limit;
  table : (string, bucket) Hashtbl.t;
  mu : Mutex.t;
  now : unit -> float;
  mutable last_sweep : float;
}

let make ?(now = Unix.gettimeofday) ?(enabled = true) ?(login = default_login)
    ?(create_user = default_create_user) () =
  { enabled; login; create_user; table = Hashtbl.create 256; mu = Mutex.create (); now;
    last_sweep = neg_infinity }

(* how often the per-request path may run the O(n) GC sweep (seconds) *)
let sweep_interval = 60.0

(* a bucket whose tokens would have refilled to its own capacity by [now] carries zero rate debt — a
   later request recreates an identical full bucket, so dropping it is behavior-preserving *)
let recovered ~now b = b.tokens +. (Float.max 0. (now -. b.last) *. b.lim.per_second) >= b.lim.capacity

let sweep_dead ~now table =
  let dead = Hashtbl.fold (fun k b acc -> if recovered ~now b then k :: acc else acc) table [] in
  List.iter (Hashtbl.remove table) dead

(* find-or-create a bucket and refill it by elapsed time (never negative if the clock jumps back,
   capped at capacity); the table mutex must already be held *)
let refilled t ~now key lim =
  let b =
    match Hashtbl.find_opt t.table key with
    | Some b -> b
    | None ->
      let b = { tokens = lim.capacity; last = now; lim } in
      Hashtbl.replace t.table key b;
      b
  in
  b.tokens <- Float.min b.lim.capacity (b.tokens +. (Float.max 0. (now -. b.last) *. b.lim.per_second));
  b.last <- now;
  b

let retry_after b =
  let secs = if b.lim.per_second > 0. then (1.0 -. b.tokens) /. b.lim.per_second else 1.0 in
  max 1 (int_of_float (Float.ceil secs))

(* charge one attempt against EVERY (key, limit) in [specs]: spend one token from each if all have
   one to give, else spend nothing and report the longest retry-after across the empty buckets. Not
   spending on denial keeps a throttled attempt from depleting the other dimension's budget. *)
let charge t (specs : (string * limit) list) : (unit, int) result =
  if not t.enabled then Ok ()
  else begin
    Mutex.lock t.mu;
    Fun.protect
      ~finally:(fun () -> Mutex.unlock t.mu)
      (fun () ->
        let now = t.now () in
        if now -. t.last_sweep >= sweep_interval then begin
          t.last_sweep <- now;
          sweep_dead ~now t.table
        end;
        let buckets = List.map (fun (key, lim) -> refilled t ~now key lim) specs in
        if List.for_all (fun b -> b.tokens >= 1.0) buckets then begin
          List.iter (fun b -> b.tokens <- b.tokens -. 1.0) buckets;
          Ok ()
        end
        else
          Error (List.fold_left (fun acc b -> if b.tokens >= 1.0 then acc else max acc (retry_after b)) 1 buckets))
  end

let login_allowed t ~ip ~account =
  charge t
    (("login\000account\000" ^ account, t.login)
    :: (match ip with Some ip -> [ ("login\000ip\000" ^ ip, t.login) ] | None -> []))

let create_user_allowed t ~ip =
  (* unknown IP → one shared bucket, so a missing source address fails CLOSED (one shared limit) *)
  charge t [ ("createUser\000ip\000" ^ (match ip with Some ip -> ip | None -> "\000anon"), t.create_user) ]

(* ──── accounts_rate_limit tests ──── *)

let fixed c = make ~now:(fun () -> c) ()
let ip = Some "1.2.3.4"

let%test "limit rejects non-positive configuration" =
  let bad f = match f () with exception Invalid_argument _ -> true | _ -> false in
  bad (fun () -> limit ~max_attempts:0 ~window:10.) && bad (fun () -> limit ~max_attempts:5 ~window:0.)

let%test "login: five attempts pass, the sixth is throttled (Meteor's 5/10s default)" =
  let t = fixed 0. in
  let one () = login_allowed t ~ip ~account:"email:ada@x" in
  List.for_all (fun _ -> one () = Ok ()) [ 1; 2; 3; 4; 5 ]
  && (match one () with Error retry -> retry >= 1 | Ok () -> false)

let%test "login: distinct accounts get independent buckets" =
  (* isolate the account dimension with ip:None; a's exhaustion must not spill onto b *)
  let t = fixed 0. in
  let attempt account = login_allowed t ~ip:None ~account in
  List.iter (fun _ -> ignore (attempt "email:a@x")) [ 1; 2; 3; 4; 5 ];
  (match attempt "email:a@x" with Error _ -> true | Ok () -> false) && attempt "email:b@x" = Ok ()

let%test "login: the account bucket throttles even when the attacker rotates IPs" =
  (* five guesses at one account from five different IPs exhaust the per-account bucket; a sixth from
     a fresh IP (its own IP bucket full) is still denied — cross-IP account protection *)
  let t = fixed 0. in
  let attempt n = login_allowed t ~ip:(Some (Printf.sprintf "10.0.0.%d" n)) ~account:"email:victim@x" in
  List.for_all (fun n -> attempt n = Ok ()) [ 1; 2; 3; 4; 5 ]
  && (match attempt 6 with Error _ -> true | Ok () -> false)

let%test "login: the IP bucket throttles a spray across many accounts" =
  let t = fixed 0. in
  let attempt n = login_allowed t ~ip ~account:(Printf.sprintf "email:user%d@x" n) in
  List.for_all (fun n -> attempt n = Ok ()) [ 1; 2; 3; 4; 5 ]
  && (match attempt 6 with Error _ -> true | Ok () -> false)

let%test "login with no client IP still throttles per account" =
  let t = fixed 0. in
  let one () = login_allowed t ~ip:None ~account:"email:ada@x" in
  List.for_all (fun _ -> one () = Ok ()) [ 1; 2; 3; 4; 5 ] && (match one () with Error _ -> true | _ -> false)

let%test "buckets refill over time (a throttled key recovers)" =
  let clock = ref 0. in
  let t = make ~now:(fun () -> !clock) () in
  let one () = login_allowed t ~ip ~account:"email:ada@x" in
  List.iter (fun _ -> ignore (one ())) [ 1; 2; 3; 4; 5 ];
  let blocked = match one () with Error _ -> true | Ok () -> false in
  clock := 4.0 (* +4s @ 0.5 tok/s ⇒ +2 tokens *);
  blocked && one () = Ok () && one () = Ok () && (match one () with Error _ -> true | _ -> false)

let%test "a throttled login spends nothing from the still-healthy dimension" =
  (* exhaust the account bucket from IP A, then attempt from IP B: B is denied (account empty) but its
     own IP bucket must be untouched — once the account recovers, B still has its full burst *)
  let clock = ref 0. in
  let t = make ~now:(fun () -> !clock) () in
  let a = Some "1.1.1.1" and b = Some "2.2.2.2" in
  List.iter (fun _ -> ignore (login_allowed t ~ip:a ~account:"email:v@x")) [ 1; 2; 3; 4; 5 ];
  let b_denied = match login_allowed t ~ip:b ~account:"email:v@x" with Error _ -> true | Ok () -> false in
  clock := 20.0 (* fully refill the account bucket *);
  (* B should now get a full five — proving its IP bucket never lost the earlier denied token *)
  b_denied && List.for_all (fun _ -> login_allowed t ~ip:b ~account:"email:v@x" = Ok ()) [ 1; 2; 3; 4; 5 ]

let%test "createUser: throttled per IP, independent IPs unaffected" =
  let t = fixed 0. in
  let one ip = create_user_allowed t ~ip in
  List.for_all (fun _ -> one (Some "9.9.9.9") = Ok ()) [ 1; 2; 3; 4; 5 ]
  && (match one (Some "9.9.9.9") with Error _ -> true | Ok () -> false)
  && one (Some "8.8.8.8") = Ok ()

let%test "createUser with unknown IP shares one fail-closed bucket" =
  let t = fixed 0. in
  List.for_all (fun _ -> create_user_allowed t ~ip:None = Ok ()) [ 1; 2; 3; 4; 5 ]
  && (match create_user_allowed t ~ip:None with Error _ -> true | _ -> false)

let%test "disabled limiter always passes" =
  let t = make ~enabled:false ~now:(fun () -> 0.) () in
  List.for_all (fun _ -> login_allowed t ~ip ~account:"email:ada@x" = Ok ()) [ 1; 2; 3; 4; 5; 6; 7; 8 ]

let%test "configurable limits are honored" =
  let t = make ~now:(fun () -> 0.) ~login:(limit ~max_attempts:2 ~window:30.) () in
  let one () = login_allowed t ~ip ~account:"email:ada@x" in
  one () = Ok () && one () = Ok () && (match one () with Error _ -> true | _ -> false)

let%test "sweep_dead evicts fully-recovered buckets, keeps in-debt ones" =
  let l = limit ~max_attempts:5 ~window:10. (* 0.5 tok/s *) in
  let table : (string, bucket) Hashtbl.t = Hashtbl.create 8 in
  Hashtbl.replace table "full" { tokens = l.capacity; last = 0.; lim = l };
  Hashtbl.replace table "recovered" { tokens = 1.; last = 0.; lim = l } (* +4 over 8s ⇒ full *);
  Hashtbl.replace table "in_debt" { tokens = 0.; last = 7.5; lim = l } (* +0.25 over 0.5s ⇒ debt *);
  sweep_dead ~now:8.0 table;
  (not (Hashtbl.mem table "full"))
  && (not (Hashtbl.mem table "recovered"))
  && Hashtbl.mem table "in_debt"
