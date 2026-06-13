(* Token-bucket rate limiting. Each client key holds up to [capacity] tokens, refilled at
   [per_second] tokens/sec; each request spends one. Empty bucket → 429 + Retry-After. The bucket
   table is guarded by a mutex so it is correct across the server's worker domains. *)

module Conn = Fennec_paw.Conn
module Paw = Fennec_paw.Paw
module H = Fennec_core.Http

type bucket = { mutable tokens : float; mutable last : float }

(* best-effort client identity: the socket peer IP, else the first X-Forwarded-For hop, else a
   single shared "anon" bucket — so a misconfigured proxy fails CLOSED (one shared limit) not open *)
let default_key c =
  match Conn.remote_ip c with
  | Some ip -> ip
  | None -> (
    match Conn.req_header c "x-forwarded-for" with
    | Some f -> ( match String.split_on_char ',' f with x :: _ when String.trim x <> "" -> String.trim x | _ -> "anon")
    | None -> "anon")

(* how often the per-request path may run the O(n) GC sweep (seconds) *)
let sweep_interval = 60.0

(* Drop every FULLY-RECOVERED bucket: one whose tokens would have refilled to [cap] by [now] carries
   zero rate debt, so a later request recreates an identical full bucket — eviction is
   behavior-preserving. This is what keeps the table from growing one entry per distinct client key
   forever (a real leak / spoofed-[X-Forwarded-For] DoS vector on a long-running server). *)
let sweep_dead ~cap ~per_second ~now table =
  let dead =
    Hashtbl.fold
      (fun bk b acc -> if b.tokens +. (Float.max 0. (now -. b.last) *. per_second) >= cap then bk :: acc else acc)
      table []
  in
  List.iter (Hashtbl.remove table) dead

let make ?(key = default_key) ?(capacity = 100) ?(per_second = 10.0) ?(now = Unix.gettimeofday) () : Paw.t =
  let table : (string, bucket) Hashtbl.t = Hashtbl.create 256 in
  let mu = Mutex.create () in
  let cap = float_of_int capacity in
  let last_sweep = ref neg_infinity in
  fun c ->
    let k = key c in
    let t = now () in
    let allowed, retry_after =
      Mutex.lock mu;
      Fun.protect
        ~finally:(fun () -> Mutex.unlock mu)
        (fun () ->
          (* gated GC: at most once per [sweep_interval], keeping the request path amortized O(1).
             Only meaningful when buckets refill ([per_second] > 0); a no-refill limiter holds at
             most one bucket per key by design and the fixed-clock tests stay deterministic. *)
          if per_second > 0. && t -. !last_sweep >= sweep_interval then begin
            last_sweep := t;
            sweep_dead ~cap ~per_second ~now:t table
          end;
          let b =
            match Hashtbl.find_opt table k with
            | Some b -> b
            | None ->
              let b = { tokens = cap; last = t } in
              Hashtbl.replace table k b;
              b
          in
          (* refill by elapsed time (never negative if the clock jumps back), capped at capacity *)
          b.tokens <- Float.min cap (b.tokens +. (Float.max 0. (t -. b.last) *. per_second));
          b.last <- t;
          if b.tokens >= 1.0 then (b.tokens <- b.tokens -. 1.0; (true, 0))
          else
            let secs = if per_second > 0. then (1.0 -. b.tokens) /. per_second else 1.0 in
            (false, max 1 (int_of_float (Float.ceil secs))))
    in
    if allowed then c else Conn.text ~status:429 ~headers:[ ("Retry-After", string_of_int retry_after) ] c "Too Many Requests"

(* ──── rate_limit tests ──── *)

let req_ ?(headers = []) path = H.make_request ~meth:H.GET ~path ~headers ~host:"app.test" ()
let status_ c = (Option.value (Conn.resp c) ~default:(H.text ~status:200 "")).H.status

let%test "within capacity passes; over capacity → 429" =
  let rl = make ~key:(fun _ -> "k") ~capacity:2 ~per_second:0. ~now:(fun () -> 0.) () in
  let one () = status_ (rl (Conn.make (req_ "/x"))) in
  one () = 200 && one () = 200 && one () = 429

let%test "bucket refills over time" =
  let clock = ref 0. in
  let rl = make ~key:(fun _ -> "k") ~capacity:1 ~per_second:1. ~now:(fun () -> !clock) () in
  let one () = status_ (rl (Conn.make (req_ "/x"))) in
  let a = one () in
  let b = one () in
  clock := 1.0 (* +1s ⇒ +1 token *);
  let d = one () in
  a = 200 && b = 429 && d = 200

let%test "separate keys get separate buckets" =
  let rl = make ~capacity:1 ~per_second:0. ~now:(fun () -> 0.) () in
  let one k = status_ (rl (Conn.make (req_ ~headers:[ ("x-forwarded-for", k) ] "/x"))) in
  one "1.1.1.1" = 200 && one "2.2.2.2" = 200 && one "1.1.1.1" = 429

let%test "sweep_dead evicts fully-recovered buckets, keeps in-debt ones (bounds the table)" =
  let cap = 5.0 in
  let table : (string, bucket) Hashtbl.t = Hashtbl.create 8 in
  Hashtbl.replace table "full" { tokens = cap; last = 0. };
  Hashtbl.replace table "recovered" { tokens = 1.; last = 0. } (* +4 over 8s @0.5/s ⇒ full *);
  Hashtbl.replace table "in_debt" { tokens = 0.; last = 7.5 } (* +0.25 over 0.5s ⇒ still in debt *);
  sweep_dead ~cap ~per_second:0.5 ~now:8.0 table;
  (not (Hashtbl.mem table "full"))
  && (not (Hashtbl.mem table "recovered"))
  && Hashtbl.mem table "in_debt"

let%test "eviction is behavior-preserving: a swept full bucket recreates identically" =
  (* a key spends a token, goes fully idle past a sweep, then returns: it must see a fresh full
     bucket — same as a brand-new key — proving the GC changed nothing observable *)
  let clock = ref 0. in
  let rl = make ~key:(fun _ -> "k") ~capacity:1 ~per_second:1. ~now:(fun () -> !clock) () in
  let one () = status_ (rl (Conn.make (req_ "/x"))) in
  let a = one () (* spend the only token *) in
  clock := !clock +. sweep_interval +. 10. (* idle long enough to refill AND trigger a sweep *);
  let b = one () (* fresh full bucket ⇒ allowed *) in
  a = 200 && b = 200
