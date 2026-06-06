(* See crash_limiter.mli. *)

type t = { window : float; max : int; mutable times : float list (* crash times within the window, newest first *) }

let create ?(window = 10.0) ?(max = 5) () = { window; max; times = [] }

type decision = Retry of float | Give_up

let record t ~now ?(flat = false) () =
  t.times <- now :: List.filter (fun ts -> now -. ts <= t.window) t.times;
  let n = List.length t.times in
  if n >= t.max then Give_up
  else if flat then Retry 1.0
  else Retry (Float.min 3.0 (0.2 *. (2. ** float_of_int (n - 1)))) (* 0.2, 0.4, 0.8, … capped at 3s *)

let reset t = t.times <- []

(* ──── tests ──── *)

let%test "1st crash -> retry" =
  let t = create ~window:10.0 ~max:3 () in
  match record t ~now:0.0 () with Retry _ -> true | Give_up -> false

let%test "2nd crash -> retry" =
  let t = create ~window:10.0 ~max:3 () in
  ignore (record t ~now:0.0 ());
  match record t ~now:1.0 () with Retry _ -> true | Give_up -> false

let%test "3rd within window -> give up" =
  let t = create ~window:10.0 ~max:3 () in
  ignore (record t ~now:0.0 ());
  ignore (record t ~now:1.0 ());
  match record t ~now:2.0 () with Give_up -> true | Retry _ -> false

let%test "crash at t=0 retry" =
  let t = create ~window:10.0 ~max:3 () in
  match record t ~now:0.0 () with Retry _ -> true | Give_up -> false

let%test "crash at t=20 retry (old one aged out)" =
  let t = create ~window:10.0 ~max:3 () in
  ignore (record t ~now:0.0 ());
  match record t ~now:20.0 () with Retry _ -> true | Give_up -> false

let%test "crash at t=40 retry (window slid)" =
  let t = create ~window:10.0 ~max:3 () in
  ignore (record t ~now:0.0 ());
  ignore (record t ~now:20.0 ());
  match record t ~now:40.0 () with Retry _ -> true | Give_up -> false

let%test "after reset, next crash retries (not give up)" =
  let t = create ~window:10.0 ~max:2 () in
  ignore (record t ~now:0.0 ());
  reset t;
  match record t ~now:0.1 () with Retry _ -> true | Give_up -> false

let%test "flat backoff is 1.0s" =
  let t = create () in
  record t ~now:0.0 ~flat:true () = Retry 1.0

let%test_unit "backoff curve" =
  let open Fennec_hunt_unit in
  let backoff = function Retry b -> b | Give_up -> Float.nan in
  let approx a b = Float.abs (a -. b) < 1e-9 in
  let t = create ~window:1000.0 ~max:100 () in
  check "backoff #1 = 0.2" (approx (backoff (record t ~now:0.0 ())) 0.2);
  check "backoff #2 = 0.4" (approx (backoff (record t ~now:0.0 ())) 0.4);
  check "backoff #3 = 0.8" (approx (backoff (record t ~now:0.0 ())) 0.8);
  check "backoff #4 = 1.6" (approx (backoff (record t ~now:0.0 ())) 1.6);
  check "backoff #5 caps at 3.0" (approx (backoff (record t ~now:0.0 ())) 3.0);
  check "backoff #6 stays at 3.0" (approx (backoff (record t ~now:0.0 ())) 3.0)

let%test "max=1 -> first crash gives up" =
  let t = create ~max:1 () in
  match record t ~now:0.0 () with Give_up -> true | Retry _ -> false
