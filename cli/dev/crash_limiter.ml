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
