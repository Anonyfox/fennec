type result = {
  name : string;
  port : int;
  ok : bool;
}

let failures rs = List.fold_left (fun n r -> if r.ok then n else n + 1) 0 rs

let plural n = if n = 1 then "" else "s"

let summary rs =
  let n = List.length rs in
  match failures rs with
  | 0 -> Printf.sprintf "%d suite%s passed" n (plural n)
  | f ->
    let failed =
      List.filter_map (fun r -> if r.ok then None else Some (Printf.sprintf "%s (:%d)" r.name r.port)) rs
    in
    Printf.sprintf "%d of %d suite%s failed: %s" f n (plural n) (String.concat ", " failed)

(* ──── tests ──── *)

let ok name port = { name; port; ok = true }
let bad name port = { name; port; ok = false }

let%test "no suites -> 0 failures" = failures [] = 0
let%test "all green -> 0 failures" = failures [ ok "a" 8200; ok "b" 8300 ] = 0
let%test "counts only the failed" = failures [ ok "a" 8200; bad "b" 8300; bad "c" 8400 ] = 2

let%test "single green" = summary [ ok "smoke" 8200 ] = "1 suite passed"
let%test "many green" = summary [ ok "a" 8200; ok "b" 8300; ok "c" 8400 ] = "3 suites passed"
let%test "single failed names it" = summary [ bad "checkout" 8300 ] = "1 of 1 suite failed: checkout (:8300)"
let%test "mixed names only the failures, in order" =
  summary [ ok "a" 8200; bad "checkout" 8300; ok "c" 8400; bad "users" 8500 ]
  = "2 of 4 suites failed: checkout (:8300), users (:8500)"
