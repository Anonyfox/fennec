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
