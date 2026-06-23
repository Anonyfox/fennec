(* See geometry.mli. Parse the [-r] geometry into a typed resize request. *)

type t =
  | Width of int
  | Height of int
  | Box of int * int

let pos_int label s =
  match int_of_string_opt (String.trim s) with
  | Some n when n > 0 -> Ok n
  | _ -> Error (Printf.sprintf "resize: %s must be a positive integer (got %S)" label s)

let ( let* ) = Result.bind

let of_string s =
  let s = String.trim s in
  match String.index_opt s 'x' with
  | None ->
    let* w = pos_int "size" s in
    Ok (Width w)
  | Some i -> (
    let l = String.sub s 0 i and r = String.sub s (i + 1) (String.length s - i - 1) in
    match (l, r) with
    | "", "" -> Error "resize: expected WxH, e.g. 800x600"
    | _, "" ->
      let* w = pos_int "width" l in
      Ok (Width w)
    | "", _ ->
      let* h = pos_int "height" r in
      Ok (Height h)
    | _, _ ->
      let* w = pos_int "width" l in
      let* h = pos_int "height" r in
      Ok (Box (w, h)))

(* ──── tests ──── *)

let%test "bare number is a width" = of_string "800" = Ok (Width 800)
let%test "WxH is a box" = of_string "800x600" = Ok (Box (800, 600))
let%test "trailing x is width-only" = of_string "800x" = Ok (Width 800)
let%test "leading x is height-only" = of_string "x600" = Ok (Height 600)
let%test "non-positive is rejected" = Result.is_error (of_string "0") && Result.is_error (of_string "-5")
let%test "garbage is rejected" = Result.is_error (of_string "abc") && Result.is_error (of_string "8xy")
let%test "lone x is rejected" = Result.is_error (of_string "x")
