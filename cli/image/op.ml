(* See op.mli. The typed transform options + their serialisation to the engine wire string. *)

type fit =
  | Contain
  | Cover

type t = {
  resize : Geometry.t option;
  fit : fit;
  quality : int option;
  strip : bool;
}

let default = { resize = None; fit = Contain; quality = None; strip = false }

let to_opts t =
  let parts = ref [] in
  let add k v = parts := Printf.sprintf "%s=%s" k v :: !parts in
  (match t.resize with
   | None -> ()
   | Some (Geometry.Width w) -> add "w" (string_of_int w)
   | Some (Geometry.Height h) -> add "h" (string_of_int h)
   | Some (Geometry.Box (w, h)) ->
     add "w" (string_of_int w);
     add "h" (string_of_int h));
  (match t.fit with Cover -> add "fit" "cover" | Contain -> ());
  (match t.quality with Some q -> add "q" (string_of_int q) | None -> ());
  if t.strip then add "strip" "1";
  String.concat ";" (List.rev !parts)

(* ──── tests ──── *)

let%test "default serialises to the empty string" = to_opts default = ""

let%test "box + cover + quality, in a stable key order" =
  to_opts { default with resize = Some (Geometry.Box (800, 600)); fit = Cover; quality = Some 75 } = "w=800;h=600;fit=cover;q=75"

let%test "width-only emits just w" = to_opts { default with resize = Some (Geometry.Width 1280) } = "w=1280"
let%test "height-only emits just h" = to_opts { default with resize = Some (Geometry.Height 600) } = "h=600"
let%test "contain emits no fit key" = to_opts { default with resize = Some (Geometry.Box (10, 10)) } = "w=10;h=10"
let%test "strip emits strip=1" = to_opts { default with strip = true } = "strip=1"
