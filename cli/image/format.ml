(* See format.mli. *)

type t =
  | Jpeg
  | Png
  | Gif
  | Webp
  | Ico

let all = [ Jpeg; Png; Gif; Webp; Ico ]
let extension = function Jpeg -> "jpg" | Png -> "png" | Gif -> "gif" | Webp -> "webp" | Ico -> "ico"
let to_string = extension
let to_engine = function Jpeg -> "jpeg" | Png -> "png" | Gif -> "gif" | Webp -> "webp" | Ico -> "ico"

let of_string s =
  match String.lowercase_ascii s with
  | "jpg" | "jpeg" -> Some Jpeg
  | "png" -> Some Png
  | "gif" -> Some Gif
  | "webp" -> Some Webp
  | "ico" -> Some Ico
  | _ -> None

let of_extension path =
  match String.rindex_opt path '.' with
  | None -> None
  | Some i -> of_string (String.sub path (i + 1) (String.length path - i - 1))

(* ──── tests ──── *)

let%test "of_string accepts jpg + jpeg, case-insensitively" = of_string "JPG" = Some Jpeg && of_string "jpeg" = Some Jpeg
let%test "of_string rejects an unknown name" = of_string "bmp" = None
let%test "of_extension reads the LAST dot" = of_extension "a/b.c/hero.webp" = Some Webp
let%test "of_extension is None for no / unknown extension" = of_extension "noext" = None && of_extension "x.tiff" = None
let%test "extension and engine token differ for jpeg" = extension Jpeg = "jpg" && to_engine Jpeg = "jpeg"
let%test "every format round-trips through its own extension" = List.for_all (fun f -> of_string (extension f) = Some f) all
