type t = {
  path : string;
  line : int;
  digest : string option;
  generated : bool;
}

let make ?digest ?(generated = false) ~path ~line () = { path; line; digest; generated }

let compare a b =
  match compare a.path b.path with
  | 0 -> compare a.line b.line
  | n -> n

let to_string t =
  let suffix = if t.generated then " generated" else "" in
  Printf.sprintf "%s:%d%s" t.path t.line suffix

let to_yojson t =
  `Assoc
    [
      ("path", `String t.path);
      ("line", `Int t.line);
      ("generated", `Bool t.generated);
      ("digest", match t.digest with Some d -> `String d | None -> `Null);
    ]

let of_yojson = function
  | `Assoc fields ->
    let find name = List.assoc_opt name fields in
    let path = match find "path" with Some (`String s) -> s | _ -> "" in
    let line = match find "line" with Some (`Int n) -> n | _ -> 1 in
    let generated = match find "generated" with Some (`Bool b) -> b | _ -> false in
    let digest = match find "digest" with Some (`String s) -> Some s | _ -> None in
    { path; line; digest; generated }
  | _ -> { path = ""; line = 1; digest = None; generated = false }

let%test "source refs render path and line" =
  to_string (make ~path:"a.ml" ~line:3 ()) = "a.ml:3"
