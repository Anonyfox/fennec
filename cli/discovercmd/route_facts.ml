type seg = Catch of string | Param of string | Lit of string

let starts p s =
  String.length s >= String.length p && String.sub s 0 (String.length p) = p

let classify s =
  let n = String.length s in
  if n >= 5 && starts "[..." s && s.[n - 1] = ']' then Catch (String.sub s 4 (n - 5))
  else if n >= 2 && s.[0] = '[' && s.[n - 1] = ']' then Param (String.sub s 1 (n - 2))
  else if n >= 3 && s.[n - 1] = '_' && s.[n - 2] = '_' then Catch (String.sub s 0 (n - 2))
  else if n >= 2 && s.[n - 1] = '_' then Param (String.sub s 0 (n - 1))
  else Lit s

let mangle s =
  String.map
    (function
      | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' as c -> c
      | _ -> '_')
    s

let route_path ~prefix ~basename =
  let url_segs = prefix @ if basename = "index" then [] else [ basename ] in
  "/" ^ String.concat "/" (List.map (fun s -> match classify s with Lit x -> x | Param p -> ":" ^ p | Catch _ -> "*") url_segs)

let typed_path_name ~prefix ~basename =
  let segs = prefix @ if basename = "index" then [] else [ basename ] in
  match
    List.filter_map
      (fun s -> match classify s with Lit x | Param x -> Some (mangle x) | Catch _ -> None)
      segs
  with
  | [] -> "root"
  | xs -> String.concat "_" xs

let%test "underscore param route" =
  route_path ~prefix:[ "products" ] ~basename:"id_" = "/products/:id"

let%test "index path name is root" =
  typed_path_name ~prefix:[] ~basename:"index" = "root"
