type t = {
  paths : string list;
  components : string list;
  apps : string list;
  routes : string list;
  styles : bool;
  assets : bool;
  tests : string list;
  backend : bool;
  config : bool;
}

let empty =
  { paths = []; components = []; apps = []; routes = []; styles = false; assets = false; tests = []; backend = false; config = false }

let find_sub = Dune_watch.find_sub

let starts_with s pfx =
  let lp = String.length pfx in
  String.length s >= lp && String.sub s 0 lp = pfx

let drop_suffix s suffix =
  let ls = String.length s and lsf = String.length suffix in
  if ls >= lsf && String.sub s (ls - lsf) lsf = suffix then String.sub s 0 (ls - lsf) else s

let path_of_trigger s =
  match find_sub s " changed" with
  | Some i -> String.sub s 0 i
  | None -> s

let uniq xs =
  let rec go seen = function
    | [] -> List.rev seen
    | x :: xs -> if List.mem x seen then go seen xs else go (x :: seen) xs
  in
  go [] xs

let component_name path =
  let prefix = "examples/site/frontend/components/" in
  if starts_with path prefix && Filename.extension path = ".mlx" then
    Some (Filename.basename path |> drop_suffix ".mlx")
  else None

let app_name path =
  let prefix = "examples/site/frontend/apps/" in
  if starts_with path prefix then
    match String.split_on_char '/' (String.sub path (String.length prefix) (String.length path - String.length prefix)) with
    | app :: _ when app <> "" -> Some app
    | _ -> None
  else None

let route_name path =
  let prefix = "examples/site/frontend/apps/" in
  if starts_with path prefix && Filename.extension path = ".mlx" then
    match String.split_on_char '/' (String.sub path (String.length prefix) (String.length path - String.length prefix)) with
    | _app :: "index.mlx" :: [] -> Some "/"
    | _app :: parts ->
      let parts = List.map (fun p -> drop_suffix p ".mlx" |> String.map (function '_' -> ':' | c -> c)) parts in
      Some ("/" ^ String.concat "/" parts)
    | _ -> None
  else None

let classify ?(backend = false) triggers =
  let paths = List.map path_of_trigger triggers |> List.filter (fun s -> s <> "") |> uniq in
  let components = List.filter_map component_name paths |> uniq in
  let apps = List.filter_map app_name paths |> uniq in
  let routes = List.filter_map route_name paths |> uniq in
  let styles =
    List.exists
      (fun p ->
        Filename.extension p = ".scss" || Filename.extension p = ".css"
        || starts_with p "examples/site/frontend/styles/")
      paths
  in
  let assets =
    List.exists
      (fun p ->
        starts_with p "examples/site/public/" || Filename.extension p = ".svg"
        || Filename.extension p = ".png" || Filename.extension p = ".jpg"
        || Filename.extension p = ".jpeg" || Filename.extension p = ".webp")
      paths
  in
  let tests =
    List.filter
      (fun p -> starts_with p "test/" || find_sub p "/test/" <> None || Filename.basename p = "dune" && find_sub p "test" <> None)
      paths
    |> uniq
  in
  let config =
    List.exists
      (fun p ->
        Filename.basename p = "dune" || Filename.basename p = "dune-project" || Filename.extension p = ".opam")
      paths
  in
  { paths; components; apps; routes; styles; assets; tests; backend; config }

let short t =
  let parts =
    []
    |> fun xs -> if t.backend then "backend" :: xs else xs
    |> fun xs -> if t.components <> [] then ("component " ^ String.concat ", " t.components) :: xs else xs
    |> fun xs -> if t.routes <> [] then ("route " ^ String.concat ", " t.routes) :: xs else xs
    |> fun xs -> if t.apps <> [] then ("app " ^ String.concat ", " t.apps) :: xs else xs
    |> fun xs -> if t.styles then "styles" :: xs else xs
    |> fun xs -> if t.assets then "assets" :: xs else xs
    |> fun xs -> if t.tests <> [] then "tests" :: xs else xs
    |> fun xs -> if t.config then "config" :: xs else xs
    |> List.rev
  in
  match parts with
  | [] -> ""
  | _ -> String.concat "; " parts

let%test "classifies component path" =
  let a = classify [ "examples/site/frontend/components/greeting.mlx changed" ] in
  a.components = [ "greeting" ] && a.paths = [ "examples/site/frontend/components/greeting.mlx" ]

let%test "classifies app route" =
  let a = classify [ "examples/site/frontend/apps/web/products/id_.mlx changed" ] in
  a.apps = [ "web" ] && a.routes = [ "/products/id:" ]

let%test "short is compact" =
  let a = classify ~backend:true [ "examples/site/frontend/components/nav.mlx changed"; "examples/site/frontend/apps/web/index.mlx changed" ] in
  Fennec_hunt_unit.str_contains (short a) "backend" && Fennec_hunt_unit.str_contains (short a) "component nav"
