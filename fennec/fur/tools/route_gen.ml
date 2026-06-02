(* File-tree routing codegen, multi-app. Scans frontend/apps/<name>/ — each app is a
   folder with main.mlx (config: base + template), layout.mlx (the shell), and route
   files (index.mlx / [id].mlx / [...rest].mlx, nesting freely). Emits ONE module:

     module Shop = struct
       <main.mlx inlined: base, template>
       module Layout = struct <layout.mlx> end          (* ppx: view -> make *)
       module Paths  = struct ...typed builders... end
       module Page_* = struct <route file> end          (* ppx: view -> make *)
       let router = Router.make ~base ?not_found () |> Router.page ...
     end
     ...
     let apps : Fur.mount list = [ { base = Shop.base; root = Shop.Layout.make;
                                     router = Shop.router; document = Shop.template }; ... ]

   so the client/server entries are fully generic. usage: route_gen <apps_dir> <out> *)

let read f = let ic = open_in_bin f in let s = really_input_string ic (in_channel_length ic) in close_in ic; s
let write f s = let oc = open_out_bin f in output_string oc s; close_out oc
let starts p s = String.length s >= String.length p && String.sub s 0 (String.length p) = p
let is_alnum c = (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || (c >= '0' && c <= '9')

type seg = Catch of string | Param of string | Lit of string
let classify s =
  let n = String.length s in
  if n >= 5 && starts "[..." s && s.[n-1] = ']' then Catch (String.sub s 4 (n - 5))
  else if n >= 2 && s.[0] = '[' && s.[n-1] = ']' then Param (String.sub s 1 (n - 2))
  else Lit s
let clean s = match classify s with Catch n | Param n | Lit n -> n
let mangle s = String.map (fun c -> if is_alnum c then c else '_') s

(* route files of one app dir (excludes the special main.mlx / layout.mlx) *)
let rec walk dir prefix acc =
  Sys.readdir dir |> Array.to_list |> List.sort compare
  |> List.fold_left (fun acc name ->
       let full = Filename.concat dir name in
       if Sys.is_directory full then walk full (prefix @ [ name ]) acc
       else if Filename.check_suffix name ".mlx" && name <> "main.mlx" && name <> "layout.mlx" then
         (prefix, Filename.chop_suffix name ".mlx", full) :: acc
       else acc)
       acc

let gen_app dir name =
  let appmod = String.capitalize_ascii (mangle name) in
  let pages = List.rev (walk dir [] []) in
  let route_lines = ref [] and path_lines = ref [] and page_mods = ref [] and catch = ref None in
  List.iter (fun (prefix, basename, full) ->
    let url_segs = prefix @ (if basename = "index" then [] else [ basename ]) in
    let segs = List.map classify url_segs in
    let modname = "Page_" ^ (match List.map (fun s -> mangle (clean s)) (prefix @ [ basename ]) with [] -> "root" | l -> String.concat "_" l) in
    page_mods := Printf.sprintf "  module %s = struct\n%s\n  end" modname (read full) :: !page_mods;
    let pattern = "/" ^ String.concat "/" (List.map (function Lit s -> s | Param p -> ":" ^ p | Catch _ -> "*") segs) in
    if List.exists (function Catch _ -> true | _ -> false) segs then catch := Some modname
    else begin
      route_lines := Printf.sprintf "    |> Router.page %S %s.make" pattern modname :: !route_lines;
      let pname = (match List.filter_map (function Lit s -> Some (mangle s) | Param p -> Some (mangle p) | Catch _ -> None) segs with [] -> "root" | l -> String.concat "_" l) in
      let params = List.filter_map (function Param p -> Some p | _ -> None) segs in
      let fmt = "/" ^ String.concat "/" (List.map (function Lit s -> s | Param _ -> "%s" | Catch _ -> "%s") segs) in
      let args_sig = if params = [] then "()" else String.concat " " (List.map (fun p -> "~" ^ p) params) in
      let args_app = String.concat " " params in
      path_lines := Printf.sprintf "    let %s %s = Router.absolutize base (Printf.sprintf %S %s)" pname args_sig fmt args_app :: !path_lines
    end)
    pages;
  let not_found = match !catch with Some m -> Printf.sprintf " ~not_found:%s.make" m | None -> "" in
  let b = Buffer.create 4096 in
  Buffer.add_string b (Printf.sprintf "module %s = struct\n" appmod);
  Buffer.add_string b (Printf.sprintf "%s\n" (read (Filename.concat dir "main.mlx")));   (* base, template *)
  Buffer.add_string b (Printf.sprintf "  module Layout = struct\n%s\n  end\n" (read (Filename.concat dir "layout.mlx")));
  Buffer.add_string b (Printf.sprintf "  module Paths = struct\n%s\n  end\n" (String.concat "\n" (List.rev !path_lines)));
  Buffer.add_string b (String.concat "\n\n" (List.rev !page_mods));
  Buffer.add_string b (Printf.sprintf "\n  let router =\n    Router.make ~base%s ()\n%s\nend\n"
                         not_found (String.concat "\n" (List.rev !route_lines)));
  (appmod, Buffer.contents b)

let () =
  let apps_dir = Sys.argv.(1) and out = Sys.argv.(2) in
  let app_names =
    Sys.readdir apps_dir |> Array.to_list |> List.sort compare
    |> List.filter (fun n -> Sys.is_directory (Filename.concat apps_dir n)) in
  let mods = List.map (fun n -> gen_app (Filename.concat apps_dir n) n) app_names in
  let buf = Buffer.create 8192 in
  Buffer.add_string buf "(* GENERATED from frontend/apps/ — do not edit. *)\n\n";
  List.iter (fun (_, src) -> Buffer.add_string buf src; Buffer.add_string buf "\n") mods;
  let mounts = List.map (fun (m, _) ->
    Printf.sprintf "  { Fur.base = %s.base; root = %s.Layout.make; router = %s.router; document = %s.template }"
      m m m m) mods in
  Buffer.add_string buf (Printf.sprintf "\nlet apps : Fur.mount list = [\n%s\n]\n" (String.concat ";\n" mounts));
  write out (Buffer.contents buf)
