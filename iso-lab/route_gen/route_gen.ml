(* File-tree routing codegen. Scans a routes/ tree (Next.js convention) and emits a
   self-contained source dir: a renamed copy of each page (so [id].mlx becomes a
   valid OCaml module), a Routes module (router + registration), and a typed Paths
   module (one builder per route — existence + param-names checked at COMPILE time).

   usage: route_gen <routes_dir> <out_dir> <base> *)

let read f = let ic = open_in_bin f in let s = really_input_string ic (in_channel_length ic) in close_in ic; s
let write f s = let oc = open_out_bin f in output_string oc s; close_out oc

let starts p s = String.length s >= String.length p && String.sub s 0 (String.length p) = p
let is_alnum c = (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || (c >= '0' && c <= '9')

(* a filesystem segment -> (kind, name). "[...x]" catch-all, "[x]" param, else literal *)
type seg = Catch of string | Param of string | Lit of string
let classify s =
  let n = String.length s in
  if n >= 5 && starts "[..." s && s.[n-1] = ']' then Catch (String.sub s 4 (n - 5))
  else if n >= 2 && s.[0] = '[' && s.[n-1] = ']' then Param (String.sub s 1 (n - 2))
  else Lit s
let mangle s = String.map (fun c -> if is_alnum c then c else '_') s

(* collect (segments-incl-basename) for every *.mlx, basename "index" dropped from the URL *)
let rec walk dir prefix acc =
  Sys.readdir dir |> Array.to_list |> List.sort compare
  |> List.fold_left (fun acc name ->
       let full = Filename.concat dir name in
       if Sys.is_directory full then walk full (prefix @ [ name ]) acc
       else if Filename.check_suffix name ".mlx" then
         (prefix, Filename.chop_suffix name ".mlx", full) :: acc
       else acc)
       acc

let () =
  let routes = Sys.argv.(1) and out = Sys.argv.(2) and base = Sys.argv.(3) in
  let pages = List.rev (walk routes [] []) in
  let route_lines = ref [] and path_lines = ref [] and page_mods = ref [] and catch = ref None in
  List.iter (fun (prefix, basename, full) ->
    let url_segs = prefix @ (if basename = "index" then [] else [ basename ]) in
    let segs = List.map classify url_segs in
    let clean s = match classify s with Catch n | Param n | Lit n -> n in
    let modname = "Page_" ^ (match List.map (fun s -> mangle (clean s)) (prefix @ [ basename ]) with [] -> "root" | l -> String.concat "_" l) in
    (* each page becomes a nested module; the ppx turns its `view` into a `make` *)
    page_mods := Printf.sprintf "module %s = struct\n%s\nend" modname (read full) :: !page_mods;
    let pattern = "/" ^ String.concat "/" (List.map (function Lit s -> s | Param p -> ":" ^ p | Catch _ -> "*") segs) in
    let is_catch = List.exists (function Catch _ -> true | _ -> false) segs in
    if is_catch then catch := Some modname
    else begin
      route_lines := Printf.sprintf "  |> Router.page %S %s.make" pattern modname :: !route_lines;
      let pname = (match List.filter_map (function Lit s -> Some (mangle s) | Param p -> Some (mangle p) | Catch _ -> None) segs with [] -> "root" | l -> String.concat "_" l) in
      let params = List.filter_map (function Param p -> Some p | _ -> None) segs in
      let fmt = "/" ^ String.concat "/" (List.map (function Lit s -> s | Param _ -> "%s" | Catch _ -> "%s") segs) in
      let args_sig = if params = [] then "()" else String.concat " " (List.map (fun p -> "~" ^ p) params) in
      let args_app = String.concat " " params in
      path_lines := Printf.sprintf "  let %s %s = Router.absolutize base (Printf.sprintf %S %s)" pname args_sig fmt args_app :: !path_lines
    end)
    pages;
  let not_found = match !catch with Some m -> Printf.sprintf " ~not_found:%s.make" m | None -> "" in
  (* ONE generated module: typed Paths first (pages link via it), then the page
     modules, then the router (references the pages). *)
  let buf = Buffer.create 4096 in
  Buffer.add_string buf "(* GENERATED from routes/ — do not edit. The tree IS the route table. *)\n\n";
  Buffer.add_string buf (Printf.sprintf "module Paths = struct\n  let base = %S\n%s\nend\n\n"
                           base (String.concat "\n" (List.rev !path_lines)));
  Buffer.add_string buf (String.concat "\n\n" (List.rev !page_mods));
  Buffer.add_string buf (Printf.sprintf "\n\nlet router =\n  Router.make ~base:%S%s ()\n%s\n"
                           base not_found (String.concat "\n" (List.rev !route_lines)));
  write out (Buffer.contents buf)
