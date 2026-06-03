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
(* A route segment (file or folder name, minus extension) classifies to:
     literal   "about"           -> /about
     param     "id_"   (trailing _)  -> /:id        (valid OCaml module Id_, so LSP works)
     catch-all "rest__" (trailing __) -> /*rest      (valid module Rest__)
   The bracket forms ([id], [...rest]) are still accepted for back-compat (used by the
   inlining mode), but the underscore forms are the real-module convention. *)
let classify s =
  let n = String.length s in
  if n >= 5 && starts "[..." s && s.[n-1] = ']' then Catch (String.sub s 4 (n - 5))
  else if n >= 2 && s.[0] = '[' && s.[n-1] = ']' then Param (String.sub s 1 (n - 2))
  else if n >= 3 && s.[n-1] = '_' && s.[n-2] = '_' then Catch (String.sub s 0 (n - 2))
  else if n >= 2 && s.[n-1] = '_' then Param (String.sub s 0 (n - 1))
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
  (* The app is emitted as its OWN top-level module (file <app>_routes.mlx), NOT a
     submodule of a shared file. That separation is what lets a per-app client bundle
     link ONLY this app (jsoo/DCE never pulls a sibling app's code) — strictly isolated
     bundles, dev and prod, with no per-app libraries. *)
  ignore appmod;
  Buffer.add_string b (Printf.sprintf "%s\n" (read (Filename.concat dir "main.mlx")));   (* base, template *)
  Buffer.add_string b (Printf.sprintf "module Layout = struct\n%s\n  end\n" (read (Filename.concat dir "layout.mlx")));
  Buffer.add_string b (Printf.sprintf "module Paths = struct\n%s\n  end\n" (String.concat "\n" (List.rev !path_lines)));
  Buffer.add_string b (String.concat "\n\n" (List.rev !page_mods));
  Buffer.add_string b (Printf.sprintf "\nlet router =\n    Router.make ~base%s ()\n%s\n"
                         not_found (String.concat "\n" (List.rev !route_lines)));
  Buffer.add_string b "let mount : Fur.mount = { Fur.base; root = Layout.make; router; document = template }\n";
  Buffer.contents b

let app_dirs apps_dir =
  Sys.readdir apps_dir |> Array.to_list |> List.sort compare
  |> List.filter (fun n -> Sys.is_directory (Filename.concat apps_dir n))

(* the top-level module name for an app's routes file: <app>_routes.mlx -> <App>_routes *)
let route_mod n = String.capitalize_ascii (mangle n ^ "_routes")

(* the wrapped per-app library's module name: app "web" -> lib web_app -> module Web_app *)
let lib_mod n = String.capitalize_ascii (mangle n) ^ "_app"

(* GLUE mode (real-module apps): the app's page/layout/main .mlx are REAL dune modules
   in a per-app library with (include_subdirs qualified), so Merlin/LSP works and editing
   a page recompiles only that module. route_gen emits only the wiring that REFERENCES
   those modules — routes.ml (router + mount) and paths.ml (typed paths) — never inlining
   source. Handles params (id_), catch-all (rest__) and nested folders (products/id_.mlx
   -> module Products.Id_, route /products/:id). *)
let emit_glue app_dir out_dir =
  let pages = List.rev (walk app_dir [] []) in
  let route_lines = ref [] and path_lines = ref [] and catch = ref None in
  List.iter (fun (prefix, basename, _full) ->
    (* qualified module reference: prefix folders + the file, each capitalized as dune
       names them under (include_subdirs qualified): products/id_.mlx -> Products.Id_ *)
    let modref = String.concat "." (List.map String.capitalize_ascii (prefix @ [ basename ])) in
    let url_segs = prefix @ (if basename = "index" then [] else [ basename ]) in
    let segs = List.map classify url_segs in
    let pattern = "/" ^ String.concat "/" (List.map (function Lit s -> s | Param p -> ":" ^ p | Catch _ -> "*") segs) in
    if List.exists (function Catch _ -> true | _ -> false) segs then catch := Some modref
    else begin
      route_lines := Printf.sprintf "  |> Router.page %S %s.make" pattern modref :: !route_lines;
      let pname = (match List.filter_map (function Lit s -> Some (mangle s) | Param p -> Some (mangle p) | Catch _ -> None) segs with [] -> "root" | l -> String.concat "_" l) in
      let params = List.filter_map (function Param p -> Some p | _ -> None) segs in
      let fmt = "/" ^ String.concat "/" (List.map (function Lit s -> s | Param _ -> "%s" | Catch _ -> "%s") segs) in
      let args_sig = if params = [] then "()" else String.concat " " (List.map (fun p -> "~" ^ p) params) in
      let args_app = String.concat " " params in
      path_lines := Printf.sprintf "let %s %s = Router.absolutize Main.base (Printf.sprintf %S %s)" pname args_sig fmt args_app :: !path_lines
    end)
    pages;
  let not_found = match !catch with Some m -> Printf.sprintf " ~not_found:%s.make" m | None -> "" in
  write (Filename.concat out_dir "routes.ml")
    (Printf.sprintf
       "(* GENERATED glue — do not edit. Wires the app's REAL page modules (Index, ...)\n   into a router + mount; the pages themselves are authored .mlx in this library. *)\nlet base = Main.base\nlet template = Main.template\nlet router =\n  Router.make ~base%s ()\n%s\nlet mount : Fur.mount = { Fur.base; root = Layout.make; router; document = template }\n"
       not_found (String.concat "\n" (List.rev !route_lines)));
  write (Filename.concat out_dir "paths.ml")
    (Printf.sprintf "(* GENERATED typed paths — do not edit. *)\n%s\n" (String.concat "\n" (List.rev !path_lines)))

(* routes mode: emit ONE FILE PER APP — <app>_routes.mlx — each its OWN top-level
   module. The per-file separation (not shared submodules of one Routes_gen) is what
   isolates per-app client bundles: a boot referencing only <App>_routes never links a
   sibling app's code, in dev and prod alike. *)
let emit_routes apps_dir out_dir =
  let names = app_dirs apps_dir in
  names |> List.iter (fun n ->
    let body = gen_app (Filename.concat apps_dir n) n in
    write (Filename.concat out_dir (mangle n ^ "_routes.mlx"))
      (Printf.sprintf "(* GENERATED from frontend/apps/%s/ — do not edit. *)\n%s" n body));
  (* routes_index: the combined mount list, for the server / a single all-apps bundle.
     Per-app client bundles do NOT reference this (they boot <App>_routes directly), so
     it never pulls a sibling app's code into an isolated bundle. *)
  write (Filename.concat out_dir "routes_index.ml")
    (Printf.sprintf "(* GENERATED — combined mount list. *)\nlet apps : Fur.mount list = [ %s ]\n"
       (String.concat "; " (List.map (fun n -> route_mod n ^ ".mount") names)))

(* boots mode: emit one client entry per app (<app>.ml) into out_dir, booting ONLY its
   own app — by referencing its per-app library's Routes.mount. Generated, so there are
   no hand-written entry files; each bundle links only its own app's lib. *)
let emit_boots apps_dir out_dir =
  app_dirs apps_dir |> List.iter (fun n ->
    write (Filename.concat out_dir (mangle n ^ ".ml"))
      (Printf.sprintf
         "(* GENERATED client entry for app %S — do not edit. Boots only this app. *)\nlet () = Fur_csr.start [ %s.Routes.mount ]\n"
         n (lib_mod n)))

let () =
  match Array.to_list Sys.argv with
  | _ :: "--glue" :: app_dir :: out_dir :: _ -> emit_glue app_dir out_dir
  | _ :: "--boots" :: apps_dir :: out_dir :: _ -> emit_boots apps_dir out_dir
  | _ :: apps_dir :: out_dir :: _ -> emit_routes apps_dir out_dir
  | _ -> prerr_endline "usage: route_gen --glue <app_dir> <out_dir> | --boots <apps_dir> <out_dir> | <apps_dir> <out_dir>"; exit 2
