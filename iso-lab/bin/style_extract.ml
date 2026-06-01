(* Extract scoped style blocks from EVERY component file, scope each under its
   own component hash, emit one stylesheet per component with the right extension. *)
let read f = In_channel.with_open_bin f In_channel.input_all
let find s sub from =
  let n = String.length s and m = String.length sub in
  let rec go i = if i + m > n then -1 else if String.sub s i m = sub then i else go (i+1) in go from
let between src d =
  let o = "{"^d^"|" and c = "|"^d^"}" in
  let i = find src o 0 in
  if i < 0 then None else let j = find src c (i+String.length o) in
    if j < 0 then None else Some (String.sub src (i+String.length o) (j-(i+String.length o)))
let scope_css scope css =
  String.split_on_char '}' css
  |> List.filter_map (fun rule -> match String.index_opt rule '{' with None -> None | Some k ->
       let sel = String.trim (String.sub rule 0 k) in
       if sel="" then None else
       let decls = String.trim (String.sub rule (k+1) (String.length rule-k-1)) in
       let sels = String.split_on_char ',' sel
         |> List.map (fun s -> String.trim s ^ Printf.sprintf "[data-iso=\"%s\"]" scope)
         |> String.concat ", " in
       Some (Printf.sprintf "%s { %s }" sels decls))
  |> String.concat "\n"
let process outdir path =
  let src = read path in
  match (match between src "scss" with Some c -> Some ("scss",c) | None ->
          (match between src "css" with Some c -> Some ("css",c) | None -> None)) with
  | None -> ()
  | Some (ext, css) ->
    let scope = "iso-" ^ String.sub (Digest.to_hex (Digest.string css)) 0 6 in
    let name = Filename.remove_extension (Filename.basename path) in
    Out_channel.with_open_bin (Filename.concat outdir (name ^ "." ^ ext))
      (fun oc -> Out_channel.output_string oc (scope_css scope css ^ "\n"))
let () =
  let outdir = Sys.argv.(1) and srcdir = Sys.argv.(2) in
  (try Unix.mkdir outdir 0o755 with _ -> ());
  Sys.readdir srcdir |> Array.to_list |> List.sort compare
  |> List.iter (fun f -> if Filename.extension f = ".mlx" then process outdir (Filename.concat srcdir f))
