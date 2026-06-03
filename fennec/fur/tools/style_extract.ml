(* Extract scoped [%%style] blocks from component .mlx files and emit ONE OCaml module
   (let css : string) — every component's CSS scoped under its own data-fur hash, the
   SAME hash the Fur ppx stamps onto that component's elements. The server inlines this
   string into the document <style>, so components stay single-file (styles colocated in
   the .mlx) with no external per-component stylesheet.

   usage: style_extract <out.ml> <dir>...   (dirs scanned recursively for *.mlx) *)
let read f = In_channel.with_open_bin f In_channel.input_all
let find s sub from =
  let n = String.length s and m = String.length sub in
  let rec go i = if i + m > n then -1 else if String.sub s i m = sub then i else go (i + 1) in
  go from
let between src d =
  let o = "{" ^ d ^ "|" and c = "|" ^ d ^ "}" in
  let i = find src o 0 in
  if i < 0 then None
  else
    let j = find src c (i + String.length o) in
    if j < 0 then None else Some (String.sub src (i + String.length o) (j - (i + String.length o)))
(* suffix every selector with [data-fur="<scope>"] so a component's rules only hit its
   own elements. Flat rules only (no scss nesting) — matches the inline-styles contract. *)
let scope_css scope css =
  String.split_on_char '}' css
  |> List.filter_map (fun rule ->
         match String.index_opt rule '{' with
         | None -> None
         | Some k ->
           let sel = String.trim (String.sub rule 0 k) in
           if sel = "" then None
           else
             let decls = String.trim (String.sub rule (k + 1) (String.length rule - k - 1)) in
             let sels =
               String.split_on_char ',' sel
               |> List.map (fun s -> String.trim s ^ Printf.sprintf "[data-fur=\"%s\"]" scope)
               |> String.concat ", "
             in
             Some (Printf.sprintf "%s { %s }" sels decls))
  |> String.concat "\n"
let extract path =
  let src = read path in
  match match between src "scss" with Some c -> Some c | None -> between src "css" with
  | None -> None
  | Some css ->
    let scope = "fur-" ^ String.sub (Digest.to_hex (Digest.string css)) 0 6 in
    Some (scope_css scope css)
let rec mlx_files dir =
  Sys.readdir dir |> Array.to_list |> List.sort compare
  |> List.concat_map (fun n ->
         let f = Filename.concat dir n in
         if Sys.is_directory f then mlx_files f
         else if Filename.check_suffix n ".mlx" then [ f ]
         else [])
let () =
  let out = Sys.argv.(1) in
  let dirs = Array.to_list Sys.argv |> List.filteri (fun i _ -> i >= 2) in
  let css = List.concat_map mlx_files dirs |> List.filter_map extract |> String.concat "\n" in
  Out_channel.with_open_bin out (fun oc ->
      Out_channel.output_string oc
        (Printf.sprintf "(* GENERATED — inlined, scoped component styles. do not edit. *)\nlet css = {furcss|%s|furcss}\n" css))
