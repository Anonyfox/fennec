(* Extract scoped [%%style] blocks from component .mlx files and emit ONE OCaml module. Two views of the
   SAME styles, both scoped under each component's data-fur hash (the hash the Fur ppx stamps on its
   elements):
     - [css]    : the scoped stylesheet string the server inlines into the document <style> (web).
     - [inline] : a flat [(scope, selector-key, declarations)] list for the SIMPLE rules (a single class
                  or a bare tag), which the email inliner ([Fur_email]) merges into [style=] attributes —
                  because email clients ignore <style>. Complex selectors (descendant, pseudo, …) stay in
                  [css] only; they're left to the <style> for clients that honor it.
   So a component's styles, authored once, drive BOTH web (<style>) and email (inlined) with no new source.

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

(* A selector the runtime inliner can key on without a CSS engine: a single class [.foo] -> ["foo"], or a
   bare tag [div] -> ["div"]. Anything else (descendant, pseudo, attribute, #id, compound) -> None and is
   left to the scoped <style>. This is the "flat selectors" half of the inline-styles contract. *)
let simple_key sel =
  let s = String.trim sel in
  let ident s =
    s <> ""
    && String.for_all
         (fun c ->
           (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || (c >= '0' && c <= '9') || c = '-')
         s
  in
  if String.length s >= 2 && s.[0] = '.' then
    let c = String.sub s 1 (String.length s - 1) in
    if ident c then Some c else None
  else if ident s then Some s
  else None

(* the flat (scope, key, decls) rules for the SIMPLE selectors in one component's raw [%%style] css *)
let inline_rules scope css =
  String.split_on_char '}' css
  |> List.concat_map (fun rule ->
         match String.index_opt rule '{' with
         | None -> []
         | Some k ->
           let sel = String.sub rule 0 k in
           let decls = String.trim (String.sub rule (k + 1) (String.length rule - k - 1)) in
           if String.trim sel = "" || decls = "" then []
           else
             String.split_on_char ',' sel
             |> List.filter_map (fun s ->
                    match simple_key s with Some key -> Some (scope, key, decls) | None -> None))

let extract path =
  let src = read path in
  match match between src "scss" with Some c -> Some c | None -> between src "css" with
  | None -> None
  | Some css ->
    let scope = "fur-" ^ String.sub (Digest.to_hex (Digest.string css)) 0 6 in
    Some (scope_css scope css, inline_rules scope css)
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
  let extracted = List.concat_map mlx_files dirs |> List.filter_map extract in
  let css = List.map fst extracted |> String.concat "\n" in
  let inline =
    List.concat_map snd extracted
    |> List.map (fun (s, k, d) -> Printf.sprintf "  (%S, %S, %S);" s k d)
    |> String.concat "\n"
  in
  Out_channel.with_open_bin out (fun oc ->
      Out_channel.output_string oc
        (Printf.sprintf
           "(* GENERATED — inlined, scoped component styles. do not edit. *)\n\
            let css = {furcss|%s|furcss}\n\n\
            (* (scope, selector-key, declarations) for SIMPLE rules — what the email inliner (Fur_email)\n\
           \   keys on; complex selectors live in [css] only. *)\n\
            let inline = [\n%s\n]\n"
           css inline))
