open Discover_model

let short_doc ?(limit = 140) = function
  | None -> ""
  | Some s ->
    let one = String.split_on_char '\n' s |> List.map String.trim |> String.concat " " in
    if String.length one <= limit then one else String.sub one 0 (limit - 1) ^ "..."

let item_line (i : public_item) =
  let doc = short_doc ~limit:86 i.doc in
  let detail = if doc = "" then kind_to_string i.kind else doc in
  Printf.sprintf "  %-34s %s  (%s)" i.path detail (Source_ref.to_string i.source)

let evidence_line (e : evidence) =
  let clean_line s =
    s |> String.trim
    |> fun s ->
    let s =
      if String.length s >= 2 && String.sub s 0 2 = "(*" then
        String.sub s 2 (String.length s - 2) |> String.trim
      else s
    in
    let s =
      if String.length s >= 2 && String.sub s (String.length s - 2) 2 = "*)" then
        String.sub s 0 (String.length s - 2) |> String.trim
      else s
    in
    s
  in
  let useful s =
    let s = clean_line s in
    s <> "" && s <> "." && s <> "fun () ->" && s <> "()" && String.exists (fun c -> Char.code c >= Char.code 'A' && Char.code c <= Char.code 'z') s
  in
  let likely_continuation s =
    let s = clean_line s in
    String.length s > 0 && s.[0] >= 'a' && s.[0] <= 'z' && not (String.starts_with ~prefix:"let " s)
  in
  let label = clean_line e.label in
  let candidates =
    (if useful label && not (likely_continuation label) then [ label ] else [])
    @ (e.text |> String.split_on_char '\n' |> List.map clean_line)
    @ [ label ]
  in
  let excerpt =
    candidates
    |> List.find_opt useful
    |> Option.value ~default:(clean_line e.label)
    |> fun s -> short_doc ~limit:118 (Some s)
    |> fun s -> if s = "" then evidence_kind_to_string e.kind else s
  in
  Printf.sprintf "  %-7s %s  (%s)" (evidence_kind_to_string e.kind) excerpt (Source_ref.to_string e.source)

let render_list title lines =
  match lines with
  | [] -> []
  | xs -> (title ^ ":") :: xs

let render_starter = function
  | None -> []
  | Some code -> [ "Starter:"; "```ocaml" ] @ String.split_on_char '\n' code @ [ "```" ]

let render = function
  | Plan { task; answer; steps; uses; evidence; avoid; confidence; reason; next } ->
    let step_lines = List.mapi (fun i s -> Printf.sprintf "  %d. %s" (i + 1) s) steps in
    String.concat "\n"
      ([
         "Task: " ^ task;
         "";
         "Answer:";
         "  " ^ answer.summary;
       ]
      @ [ "" ]
      @ render_list "Why" (List.map (fun s -> "  - " ^ s) answer.why)
      @ [ "" ]
      @ render_starter answer.starter
      @ [ "" ]
      @ [
         "Recommended path:";
       ]
      @ step_lines
      @ [ "" ]
      @ render_list "Use" (List.map item_line uses)
      @ [ "" ]
      @ render_list "Receipts" (List.map evidence_line evidence)
      @ [ "" ]
      @ render_list "Avoid unless" (List.map (fun s -> "  " ^ s) avoid)
      @ [
          "";
          Printf.sprintf "Confidence: %s - %s" (confidence_to_string confidence) reason;
        ]
      @ render_list "Next" (List.map (fun s -> "  " ^ s) (if answer.copy_next = [] then next else answer.copy_next))
      @ [ "" ])
  | Compare { task; answer; left; right; axis; left_when; right_when; evidence; confidence; next } ->
    String.concat "\n"
      ([
         "Task: " ^ task;
         "";
         "Answer:";
         "  " ^ answer.summary;
         "";
         "Compare: " ^ axis;
         "Use " ^ left.path ^ " when:";
         "  " ^ left_when;
         "Use " ^ right.path ^ " when:";
         "  " ^ right_when;
         "";
       ]
      @ render_list "Receipts" (List.map evidence_line evidence)
      @ [
          "";
          Printf.sprintf "Confidence: %s" (confidence_to_string confidence);
        ]
      @ render_list "Next" (List.map (fun s -> "  " ^ s) (if answer.copy_next = [] then next else answer.copy_next))
      @ [ "" ])
  | Browse { module_path; summary; items; evidence; next } ->
    let item_lines =
      List.map
        (fun i ->
          let doc = short_doc i.doc in
          Printf.sprintf "  %-38s %-11s %s" i.path (kind_to_string i.kind) doc)
        items
    in
    String.concat "\n"
      ([
         "Browse: " ^ module_path;
         "";
         summary;
         "";
       ]
      @ render_list "Public surface" item_lines
      @ [ "" ]
      @ render_list "Evidence" (List.map evidence_line evidence)
      @ [ "" ]
      @ render_list "Next" (List.map (fun s -> "  " ^ s) next)
      @ [ "" ])
  | Why { id; title; body; source; next } ->
    String.concat "\n"
      ([
         "Why: " ^ id;
         "";
         title;
       ]
      @ List.map (fun s -> "  " ^ s) body
      @ (match source with Some s -> [ ""; "Source: " ^ Source_ref.to_string s ] | None -> [])
      @ [ "" ]
      @ render_list "Next" (List.map (fun s -> "  " ^ s) next)
      @ [ "" ])
  | Insufficient { task; reason; suggestions; inspect } ->
    String.concat "\n"
      ([
         "Task: " ^ task;
         "";
         "Confidence: insufficient - " ^ reason;
         "";
       ]
      @ render_list "Try" (List.map (fun s -> "  " ^ s) suggestions)
      @ [ "" ]
      @ render_list "Inspect" (List.map (fun c -> Printf.sprintf "  %s  %s" c.id (Source_ref.to_string c.source)) inspect)
      @ [ "" ])
