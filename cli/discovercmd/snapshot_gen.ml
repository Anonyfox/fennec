open Discover_model

type interface_input = {
  package : string;
  library : string;
  root : string;
  file : string;
}

let read_file path =
  try Some (In_channel.with_open_bin path In_channel.input_all) with _ -> None

let rec collect acc dir suffix =
  match Sys.readdir dir with
  | exception _ -> acc
  | entries ->
    Array.fold_left
      (fun acc name ->
        let path = Filename.concat dir name in
        if name = "_build" || name = "node_modules" || (String.length name > 0 && name.[0] = '.') then acc
        else if (try Sys.is_directory path with _ -> false) then collect acc path suffix
        else if Filename.check_suffix path suffix then path :: acc
        else acc)
      acc entries

let collect_direct dir suffix =
  match Sys.readdir dir with
  | exception _ -> []
  | entries ->
    Array.fold_left
      (fun acc name ->
        let path = Filename.concat dir name in
        if (try Sys.is_directory path with _ -> false) then acc
        else if Filename.check_suffix path suffix then path :: acc
        else acc)
      [] entries

let rel ~root path =
  let prefix = if Filename.is_relative root then root else root in
  let n = String.length prefix in
  if String.length path > n && String.sub path 0 n = prefix then
    let start = if path.[n] = '/' then n + 1 else n in
    String.sub path start (String.length path - start)
  else path

let digest_string s = Digestif.SHA1.(to_hex (digest_string s))

let digest_file path = match read_file path with Some s -> digest_string s | None -> ""

let str_contains hay needle =
  let hn = String.length hay and nn = String.length needle in
  if nn = 0 then true
  else
    let rec go i =
      if i + nn > hn then false
      else if String.sub hay i nn = needle then true
      else go (i + 1)
    in
    go 0

let package_of_path path =
  if path = "hunt" || (String.length path >= 5 && String.sub path 0 5 = "hunt/") then "fennec-hunt"
  else if path = "mongo" || (String.length path >= 6 && String.sub path 0 6 = "mongo/") then "fennec-mongo"
  else if path = "cli" || (String.length path >= 4 && String.sub path 0 4 = "cli/") then "fennec-cli"
  else "fennec"

let library_of_public_name = function
  | "fennec" -> "fennec"
  | p -> p

let module_name s = String.capitalize_ascii (String.map (function '-' | '.' -> '_' | c -> c) s)

let wrapped_false text = str_contains text "(wrapped false)"

let stanza_atom name text =
  let marker = "(" ^ name in
  let len = String.length text and m = String.length marker in
  let rec find i =
    if i + m >= len then None
    else if String.sub text i m = marker then
      let j = ref (i + m) in
      while !j < len && (text.[!j] = ' ' || text.[!j] = '\n' || text.[!j] = '\t') do
        incr j
      done;
      let k = ref !j in
      while !k < len && text.[!k] <> ')' && text.[!k] <> ' ' && text.[!k] <> '\n' && text.[!k] <> '\t' do
        incr k
      done;
      Some (String.sub text !j (!k - !j))
    else find (i + 1)
  in
  find 0

let public_name_of_dune text =
  stanza_atom "public_name" text

let library_name_of_dune text = stanza_atom "name" text

let root_for_interface ~dune_text ~public_name ~file =
  let file_root = Doc_extract.root_module_of_file file in
  if public_name = "fennec" then "Fennec"
  else if public_name = "fennec.fur" then "Fur"
  else if public_name = "fennec.fur.platform" then "Fur.Platform"
  else if public_name = "fennec.fur.client" then file_root
  else if public_name = "fennec.pulse" then "Pulse"
  else if public_name = "fennec.pulse.live" then "Pulse.Live"
  else if public_name = "fennec.pulse.live.client" then "Pulse.Live.Client"
  else if public_name = "fennec.pulse.server" then "Pulse.Server"
  else if public_name = "fennec.pulse.mongo" then "Pulse.Mongo"
  else if wrapped_false dune_text then file_root
  else
    let wrapper =
      match library_name_of_dune dune_text with
      | Some name -> module_name name
      | None -> module_name public_name
    in
    if file_root = wrapper then wrapper else wrapper ^ "." ^ file_root

let interface_inputs ~root =
  let dunes = collect [] root "dune" in
  List.concat_map
    (fun dune ->
      match read_file dune with
      | None -> []
      | Some dune_text -> (
        match public_name_of_dune dune_text with
        | None -> []
        | Some public_name ->
        let dir = Filename.dirname dune in
        let package = package_of_path (rel ~root dir) in
        let library = library_of_public_name public_name in
        let mlis = collect_direct dir ".mli" in
        List.map
          (fun file ->
            { package; library; root = root_for_interface ~dune_text ~public_name ~file; file })
          mlis))
    dunes

let line_of_offset s off =
  let line = ref 1 in
  for i = 0 to min off (String.length s) - 1 do
    if s.[i] = '\n' then incr line
  done;
  !line

type mention_candidate = {
  api : public_item;
  short : string;
  suffix : string;
  short_ok : bool;
  facade : int;
}

type mention_index = (string, (mention_candidate * int) list) Hashtbl.t

let api_candidate (api : public_item) =
  let path = api.path in
  let short =
    match List.rev (String.split_on_char '.' path) with x :: _ -> x | [] -> path
  in
  let suffix =
    match List.rev (String.split_on_char '.' path) with
    | a :: b :: _ -> b ^ "." ^ a
    | _ -> short
  in
  let generic = [ "run"; "get"; "set"; "str"; "bin"; "link"; "string"; "make"; "create"; "style"; "check"; "header"; "status"; "request"; "response" ] in
  let short_ok = String.length short >= 6 && not (List.mem (String.lowercase_ascii short) generic) in
  let facade =
    if str_contains path "Fennec." || str_contains path "Fur." || str_contains path "Pulse." || str_contains path "Fennec_hunt." then 4 else 0
  in
  { api; short; suffix; short_ok; facade }

let build_mention_index apis =
  let index = Hashtbl.create 2048 in
  let key words = String.concat "\000" words in
  let add words score candidate =
    let token = key words in
    if token <> "" then
      let current = Option.value (Hashtbl.find_opt index token) ~default:[] in
      Hashtbl.replace index token ((candidate, score) :: current)
  in
  List.iter
    (fun (api : public_item) ->
      let candidate = api_candidate api in
      let depth = List.length (String.split_on_char '.' api.path) in
      let short_words = Normalize.words candidate.short in
      let suffix_words = Normalize.words candidate.suffix in
      if candidate.short_ok || (api.kind = Module && String.length candidate.short >= 3) then
        add short_words (30 + candidate.facade - (depth * 2)) candidate;
      if suffix_words <> [] then
        add suffix_words (80 + String.length candidate.suffix + candidate.facade) candidate;
      let path_words = Normalize.words api.path in
      let path_len = List.length path_words in
      if path_len > 1 && path_len <= 5 then
        add path_words (120 + String.length api.path + candidate.facade) candidate)
    apis;
  index

let ngram_keys ?(max_n = 5) words =
  let arr = Array.of_list words in
  let len = Array.length arr in
  let keys = ref [] in
  for i = 0 to len - 1 do
    let parts = ref [] in
    for j = i to min (len - 1) (i + max_n - 1) do
      parts := arr.(j) :: !parts;
      keys := (List.rev !parts |> String.concat "\000") :: !keys
    done
  done;
  !keys

let first_api_mentions (index : mention_index) text =
  let best = Hashtbl.create 16 in
  Normalize.words text
  |> ngram_keys
  |> List.iter (fun key ->
         Option.value (Hashtbl.find_opt index key) ~default:[]
         |> List.iter (fun (candidate, score) ->
                match Hashtbl.find_opt best candidate.api.id with
                | Some old when old >= score -> ()
                | _ -> Hashtbl.replace best candidate.api.id score));
  Hashtbl.fold (fun id score acc -> (id, score) :: acc) best []
  |> List.sort (fun (_, a) (_, b) -> compare b a)
  |> List.map fst
  |> fun xs ->
  let rec take n = function [] -> [] | _ when n = 0 -> [] | x :: xs -> x :: take (n - 1) xs in
  take 12 xs

let evidence_id kind rel label text =
  let safe =
    label |> Normalize.words |> (function [] -> [ Filename.basename rel ] | xs -> xs)
    |> String.concat "_"
  in
  Printf.sprintf "%s:%s:%s#%s" kind (String.map (function '/' | '.' -> ':' | c -> c) rel) safe
    (String.sub (digest_string text) 0 6)

let label_near line_text =
  let s = String.trim line_text in
  if s = "" then Filename.basename s else if String.length s > 80 then String.sub s 0 80 else s

let window_text lines idx =
  let len = Array.length lines in
  let lo = max 0 (idx - 1) in
  let hi = min (len - 1) (idx + 4) in
  let acc = ref [] in
  for i = lo to hi do
    acc := Array.get lines i :: !acc
  done;
  !acc |> List.rev |> String.concat "\n"

let evidence_of_file ~root mention_index file =
  match read_file file with
  | None -> []
  | Some text ->
    let rel_path = rel ~root file in
    if Filename.check_suffix rel_path ".pp.ml"
       || Filename.check_suffix rel_path ".mlx.ml"
       || rel_path = "examples/site/assets.ml"
       || Filename.basename rel_path = "routes.ml"
       || Filename.basename rel_path = "paths.ml"
       || (String.length rel_path >= 21 && String.sub rel_path 0 21 = "examples/site/client/")
    then []
    else
    let package = package_of_path rel_path in
    let kind =
      if str_contains rel_path "/test/" || str_contains rel_path "_test.ml" then Test
      else Example
    in
    let lines = String.split_on_char '\n' text |> Array.of_list in
    let rec loop line_no acc = function
      | idx when idx >= Array.length lines -> List.rev acc
      | idx ->
        let line = Array.get lines idx in
        let window = window_text lines idx in
        let apis_hit = first_api_mentions mention_index window in
        let interesting =
          apis_hit <> []
          || str_contains line "let%test"
          || str_contains line "let%http"
          || str_contains line "let%browser"
          || str_contains line "let%system"
          || str_contains line "Router.page"
          || str_contains line "pipe_matched"
        in
        let acc =
          if interesting && not (str_contains line "GENERATED") then
            let label = label_near line in
            {
              id = evidence_id (evidence_kind_to_string kind) rel_path label line;
              kind;
              package;
              label;
              text = window;
              apis = apis_hit;
              source = Source_ref.make ~path:rel_path ~line:line_no ();
            }
            :: acc
          else acc
        in
        loop (line_no + 1) acc (idx + 1)
    in
    loop 1 [] 0

let route_evidence ~root file =
  let rel_path = rel ~root file in
  let parts = String.split_on_char '/' rel_path in
  match List.rev parts with
  | file_name :: rest when Filename.check_suffix file_name ".mlx" && file_name <> "main.mlx" && file_name <> "layout.mlx" ->
    let basename = Filename.chop_suffix file_name ".mlx" in
    let rec after_apps = function
      | [] -> None
      | "apps" :: _app :: xs -> Some xs
      | _ :: xs -> after_apps xs
    in
    let prefix =
      match after_apps (List.rev rest) with
      | Some xs -> xs
      | None -> []
    in
    let path = Route_facts.route_path ~prefix ~basename in
    let name = Route_facts.typed_path_name ~prefix ~basename in
    Some
      {
        id = evidence_id "route" rel_path name path;
        kind = Route;
        package = "fennec";
        label = Printf.sprintf "%s -> %s" rel_path path;
        text =
          Printf.sprintf "%s builds typed path %s%s" path name
            (if str_contains path ":" || str_contains path "*" then " for a dynamic route" else "");
        apis = [ "api:Fur.Router" ];
        source = Source_ref.make ~path:rel_path ~line:1 ~generated:true ();
      }
  | _ -> None

let doc_evidence items =
  List.filter_map
    (fun item ->
      match item.doc with
      | None -> None
      | Some doc when str_contains doc "{@ocaml" ->
        Some
          {
            id = evidence_id "doctest" item.source.path item.path doc;
            kind = Doctest;
            package = item.package;
            label = "doc example for " ^ item.path;
            text = doc;
            apis = [ item.id ];
            source = item.source;
          }
      | Some doc when str_contains (String.lowercase_ascii doc) "must" || str_contains (String.lowercase_ascii doc) "raise" ->
        Some
          {
            id = evidence_id "hazard" item.source.path item.path doc;
            kind = Hazard;
            package = item.package;
            label = "constraint for " ^ item.path;
            text = doc;
            apis = [ item.id ];
            source = item.source;
          }
      | _ -> None)
    items

let packages (items : public_item list) =
  let names = List.sort_uniq String.compare (List.map (fun (i : public_item) -> i.package) items) in
  List.map
    (fun name ->
      let corpus =
        items
        |> List.filter (fun (i : public_item) -> i.package = name)
        |> List.map (fun (i : public_item) -> i.path ^ Option.value i.doc ~default:"")
        |> String.concat "\n"
      in
      { name; version = "0.0.1"; digest = digest_string corpus })
    names

let api_text (i : public_item) =
  String.concat " "
    [
      i.path;
      kind_to_string i.kind;
      i.package;
      i.library;
      Option.value i.signature ~default:"";
      Option.value i.doc ~default:"";
      i.source.path;
    ]

let api_index public_items =
  let postings = Hashtbl.create 2048 in
  List.iteri
    (fun idx item ->
      Normalize.query (api_text item)
      |> List.iter (fun term ->
             let current = Option.value (Hashtbl.find_opt postings term) ~default:[] in
             Hashtbl.replace postings term (idx :: current)))
    public_items;
  Hashtbl.fold
    (fun term refs acc -> { term; refs = List.rev refs |> Array.of_list } :: acc)
    postings []
  |> List.sort (fun a b -> compare a.term b.term)

let evidence_text (e : evidence) =
  String.concat " "
    [ e.label; e.text; e.package; evidence_kind_to_string e.kind; e.source.path; String.concat " " e.apis ]

let evidence_index evidence =
  let postings = Hashtbl.create 2048 in
  List.iteri
    (fun idx ev ->
      Normalize.query (evidence_text ev)
      |> List.iter (fun term ->
             let current = Option.value (Hashtbl.find_opt postings term) ~default:[] in
             Hashtbl.replace postings term (idx :: current)))
    evidence;
  Hashtbl.fold
    (fun term refs acc -> { term; refs = List.rev refs |> Array.of_list } :: acc)
    postings []
  |> List.sort (fun a b -> compare a.term b.term)

let api_evidence_index evidence =
  let postings = Hashtbl.create 2048 in
  List.iteri
    (fun idx ev ->
      ev.apis
      |> List.iter (fun api ->
             let current = Option.value (Hashtbl.find_opt postings api) ~default:[] in
             Hashtbl.replace postings api (idx :: current)))
    evidence;
  Hashtbl.fold
    (fun term refs acc -> { term; refs = List.rev refs |> Array.of_list } :: acc)
    postings []
  |> List.sort (fun a b -> compare a.term b.term)

let replace_prefix ~prefix ~with_ text =
  let n = String.length prefix in
  if String.length text > n && String.sub text 0 n = prefix && text.[n] = '.' then
    Some (with_ ^ String.sub text n (String.length text - n))
  else None

let facade_aliases =
  [
    ("Fennec_server.Endpoint", "Fennec.Endpoint");
    ("Fennec_paw.Conn", "Fennec.Conn");
    ("Fennec_core.Http", "Fennec.Http");
    ("Fennec_core.Cookie", "Fennec.Cookie");
  ]

let facade_items (items : public_item list) =
  List.concat_map
    (fun (prefix, with_) ->
      items
      |> List.filter_map (fun (item : public_item) ->
             match replace_prefix ~prefix ~with_ item.path with
             | None -> None
             | Some path ->
               Some
                 {
                   item with
                   id = "api:" ^ path;
                   package = "fennec";
                   library = "fennec";
                   path;
                 }))
    facade_aliases

let item_preference (i : public_item) =
  let kind =
    match i.kind with
    | Value -> 5
    | Module -> 4
    | Module_type -> 3
    | Type -> 2
    | Exception -> 1
  in
  let documented = match i.doc with Some doc when String.trim doc <> "" -> 2 | _ -> 0 in
  let facade =
    if str_contains i.path "Fennec." || str_contains i.path "Fur." || str_contains i.path "Pulse." || str_contains i.path "Fennec_hunt." then 1
    else 0
  in
  kind + documented + facade

let dedupe_public_items items =
  items
  |> List.sort (fun (a : public_item) (b : public_item) ->
         let by_id = compare a.id b.id in
         if by_id <> 0 then by_id else compare (item_preference b) (item_preference a))
  |> List.sort_uniq (fun (a : public_item) (b : public_item) -> compare a.id b.id)

let parent_paths path =
  match String.split_on_char '.' path with
  | [] | [ _ ] -> []
  | parts ->
    let rec go acc prefix = function
      | [] | [ _ ] -> List.rev acc
      | part :: rest ->
        let prefix = match prefix with "" -> part | p -> p ^ "." ^ part in
        go (prefix :: acc) prefix rest
    in
    go [] "" parts

let parent_module_items items =
  let existing = Hashtbl.create (List.length items) in
  List.iter (fun (i : public_item) -> Hashtbl.replace existing i.path ()) items;
  let parents = Hashtbl.create 128 in
  List.iter
    (fun (child : public_item) ->
      parent_paths child.path
      |> List.iter (fun path ->
             if not (Hashtbl.mem existing path || Hashtbl.mem parents path) then
               Hashtbl.replace parents path
                 {
                   id = "api:" ^ path;
                   package = child.package;
                   library = child.library;
                   path;
                   kind = Module;
                   signature = Some "module : sig ...";
                   doc = Some (path ^ " public surface");
                   source = child.source;
                 }))
    items;
  Hashtbl.fold (fun _ item acc -> item :: acc) parents []

let build ~root =
  let inputs = interface_inputs ~root in
  let public_items : public_item list =
    inputs
    |> List.concat_map (fun (i : interface_input) ->
           match read_file i.file with
           | None -> []
           | Some contents ->
             let file = rel ~root i.file in
             Doc_extract.parse_interface ~package:i.package ~library:i.library ~root:i.root ~file ~contents)
    |> dedupe_public_items
  in
  let public_items =
    public_items @ parent_module_items public_items |> dedupe_public_items
  in
  let public_items =
    public_items @ facade_items public_items |> dedupe_public_items
  in
  let example_files =
    List.concat
      [
        collect [] (Filename.concat root "examples/site") ".ml";
        collect [] (Filename.concat root "examples/site") ".mlx";
        collect [] (Filename.concat root "fennec") ".ml";
      ]
  in
  let mention_index = build_mention_index public_items in
  let evidence =
    List.concat_map (evidence_of_file ~root mention_index) example_files
    @ List.filter_map (route_evidence ~root) (collect [] (Filename.concat root "examples/site/frontend/apps") ".mlx")
    @ doc_evidence public_items
  in
  let evidence = List.sort_uniq (fun (a : evidence) (b : evidence) -> compare a.id b.id) evidence in
  {
    schema_version = Snapshot.schema_version;
    generated_at = "build";
    packages = packages public_items;
    public_items;
    api_index = api_index public_items;
    evidence;
    evidence_index = evidence_index evidence;
    api_evidence_index = api_evidence_index evidence;
  }

let ocaml_string_literal s = Printf.sprintf "%S" s

let emit_ocaml snapshot =
  let payload = Marshal.to_string snapshot [] in
  Printf.printf
    "let snapshot () = (Marshal.from_string %s 0 : \
     Fennec_discover_core.Discover_model.snapshot)\n"
    (ocaml_string_literal payload)
