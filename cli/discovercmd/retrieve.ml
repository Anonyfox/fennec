open Discover_model

type api_result = {
  item : public_item;
  score : float;
  coverage : float;
}

type evidence_result = {
  ev : evidence;
  score : float;
  coverage : float;
}

type doc = {
  text : string;
}

type runtime = {
  snapshot : snapshot;
  public_items : public_item array;
  evidence : evidence array;
  api_by_term : (string, int array) Hashtbl.t;
  evidence_by_term : (string, int array) Hashtbl.t;
  evidence_by_api : (string, int array) Hashtbl.t;
  item_by_id : (string, public_item) Hashtbl.t;
  advisory : (public_item * public_item * string) list;
  owner_items : (string, public_item array) Hashtbl.t;
}

let contains_sub ~needle haystack =
  let needle = String.lowercase_ascii needle in
  let haystack = String.lowercase_ascii haystack in
  let n = String.length needle and h = String.length haystack in
  let rec go i =
    i + n <= h && (String.sub haystack i n = needle || go (i + 1))
  in
  n = 0 || go 0

let starts_with s prefix =
  String.length s >= String.length prefix && String.sub s 0 (String.length prefix) = prefix

let take n xs =
  let rec go n acc = function
    | [] -> List.rev acc
    | _ when n <= 0 -> List.rev acc
    | x :: xs -> go (n - 1) (x :: acc) xs
  in
  go n [] xs

let uniq_by key xs =
  let seen = Hashtbl.create 32 in
  List.filter
    (fun x ->
      let k = key x in
      if Hashtbl.mem seen k then false
      else (
        Hashtbl.add seen k ();
        true))
    xs

let words text = Normalize.words text

let table_of_postings postings =
  let table = Hashtbl.create (max 16 (List.length postings * 2)) in
  List.iter (fun p -> Hashtbl.replace table p.term p.refs) postings;
  table

let item_doc (i : public_item) =
  {
    text =
      String.concat " "
        [
          i.path;
          kind_to_string i.kind;
          i.package;
          i.library;
          Option.value i.signature ~default:"";
          Option.value i.doc ~default:"";
          i.source.path;
        ];
  }

let split_sentences text =
  text
  |> String.map (function '.' | '\n' -> '\n' | c -> c)
  |> String.split_on_char '\n'
  |> List.map String.trim
  |> List.filter (( <> ) "")

let extract_doc_refs text =
  let len = String.length text in
  let rec go i acc =
    if i + 2 >= len then List.rev acc
    else if text.[i] = '{' && text.[i + 1] = '!' then
      match String.index_from_opt text (i + 2) '}' with
      | None -> List.rev acc
      | Some j ->
        let raw = String.sub text (i + 2) (j - i - 2) |> String.trim in
        let target =
          raw
          |> String.split_on_char ' '
          |> List.filter (( <> ) "")
          |> function
          | x :: _ -> x
          | [] -> raw
        in
        go (j + 1) (target :: acc)
    else go (i + 1) acc
  in
  go 0 []

let advisory_sentences doc =
  split_sentences doc
  |> List.filter (fun sentence ->
         let ws = words sentence in
         let raw = sentence |> String.lowercase_ascii |> String.split_on_char ' ' in
         List.mem "prefer" ws || List.mem "instead" ws || List.mem "use" raw)

let suffix_match ~suffix path =
  path = suffix
  || (String.length path > String.length suffix
     && String.sub path (String.length path - String.length suffix) (String.length suffix) = suffix)

let root_of_path path =
  match String.split_on_char '.' path with
  | root :: _ -> root
  | [] -> path

let resolve_ref public_items ~(source : public_item) target =
  let source_root = root_of_path source.path in
  let qualified = contains_sub ~needle:"." target in
  public_items
  |> List.filter (fun (i : public_item) -> suffix_match ~suffix:target i.path)
  |> List.sort (fun (a : public_item) (b : public_item) ->
         let score i =
           (if starts_with i.path "Fennec." || starts_with i.path "Fur." || starts_with i.path "Pulse." || starts_with i.path "Fennec_hunt." then 4.0
            else if starts_with i.path "Fennec_" then -2.0
            else 0.0)
           +. (if (not qualified) && root_of_path i.path = source_root then 12.0 else 0.0)
           +. match i.kind with Module -> 2.0 | Value -> 1.0 | Type -> 0.4 | Exception -> 0.0 | Module_type -> 0.2
         in
         compare (score b) (score a))
  |> function
  | x :: _ -> Some x
  | [] -> None

let advisory_edges public_items =
  public_items
  |> List.concat_map (fun (source : public_item) ->
         match source.doc with
         | None -> []
         | Some doc ->
           advisory_sentences doc
           |> List.concat_map (fun sentence ->
                  extract_doc_refs sentence
                  |> List.filter_map (fun target ->
                         resolve_ref public_items ~source target
                         |> Option.map (fun item -> (source, item, sentence)))))

let runtime_cache : runtime option ref = ref None

let runtime snapshot =
  match !runtime_cache with
  | Some rt when rt.snapshot == snapshot -> rt
  | _ ->
    let public_items = Array.of_list snapshot.public_items in
    let evidence = Array.of_list snapshot.evidence in
    let item_by_id = Hashtbl.create (Array.length public_items * 2) in
    Array.iter
      (fun (item : public_item) ->
        Hashtbl.replace item_by_id item.id item;
        Hashtbl.replace item_by_id item.path item)
      public_items;
    let rt =
      {
        snapshot;
        public_items;
        evidence;
        api_by_term = table_of_postings snapshot.api_index;
        evidence_by_term = table_of_postings snapshot.evidence_index;
        evidence_by_api = table_of_postings snapshot.api_evidence_index;
        item_by_id;
        advisory = advisory_edges snapshot.public_items;
        owner_items = Hashtbl.create 32;
      }
    in
    runtime_cache := Some rt;
    rt

let find_item rt id = Hashtbl.find_opt rt.item_by_id id

let find_api snapshot id = find_item (runtime snapshot) id

let owner_items rt prefix =
  match Hashtbl.find_opt rt.owner_items prefix with
  | Some items -> items
  | None ->
    let items =
      rt.public_items
      |> Array.to_seq
      |> Seq.filter (fun (i : public_item) -> starts_with i.path prefix)
      |> Array.of_seq
    in
    Hashtbl.replace rt.owner_items prefix items;
    items

let evidence_doc (e : evidence) =
  {
    text =
      String.concat " "
        [
          e.label;
          e.text;
          e.package;
          evidence_kind_to_string e.kind;
          e.source.path;
          String.concat " " e.apis;
        ];
  }

let coverage terms doc_words doc_text =
  ignore doc_text;
  let matched =
    List.fold_left
      (fun acc term ->
        if List.mem term doc_words then acc + 1
        else acc)
      0 terms
  in
  match terms with [] -> 0.0 | _ -> float_of_int matched /. float_of_int (List.length terms)

let path_coverage terms path = coverage terms (words path) path

let token_hits terms text =
  let ws = words text in
  List.fold_left (fun acc term -> if List.mem term ws then acc + 1 else acc) 0 terms

let leaf path =
  match List.rev (String.split_on_char '.' path) with
  | x :: _ -> x
  | [] -> path

let leaf_match terms path = List.mem (String.lowercase_ascii (leaf path)) terms

let source_owner_prefixes (ev : evidence) =
  let path = ev.source.path in
  let route_owner = match ev.kind with Route -> [ "Fur.Router" ] | _ -> [] in
  let path_owners =
    if starts_with path "examples/site/test/http/" || starts_with path "hunt/http" then [ "Fennec_hunt.Http" ]
    else if starts_with path "examples/site/test/browser/" || starts_with path "hunt/live" then [ "Fennec_hunt.Live" ]
    else if starts_with path "examples/site/test/system/" || starts_with path "hunt/system" then [ "Fennec_hunt.System" ]
    else if starts_with path "fennec/server/basic_auth" then [ "Fennec.Paw.Basic_auth" ]
    else if starts_with path "fennec/server/session" then [ "Fennec.Paw.Session" ]
    else if starts_with path "fennec/server/endpoint" then [ "Fennec.Endpoint" ]
    else if starts_with path "fennec/paw/conn" then [ "Fennec.Conn" ]
    else if starts_with path "fennec/core/cookie" then [ "Fennec.Cookie" ]
    else if starts_with path "fennec/fur/tools/route_gen" then [ "Fur.Router" ]
    else if starts_with path "examples/site/frontend/apps/" then [ "Fur.Router"; "Fur" ]
    else if starts_with path "fennec/fur/core/fur" || starts_with path "fennec/fur/server/" || starts_with path "examples/site/frontend/components/counter" then [ "Fur" ]
    else if starts_with path "examples/site/frontend/components/task_list" then [ "Pulse.Live"; "Fur" ]
    else if starts_with path "fennec/pulse/live" || starts_with path "examples/site/e2e/realtime" then [ "Pulse.Live" ]
    else []
  in
  uniq_by Fun.id (route_owner @ path_owners)

let inferred_source_apis rt terms (ev : evidence) =
  let prefixes = source_owner_prefixes ev in
  if prefixes = [] then []
  else
    prefixes
    |> List.concat_map (fun prefix -> Array.to_list (owner_items rt prefix))
    |> uniq_by (fun (i : public_item) -> i.id)
    |> List.sort (fun (a : public_item) (b : public_item) ->
           let score i =
             (token_hits terms (leaf i.path) * 4)
             + (token_hits terms i.path * 2)
             + if List.mem i.path prefixes then 5 else 0
             + match i.kind with Module -> 3 | Value -> 1 | Type -> 0 | Exception -> -1 | Module_type -> 0
           in
           compare (score b) (score a))
    |> take 12

let field_score terms ~path ~doc ~signature ~kind =
  let path_words = words path in
  let doc_words = words doc in
  let sig_words = words signature in
  let kind_words = words kind in
  List.fold_left
    (fun acc term ->
      acc
      +. (if List.mem term path_words then 4.0 else 0.0)
      +. (if List.mem term doc_words then 2.0 else 0.0)
      +. (if List.mem term sig_words then 0.8 else 0.0)
      +. (if List.mem term kind_words then 0.4 else 0.0)
      +. if contains_sub ~needle:term path then 1.5 else 0.0)
    0.0 terms

let doc_score terms doc =
  let doc_words = words doc.text in
  let lexical =
    List.fold_left
      (fun acc term ->
        acc
        +. if List.mem term doc_words then 1.5 else 0.0)
      0.0 terms
  in
  lexical

let source_kind_boost = function
  | Test -> 1.4
  | Example -> 1.2
  | Doctest -> 1.0
  | Route -> 1.0
  | Hazard -> -0.8

let evidence_kind_match terms kind =
  let k = evidence_kind_to_string kind in
  if List.mem k terms then 5.0 else 0.0

let public_prior path =
  let facade =
    if starts_with path "Fennec." then 2.4
    else if starts_with path "Fur." then 2.2
    else if starts_with path "Pulse." then 2.0
    else if starts_with path "Fennec_hunt." then 1.8
    else if starts_with path "Fennec_" then -1.8
    else if starts_with path "Query." then -1.2
    else 0.0
  in
  let depth = List.length (String.split_on_char '.' path) in
  let shallow = if depth <= 2 then 1.2 else if depth = 3 then 0.3 else -.2.5 in
  facade +. shallow

let is_facade path =
  starts_with path "Fennec." || starts_with path "Fur." || starts_with path "Pulse." || starts_with path "Fennec_hunt."

let posting_refs table term =
  match Hashtbl.find_opt table term with
  | Some refs -> refs
  | None -> [||]

let indexed_refs table terms =
  let term_refs =
    terms
    |> List.filter_map (fun term ->
           match posting_refs table term with
           | [||] -> None
           | refs -> Some (term, refs))
    |> List.sort (fun (_, a) (_, b) -> compare (Array.length a) (Array.length b))
  in
  let term_refs = if List.length term_refs <= 2 then term_refs else take 2 term_refs in
  let seen = Hashtbl.create 64 in
  term_refs
  |> List.concat_map (fun (_, refs) -> Array.to_list refs)
  |> List.filter (fun idx ->
         if Hashtbl.mem seen idx then false
         else (
           Hashtbl.add seen idx ();
           true))

let indexed_public_items rt terms =
  if Hashtbl.length rt.api_by_term = 0 then Array.to_list rt.public_items
  else
    indexed_refs rt.api_by_term terms
    |> List.filter_map (fun idx ->
           if idx >= 0 && idx < Array.length rt.public_items then Some rt.public_items.(idx) else None)

let indexed_evidence rt terms =
  if Hashtbl.length rt.evidence_by_term = 0 then Array.to_list rt.evidence
  else
    indexed_refs rt.evidence_by_term terms
    |> List.filter_map (fun idx ->
           if idx >= 0 && idx < Array.length rt.evidence then Some rt.evidence.(idx) else None)

let api_linked_evidence rt api_ids =
  if Hashtbl.length rt.evidence_by_api = 0 then []
  else
    let seen = Hashtbl.create 32 in
    api_ids
    |> List.concat_map (fun api_id -> Array.to_list (posting_refs rt.evidence_by_api api_id))
    |> List.filter_map (fun idx ->
           if Hashtbl.mem seen idx then None
           else (
             Hashtbl.add seen idx ();
             if idx >= 0 && idx < Array.length rt.evidence then Some rt.evidence.(idx) else None))

let evidence_channel_uncached (snapshot : snapshot) terms : evidence_result list =
  let rt = runtime snapshot in
  let enough_coverage cov source_cov =
    List.length terms <= 2 || cov >= 0.40 || source_cov >= 0.20
  in
  indexed_evidence rt terms
  |> List.map (fun ev ->
         let d = evidence_doc ev in
         let cov = coverage terms (words d.text) d.text in
         let source_cov = path_coverage terms ev.source.path in
         let score = (doc_score terms d +. source_kind_boost ev.kind +. evidence_kind_match terms ev.kind +. (source_cov *. 12.0)) *. (0.20 +. cov +. (source_cov *. 1.8)) in
         ({ ev; score; coverage = cov } : evidence_result))
  |> List.filter (fun (r : evidence_result) ->
         let source_cov = path_coverage terms r.ev.source.path in
         enough_coverage r.coverage source_cov)
  |> List.filter (fun (r : evidence_result) -> r.score > 0.6)
  |> List.sort (fun (a : evidence_result) (b : evidence_result) -> compare b.score a.score)

let evidence_cache : (string * evidence_result list) option ref = ref None

let evidence_cache_key (snapshot : snapshot) terms =
  String.concat "\000"
    (string_of_int (List.length snapshot.public_items) :: string_of_int (List.length snapshot.evidence) :: terms)

let evidence_channel (snapshot : snapshot) terms : evidence_result list =
  let key = evidence_cache_key snapshot terms in
  match !evidence_cache with
  | Some (cached_key, results) when cached_key = key -> results
  | _ ->
    let results = evidence_channel_uncached snapshot terms in
    evidence_cache := Some (key, results);
    results

let direct_api_channel rt terms : api_result list =
  indexed_public_items rt terms
  |> List.map (fun item ->
         let d = item_doc item in
         let cov = coverage terms (words d.text) d.text in
         let path_cov = path_coverage terms item.path in
         let kind_prior = match item.kind with Module -> 1.0 | Value -> 0.3 | Type -> 0.0 | Exception -> -0.2 | Module_type -> 0.0 in
         let score =
           ((field_score terms ~path:item.path
               ~doc:(Option.value item.doc ~default:"")
               ~signature:(Option.value item.signature ~default:"")
               ~kind:(kind_to_string item.kind)
             +. doc_score terms d)
            *. (0.20 +. cov +. (path_cov *. 1.5)))
           +. public_prior item.path +. kind_prior
         in
         ({ item; score; coverage = max cov path_cov } : api_result))
  |> List.filter (fun (r : api_result) -> r.score > 0.8)
  |> List.sort (fun (a : api_result) (b : api_result) -> compare b.score a.score)

let rrf ?(k = 60.0) rank = 1.0 /. (k +. float_of_int rank)

let apis snapshot terms =
  let rt = runtime snapshot in
  let direct = direct_api_channel rt terms in
  let evs = evidence_channel snapshot terms |> take 80 in
  let advisory = rt.advisory in
  let evidence_owners =
    evs |> take 20 |> List.concat_map (fun (r : evidence_result) -> source_owner_prefixes r.ev) |> uniq_by Fun.id
  in
  let by_api = Hashtbl.create 128 in
  let advisory_penalty = Hashtbl.create 32 in
  List.iteri
    (fun rank (r : api_result) ->
      Hashtbl.replace by_api r.item.id
        (r.item, r.score +. (80.0 *. rrf (rank + 1)), r.coverage))
    direct;
  List.iter
    (fun ((source : public_item), (target : public_item), sentence) ->
      let sentence_cov = coverage terms (words sentence) sentence in
      let sentence_hits = token_hits terms sentence in
      if source.id <> target.id && (sentence_cov >= 0.34 || sentence_hits >= 2) then (
        let old_score, old_cov =
          match Hashtbl.find_opt by_api target.id with
          | Some (_, s, c) -> (s, c)
          | None -> (0.0, 0.0)
        in
        let target_hits = token_hits terms target.path in
        let leaf_boost = if leaf_match terms target.path then 10.0 else 0.0 in
        let boost = (sentence_cov *. 180.0) +. float_of_int (target_hits * 16) +. (leaf_boost *. 2.0) in
        Hashtbl.replace by_api target.id (target, old_score +. boost, max old_cov sentence_cov);
        let penalty = Option.value (Hashtbl.find_opt advisory_penalty source.id) ~default:1.0 in
        Hashtbl.replace advisory_penalty source.id (min penalty 0.30)))
    advisory;
  List.iteri
    (fun rank (r : evidence_result) ->
      let inferred = inferred_source_apis rt terms r.ev |> List.map (fun (i : public_item) -> i.id) in
      let propagate weight api_id =
          match find_item rt api_id with
          | None -> ()
          | Some item ->
            let old_score, old_cov =
              match Hashtbl.find_opt by_api api_id with
              | Some (_, s, c) -> (s, c)
              | None -> (0.0, 0.0)
            in
            let propagated = weight *. ((r.score *. (0.35 +. r.coverage)) +. ((70.0 *. r.coverage) *. rrf (rank + 1))) in
            Hashtbl.replace by_api api_id (item, old_score +. propagated, max old_cov r.coverage)
      in
      List.iter (propagate 10.0) r.ev.apis;
      List.iter (propagate 1.0) inferred)
    evs;
  Hashtbl.fold
    (fun _ (item, score, coverage) acc ->
      let path_cov = path_coverage terms item.path in
      let leaf_hits = token_hits terms (leaf item.path) in
      let path_hits = token_hits terms item.path in
      let specificity = 0.45 +. (coverage *. 0.8) +. (path_cov *. 1.4) in
      let weak_single_match = List.length terms >= 3 && coverage < 0.40 && path_cov < 0.40 in
      let broad_accessor = List.length terms >= 3 && leaf_hits = 1 && path_hits <= 2 in
      let owner_aligned =
        evidence_owners = [] || List.exists (fun prefix -> starts_with item.path prefix) evidence_owners
      in
      let exact_owner = List.exists (fun prefix -> item.path = prefix) evidence_owners in
      let score =
        ((score *. specificity) +. public_prior item.path +. (float_of_int leaf_hits *. 12.0) +. (float_of_int path_hits *. 2.0))
        *. (if weak_single_match then 0.35 else 1.0)
        *. if broad_accessor then 0.55 else 1.0
      in
      let score = score *. Option.value (Hashtbl.find_opt advisory_penalty item.id) ~default:1.0 in
      let score =
        if evidence_owners = [] then score
        else if exact_owner then score +. 14.0
        else if owner_aligned then score +. 4.0
        else score *. 0.35
      in
      let support = max coverage path_cov in
      let enough_support = List.length terms <= 2 || support >= 0.40 || leaf_hits >= 2 in
      if score > 1.2 && enough_support then ({ item; score; coverage = support } : api_result) :: acc else acc)
    by_api []
  |> fun results ->
  let facade_results = List.filter (fun r -> is_facade r.item.path) results in
  (if List.length facade_results >= 4 then facade_results else results)
  |> List.sort (fun (a : api_result) (b : api_result) -> compare b.score a.score)

let evidence snapshot terms api_results =
  let rt = runtime snapshot in
  let api_ids = List.map (fun r -> r.item.id) api_results in
  let selected_prefixes =
    api_results
    |> List.map (fun r ->
           match String.split_on_char '.' r.item.path with
           | a :: b :: _ -> a ^ "." ^ b
           | a :: _ -> a
           | [] -> r.item.path)
    |> uniq_by Fun.id
  in
  let linked_to_selected ev = List.exists (fun id -> List.mem id api_ids) ev.apis in
  let near_selected ev =
    List.exists
      (fun prefix -> List.exists (fun api -> contains_sub ~needle:prefix api) ev.apis || contains_sub ~needle:prefix ev.source.path)
      selected_prefixes
  in
  let score_ev ev =
    let d = evidence_doc ev in
    let cov = coverage terms (words d.text) d.text in
    let source_cov = path_coverage terms ev.source.path in
    let score = (doc_score terms d +. source_kind_boost ev.kind +. evidence_kind_match terms ev.kind +. (source_cov *. 12.0)) *. (0.20 +. cov +. (source_cov *. 1.8)) in
    ({ ev; score; coverage = cov } : evidence_result)
  in
  let graph_evidence =
    if api_results = [] then []
    else
      api_linked_evidence rt api_ids
      |> List.map score_ev
  in
  evidence_channel snapshot terms @ graph_evidence
  |> uniq_by (fun r -> r.ev.id)
  |> List.map (fun r ->
         let graph = if linked_to_selected r.ev then 4.0 else if near_selected r.ev then 1.2 else 0.0 in
         { r with score = r.score +. graph })
  |> List.filter (fun (r : evidence_result) -> r.score > 1.2 && r.ev.kind <> Hazard)
  |> List.sort (fun (a : evidence_result) (b : evidence_result) -> compare b.score a.score)

let test_item ?doc ?(kind = Value) path =
  {
    id = "api:" ^ path;
    package = "fennec";
    library = "fennec.fur";
    path;
    kind;
    signature = Some "val x";
    doc;
    source = Source_ref.make ~path:"x.mli" ~line:1 ();
  }

let%test "advisory use transfers ranking to referenced API" =
  let data =
    test_item ~kind:Module
      ~doc:
        "Async data resources. Use {!Data} for request/SSR data. Use {!signal} for local browser state such as a counter."
      "Fur.Data"
  in
  let signal =
    test_item
      ~doc:"[signal init] creates local reactive UI state for counters and button clicks."
      "Fur.signal"
  in
  let system_signal =
    test_item ~doc:"A process signal helper." "Fennec_hunt.System.signal"
  in
  let snapshot =
    {
      schema_version = 1;
      generated_at = "test";
      packages = [];
      public_items = [ data; system_signal; signal ];
      api_index = [];
      evidence = [];
      evidence_index = [];
      api_evidence_index = [];
    }
  in
  match apis snapshot (Normalize.query "local counter") with
  | top :: _ -> top.item.path = "Fur.signal"
  | [] -> false
