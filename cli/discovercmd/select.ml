open Discover_model

let starts_with s prefix =
  String.length s >= String.length prefix && String.sub s 0 (String.length prefix) = prefix

let contains_sub ~needle haystack =
  let n = String.length needle and h = String.length haystack in
  let rec go i =
    i + n <= h && (String.sub haystack i n = needle || go (i + 1))
  in
  n = 0 || go 0

let take n xs =
  let rec go n acc = function
    | [] -> List.rev acc
    | _ when n <= 0 -> List.rev acc
    | x :: xs -> go (n - 1) (x :: acc) xs
  in
  go n [] xs

let uniq_items_by_id xs =
  let seen = Hashtbl.create 32 in
  List.filter
    (fun (i : public_item) ->
      if Hashtbl.mem seen i.id then false
      else (
        Hashtbl.add seen i.id ();
        true))
    xs

let leaf path =
  match List.rev (String.split_on_char '.' path) with
  | x :: _ -> x
  | [] -> path

let depth path = List.length (String.split_on_char '.' path)

let family path =
  match String.split_on_char '.' path with
  | "Fur" :: _ -> "Fur"
  | "Pulse" :: "Live" :: _ -> "Pulse.Live"
  | a :: b :: _ -> a ^ "." ^ b  (* Paw.Conn, Paw.Basic_auth, Fennec.Accounts, Pulse.Mongo, Fennec_hunt.Http, … *)
  | a :: _ -> a
  | [] -> path

let item_text (i : public_item) =
  String.concat " "
    [ i.path; kind_to_string i.kind; Option.value i.signature ~default:""; Option.value i.doc ~default:""; i.source.path ]

let token_hits terms text =
  let ws = Normalize.words text in
  List.fold_left (fun acc term -> if List.mem term ws then acc + 1 else acc) 0 terms

let phrase_hits terms path =
  let words = Normalize.words path in
  List.fold_left (fun acc term -> if List.mem term words then acc + 1 else acc) 0 terms

let is_facade_path path =
  starts_with path "Fennec." || starts_with path "Paw." || starts_with path "Fur." || starts_with path "Pulse." || starts_with path "Fennec_hunt."

let helper_leaf leaf =
  List.mem (String.lowercase_ascii leaf)
    [ "cookie"; "cookies"; "get"; "set"; "run"; "expect"; "status"; "header"; "headers"; "render"; "handle"; "changed"; "aggregate" ]

(* the segment that NAMES the containing module — the concept the API belongs to. *)
let module_name path =
  match List.rev (String.split_on_char '.' path) with _ :: m :: _ -> m | _ -> ""

(* Generic concept match, the replacement for any hand-maintained "this task → this exact API" table:
   reward query terms that hit the API's LEAF (the action — [send_chunked], [set_cookie]) and its
   containing MODULE (the concept — [Session], [Basic_auth], [Csrf]). This is what makes "add signed
   sessions" surface [Paw.Session(.make)] and "basic auth" surface [Paw.Basic_auth] from the path
   alone — derived from the indexed `.mli` structure, nothing per-task baked in. *)
let name_match_score terms path =
  let count ws = List.fold_left (fun acc term -> if List.mem term ws then acc + 1 else acc) 0 terms in
  let leaf_hits = count (Normalize.words (leaf path)) in
  let mod_hits = count (Normalize.words (module_name path)) in
  (float_of_int leaf_hits *. 26.0) +. (float_of_int mod_hits *. 22.0)

(* PURPOSE match: query terms found in the API's `.mli` doc. This is what disambiguates a coincidental
   name collision from the real intent — "local counter" hits [Fur.Data.local] by leaf but [Fur.signal]
   by DOC ("create a local reactive signal"), and the doc is the truth. Capped so a long doc can't run
   away with it. *)
let doc_match_score terms (i : public_item) =
  let hits = token_hits terms (Option.value i.doc ~default:"") in
  float_of_int (min hits 4) *. 11.0

let first_leaf_term_position terms path =
  let leaf_words = Normalize.words (leaf path) in
  let rec go index = function
    | [] -> None
    | term :: rest ->
      if List.mem term leaf_words then Some index else go (index + 1) rest
  in
  go 0 terms

let task_order_bonus terms path =
  match first_leaf_term_position terms path with
  | None -> 0.0
  | Some index -> max 0.0 (32.0 -. (float_of_int index *. 4.0))

let kind_bonus = function
  | Value -> 5.0
  | Module -> 4.0
  | Type -> -.4.0
  | Module_type -> -.2.0
  | Exception -> -.8.0

let module_anchor_bonus terms (i : public_item) =
  match i.kind with
  | Module when depth i.path <= 3 && token_hits terms (item_text i) >= 2 -> 16.0
  | Module when depth i.path <= 2 && token_hits terms (item_text i) >= 1 -> 8.0
  | _ -> 0.0

let internal_doc_penalty (i : public_item) =
  match i.doc with
  | None -> 0.0
  | Some doc ->
    let text = String.lowercase_ascii doc in
    if
      starts_with text "{1 internal"
      || token_hits [ "internal" ] text > 0
      || contains_sub ~needle:"not stable" text
      || contains_sub ~needle:"do not depend" text
      || contains_sub ~needle:"subject to change" text
    then -.180.0
    else 0.0

(* test-intent terms: when the task is about TESTING, the test library is exactly what you want;
   otherwise it is for proving a feature, not building one, so it should not crowd the recommendation. *)
let test_intent terms =
  List.exists (fun t -> List.mem t terms) [ "test"; "tests"; "testing"; "assert"; "assertion"; "expect"; "spec"; "suite" ]

let score_item terms seed_ids api_rank (i : public_item) =
  let rank_bonus =
    match Hashtbl.find_opt api_rank i.id with
    | None -> 0.0
    | Some rank -> max 0.0 (24.0 -. (float_of_int rank *. 0.6))
  in
  (* an API that a query-matching EXAMPLE actually uses is strong evidence it's the right one — this is
     the bridge when the user's words ("counter") differ from the API name ("signal"). *)
  let seed_bonus = if List.mem i.id seed_ids then 50.0 else 0.0 in
  let helper_penalty =
    if helper_leaf (leaf i.path) && phrase_hits terms i.path <= 1 && token_hits terms (Option.value i.doc ~default:"") < 2 then -.28.0
    else 0.0
  in
  (* the test package (fennec-hunt) answers test tasks, not build tasks — one library-role rule. Strong
     enough to sink a test assertion that lexically matches a build query ("response body" → response_body). *)
  let test_tool_penalty = if i.package = "fennec-hunt" && not (test_intent terms) then -.75.0 else 0.0 in
  name_match_score terms i.path
  +. doc_match_score terms i
  +. test_tool_penalty
  +. seed_bonus
  +. rank_bonus
  +. task_order_bonus terms i.path
  +. (float_of_int (token_hits terms (item_text i)) *. 7.0)
  +. (float_of_int (phrase_hits terms i.path) *. 9.0)
  +. kind_bonus i.kind
  +. module_anchor_bonus terms i
  +. (if is_facade_path i.path then 8.0 else -.20.0)
  -. (float_of_int (max 0 (depth i.path - 3)) *. 6.0)
  +. helper_penalty
  +. internal_doc_penalty i

(* How many items of one API family may share the visible list. A flat small cap keeps the card
   diverse (the set_cookie/delete_cookie pair, the endpoint/pipe_matched pair) without a per-family
   table — two members of a family is enough to orient, the rest is drilldown. *)
let family_cap _terms _fam = 2

let select_diverse terms limit scored =
  let counts = Hashtbl.create 16 in
  let rec go acc = function
    | [] -> List.rev acc
    | _ when List.length acc >= limit -> List.rev acc
    | (item, _) :: rest ->
      let fam = family item.path in
      let count = Option.value (Hashtbl.find_opt counts fam) ~default:0 in
      if count >= family_cap terms fam then go acc rest
      else (
        Hashtbl.replace counts fam (count + 1);
        go (item :: acc) rest)
  in
  go [] scored

let plan_uses ~terms ~more ~api_results ~evidence_seed_items ~public_items =
  ignore public_items;
  let api_rank = Hashtbl.create 64 in
  List.iteri (fun rank r -> Hashtbl.replace api_rank r.Retrieve.item.id rank) api_results;
  let seed_ids = List.map (fun (i : public_item) -> i.id) evidence_seed_items in
  (* candidates are the retrieved APIs (lexical match over the whole index) plus the APIs that
     source-backed evidence points at — re-ranked for PRESENTATION by [score_item]. No hand-seeded
     "this query always includes X" list: if an API belongs on the card, retrieval or its evidence
     surfaced it. *)
  let candidates =
    evidence_seed_items @ (api_results |> take 80 |> List.map (fun r -> r.Retrieve.item))
    |> uniq_items_by_id
    |> List.filter (fun i -> is_facade_path i.path)
  in
  candidates
  |> List.map (fun i -> (i, score_item terms seed_ids api_rank i))
  |> List.sort (fun (_, a) (_, b) -> compare b a)
  |> select_diverse terms (if more then 8 else 4)

let words_of s = Normalize.query s

let split_compare_task task =
  let lower = String.lowercase_ascii task in
  let separators = [ " vs "; " versus "; " when "; " choose " ] in
  let rec find_sep = function
    | [] -> None
    | sep :: rest -> (
      match String.index_from_opt lower 0 sep.[1] with
      | None -> find_sep rest
      | Some _ ->
        let n = String.length sep in
        let rec scan i =
          if i + n > String.length lower then None
          else if String.sub lower i n = sep then Some (i, n)
          else scan (i + 1)
        in
        match scan 0 with Some _ as x -> x | None -> find_sep rest)
  in
  match find_sep separators with
  | None -> (words_of task, words_of task)
  | Some (i, n) ->
    let left = String.sub task 0 i in
    let right = String.sub task (i + n) (String.length task - i - n) in
    (words_of left, words_of right)

let best_for_terms terms items =
  let fake_rank = Hashtbl.create 16 in
  items
  |> List.map (fun i -> (i, score_item terms [] fake_rank i))
  |> List.sort (fun (_, a) (_, b) -> compare b a)
  |> List.map fst

let prefer_compare_order terms a b =
  let has x = List.mem x terms in
  if (has "local" || has "state") && family a.path = "Fur" then (a, b)
  else if (has "local" || has "state") && family b.path = "Fur" then (b, a)
  else (a, b)

let compare_pair ~task ~terms ~uses ~public_items =
  let left_terms, right_terms = split_compare_task task in
  let diverse_families items =
    items
    |> List.map (fun i -> family i.path)
    |> List.sort_uniq String.compare
    |> List.length
  in
  let selected_pool = uses |> List.filter (fun i -> is_facade_path i.path) |> uniq_items_by_id in
  let pool =
    if diverse_families selected_pool >= 2 then selected_pool
    else
      uses @ public_items
      |> uniq_items_by_id
      |> List.filter (fun i ->
           is_facade_path i.path
           &&
           let fam = family i.path in
           List.exists (fun u -> family u.path = fam) uses)
  in
  let left_candidates = best_for_terms left_terms pool in
  let right_candidates = best_for_terms right_terms pool in
  match left_candidates with
  | [] -> None
  | left :: _ -> (
    match List.find_opt (fun i -> family i.path <> family left.path) right_candidates with
    | None -> (
      match List.find_opt (fun i -> family i.path <> family left.path) pool with
      | None -> None
      | Some right -> Some (prefer_compare_order terms left right))
    | Some right -> Some (prefer_compare_order terms left right))

let%test "selects action leaves for response cookie task" =
  let src = Source_ref.make ~path:"x" ~line:1 () in
  let item path doc = { id = "api:" ^ path; package = "fennec"; library = "fennec"; path; kind = Value; signature = None; doc = Some doc; source = src } in
  let set = item "Paw.Conn.set_cookie" "Set a response cookie" in
  let del = item "Paw.Conn.delete_cookie" "Expire a response cookie" in
  let read = item "Paw.Conn.cookie" "Read a request cookie" in
  let api_results = List.map (fun item -> ({ Retrieve.item; score = 1.0; coverage = 1.0 } : Retrieve.api_result)) [ read; set; del ] in
  match plan_uses ~terms:(Normalize.query "set and delete a response cookie") ~more:false ~api_results ~evidence_seed_items:[] ~public_items:[ read; set; del ] with
  | a :: b :: _ -> a.path = "Paw.Conn.set_cookie" && b.path = "Paw.Conn.delete_cookie"
  | _ -> false
