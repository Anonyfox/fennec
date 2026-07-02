open Discover_model

let take n xs =
  let rec go n acc = function
    | [] -> List.rev acc
    | _ when n <= 0 -> List.rev acc
    | x :: xs -> go (n - 1) (x :: acc) xs
  in
  go n [] xs

let contains_sub ~needle haystack =
  let needle = String.lowercase_ascii needle in
  let haystack = String.lowercase_ascii haystack in
  let n = String.length needle and h = String.length haystack in
  let rec go i =
    i + n <= h && (String.sub haystack i n = needle || go (i + 1))
  in
  n = 0 || go 0

let uniq_items_by_id xs =
  let seen = Hashtbl.create 16 in
  List.filter
    (fun (i : public_item) ->
      if Hashtbl.mem seen i.id then false
      else (
        Hashtbl.add seen i.id ();
        true))
    xs

let limit_evidence_per_source max_per_source xs =
  let counts = Hashtbl.create 16 in
  List.filter
    (fun (e : evidence) ->
      let n = Option.value (Hashtbl.find_opt counts e.source.path) ~default:0 in
      if n >= max_per_source then false
      else (
        Hashtbl.replace counts e.source.path (n + 1);
        true))
    xs

let family path =
  match String.split_on_char '.' path with
  | a :: b :: _ -> a ^ "." ^ b
  | a :: _ -> a
  | [] -> path

let leaf path =
  match List.rev (String.split_on_char '.' path) with
  | x :: _ -> x
  | [] -> path

let starts_with s prefix =
  String.length s >= String.length prefix && String.sub s 0 (String.length prefix) = prefix

let is_facade path =
  starts_with path "Fennec." || starts_with path "Paw." || starts_with path "Fur." || starts_with path "Pulse." || starts_with path "Fennec_hunt."

let depth path = List.length (String.split_on_char '.' path)

let token_hits terms text =
  let ws = Normalize.words text in
  List.fold_left (fun acc term -> if List.mem term ws then acc + 1 else acc) 0 terms

let item_text (i : public_item) =
  String.concat " " [ i.path; Option.value i.signature ~default:""; Option.value i.doc ~default:"" ]

let presentation_score terms seed_ids api_rank (i : public_item) =
  let rank_bonus =
    match Hashtbl.find_opt api_rank i.id with
    | None -> 0.0
    | Some rank -> max 0.0 (8.0 -. (float_of_int rank *. 0.25))
  in
  let kind_bonus = match i.kind with Value -> 4.0 | Module -> 3.0 | Type -> 0.5 | Module_type -> 1.0 | Exception -> 0.0 in
  (float_of_int (token_hits terms (item_text i)) *. 10.0)
  +. (float_of_int (token_hits terms i.path) *. 14.0)
  +. (if List.mem i.id seed_ids then 50.0 else 0.0)
  +. rank_bonus +. kind_bonus

let evidence_text (e : evidence) =
  String.concat " " [ e.label; e.text; e.source.path; String.concat " " e.apis ]

let evidence_coverage terms e =
  let ws = Normalize.words (evidence_text e) in
  let hits = List.fold_left (fun acc term -> if List.mem term ws then acc + 1 else acc) 0 terms in
  match terms with [] -> 0.0 | _ -> float_of_int hits /. float_of_int (List.length terms)

let evidence_has_proof evidence =
  List.exists
    (fun e ->
      match e.kind with
      | Example | Test | Doctest | Route -> true
      | Hazard -> false)
    evidence

let confidence (top : Retrieve.api_result option) (second : Retrieve.api_result option) evidence =
  match top with
  | None -> Conf_insufficient
  | Some top ->
    let margin = match second with None -> top.score | Some s -> top.score -. s.score in
    let proof = evidence_has_proof evidence in
    if top.score >= 7.0 && top.coverage >= 0.45 && margin >= 0.5 && proof then High
    else if top.score >= 4.0 && top.coverage >= 0.28 && proof then Medium
    else if top.score >= 3.0 && top.coverage >= 0.20 then Low
    else Conf_insufficient

let confidence_reason = function
  | High -> "public API, docs, and example/test evidence agree"
  | Medium -> "public API matched with supporting evidence"
  | Low -> "some public evidence matched, but confidence is limited"
  | Conf_insufficient -> "no public Fennec API matched strongly enough"

let mentions terms xs = List.exists (fun x -> List.mem x terms) xs

(* ── generic answer / steps / evidence synthesis ───────────────────────────────────────────────
   There is NO task taxonomy. A card's prose is synthesized from the selected public APIs, their own
   `.mli` docs, and the linked evidence — so a new framework feature needs no new branch here: index
   its `.mli` + an example and discover describes it. *)

(* first sentence of an odoc doc string, lightly de-marked, for one-line prose. *)
let doc_lead = function
  | None -> ""
  | Some doc ->
    let one = doc |> String.split_on_char '\n' |> List.map String.trim |> String.concat " " |> String.trim in
    let s = Normalize.odoc_plain one in
    (match String.index_opt s '.' with
     | Some i when i >= 12 -> String.sub s 0 (i + 1)
     | _ -> if String.length s <= 150 then s else String.sub s 0 150 ^ "…")

let first_use uses = match uses with (i : public_item) :: _ -> i.path | [] -> "the highest-ranked public API"

let next_for_query task (uses : public_item list) =
  ignore task;
  match uses with
  | [] -> []
  | i :: rest ->
    ("fennec discover --why " ^ i.id)
    :: (rest |> take 2 |> List.map (fun (i : public_item) -> "fennec discover --why " ^ i.id))

(* a module's primary constructor — the value you actually start with ([Session.make], [Csrf.make],
   …). Generic: probe the conventional constructor leaves against the index, no per-module table. *)
let constructor_of snapshot (m : public_item) =
  [ "make"; "create"; "v"; "init"; "default"; "empty" ]
  |> List.find_map (fun c -> Retrieve.find_api snapshot ("api:" ^ m.path ^ "." ^ c))

(* the starter snippet is a REAL signature from the index — the anchor's own when it's a callable, or
   its module's constructor when the anchor is a module. Source-truthful; no hand-written code templates
   to keep in sync with the framework. *)
let starter_for snapshot uses =
  let sig_of (i : public_item) = match i.signature with Some s when String.trim s <> "" -> Some s | _ -> None in
  match uses with
  | (i : public_item) :: _ -> (
    match i.kind with
    | Value | Type | Exception -> sig_of i
    | Module | Module_type -> Option.bind (constructor_of snapshot i) sig_of)
  | [] -> None

let evidence_proof_note evidence =
  if List.exists (fun (e : evidence) -> e.kind = Test) evidence then [ "A framework test backs this." ]
  else if List.exists (fun (e : evidence) -> e.kind = Example || e.kind = Doctest || e.kind = Route) evidence then [ "A framework example backs this." ]
  else []

let answer_for_plan snapshot task uses evidence =
  let anchor_lead = match uses with (i : public_item) :: _ -> doc_lead i.doc | [] -> "" in
  let summary =
    match uses with
    | (i : public_item) :: _ -> (
      match anchor_lead with "" -> Printf.sprintf "Reach for %s for %s." i.path task | lead -> Printf.sprintf "Reach for %s — %s" i.path lead)
    | [] -> Printf.sprintf "No strong public API matched %s." task
  in
  let why =
    (* skip the anchor's own doc (already the summary) so Why adds the NEXT API's purpose + proof *)
    let docs =
      uses
      |> List.filter_map (fun (i : public_item) -> match doc_lead i.doc with "" -> None | s -> if s = anchor_lead then None else Some s)
      |> take 2
    in
    take 3 (docs @ evidence_proof_note evidence)
  in
  { summary; why; starter = starter_for snapshot uses; copy_next = next_for_query task uses }

let plan_steps uses evidence =
  let api_step (i : public_item) =
    match doc_lead i.doc with "" -> Printf.sprintf "Reach for %s." i.path | lead -> Printf.sprintf "Reach for %s — %s" i.path lead
  in
  let api_steps = uses |> take 2 |> List.map api_step in
  let proof_step =
    match List.find_opt (fun (e : evidence) -> e.kind = Test || e.kind = Example || e.kind = Route) evidence with
    | Some e -> [ Printf.sprintf "Follow the worked shape in %s." (Source_ref.to_string e.source) ]
    | None -> [ "Run `--why` on the API above before editing if its signature is unfamiliar." ]
  in
  match api_steps with [] -> [ "Browse the closest public module and follow its example evidence." ] | _ -> api_steps @ proof_step

(* the decision axis for a compare is each side's OWN purpose (its doc) — no hand-coded axis table. *)
let compare_axis left right =
  ( "fit",
    (match doc_lead left.doc with "" -> Printf.sprintf "Use %s when it fits the task directly." left.path | s -> s),
    (match doc_lead right.doc with "" -> Printf.sprintf "Use %s for the adjacent concern." right.path | s -> s) )

let answer_for_compare task left right =
  ignore task;
  let _, left_when, right_when = compare_axis left right in
  {
    summary = Printf.sprintf "Choose %s or %s by which fits — each one's purpose and proof are below." left.path right.path;
    why = List.filter (fun s -> s <> "") [ left_when; right_when ];
    starter = None;
    copy_next = [ "fennec discover --why " ^ left.id; "fennec discover --why " ^ right.id ];
  }

let avoid_notes evidence =
  evidence |> List.filter_map (fun (e : evidence) -> match e.kind with Hazard -> Some e.text | _ -> None) |> take 2

(* evidence ranking for the card: linkage to a selected API + query token overlap + retrieval score +
   a kind prior. No per-file or per-task constants. *)
let evidence_card_score terms selected_ids (r : Retrieve.evidence_result) =
  let e = r.Retrieve.ev in
  let linked = List.exists (fun api -> Hashtbl.mem selected_ids api) e.apis in
  let overlap = token_hits terms (String.concat " " [ e.label; e.text; e.source.path ]) in
  (if linked then 24.0 else 0.0)
  +. (float_of_int overlap *. 8.0)
  +. (r.score *. 3.0)
  +. (match e.kind with Test -> 8.0 | Example -> 6.0 | Route -> 5.0 | Doctest -> 4.0 | Hazard -> -.20.0)

(* keep an evidence item on the default card if it proves a selected API or genuinely overlaps the
   query — generic, no per-task gate. *)
let evidence_presentable terms selected_ids (e : evidence) =
  let linked = List.exists (fun api -> Hashtbl.mem selected_ids api) e.apis in
  linked || token_hits terms (String.concat " " [ e.label; e.text; e.source.path ]) >= 2

let find_api = Retrieve.find_api

(* dedupe an evidence list by id, keeping first occurrence (order-preserving). *)
let uniq_evidence_by_id xs =
  let seen = Hashtbl.create 16 in
  List.filter
    (fun (e : evidence) ->
      if Hashtbl.mem seen e.id then false else (Hashtbl.add seen e.id (); true))
    xs

let compare_card snapshot task terms uses evidence =
  match Select.compare_pair ~task ~terms ~uses ~public_items:snapshot.public_items with
  | None -> None
  | Some (left, right) ->
    let axis, left_when, right_when = compare_axis left right in
    let answer = answer_for_compare task left right in
    (* a compare must show evidence from BOTH sides; the generic plan evidence skews to whichever
       side the query's rarest terms hit (e.g. all Pulse, no Fur). For each compared API pull its
       linked evidence and pick the CANONICAL usage example — the one that names the API's leaf and is
       an example component — then put one of each FIRST, so the card always proves both alternatives. *)
    let best_linked (item : public_item) =
      let leaf = String.lowercase_ascii (leaf item.path) in
      let canonical (e : evidence) =
        (if contains_sub ~needle:leaf (String.lowercase_ascii (e.label ^ " " ^ e.text)) then 10 else 0)
        + (match e.kind with
           | Example -> if starts_with e.source.path "examples/site/web/components" then 8 else 4
           | Test -> 3
           | _ -> 0)
      in
      Retrieve.evidence snapshot terms [ ({ item; score = 10.0; coverage = 1.0 } : Retrieve.api_result) ]
      |> List.map (fun (r : Retrieve.evidence_result) -> r.ev)
      |> List.stable_sort (fun a b -> compare (canonical b) (canonical a))
      |> take 1
    in
    let evidence = uniq_evidence_by_id (best_linked left @ best_linked right @ evidence) in
    Some
      (Compare
         {
           task;
           answer;
           left;
           right;
           axis;
           left_when;
           right_when;
           evidence;
           confidence = Medium;
           next = answer.copy_next;
         })

let browse snapshot module_path ~more =
  let prefix = module_path ^ "." in
  let items =
    snapshot.public_items
    |> List.filter (fun i -> i.path = module_path || (String.length i.path > String.length prefix && String.sub i.path 0 (String.length prefix) = prefix))
    |> List.sort (fun a b -> compare a.path b.path)
    |> take (if more then 40 else 16)
  in
  let evidence =
    snapshot.evidence
    |> List.filter (fun (e : evidence) -> List.exists (fun api -> List.exists (fun (i : public_item) -> i.id = api) items) e.apis)
    |> take (if more then 6 else 2)
  in
  let summary =
    match List.find_opt (fun i -> i.path = module_path) snapshot.public_items with
    | Some i -> Option.value i.doc ~default:(module_path ^ " public surface")
    | None -> module_path ^ " public surface"
  in
  Browse { module_path; summary; items; evidence; next = [ "fennec discover --more --browse " ^ module_path ] }

let why snapshot id =
  let api_match (i : public_item) =
    i.id = id || String.length i.id >= String.length id && String.sub i.id 0 (String.length id) = id
  in
  match List.find_opt api_match snapshot.public_items with
  | Some i ->
    Why
      {
        id = i.id;
        title = i.path;
        body =
          List.filter (( <> ) "")
            [
              kind_to_string i.kind ^ " from package " ^ i.package;
              Option.value i.signature ~default:"";
              Option.value i.doc ~default:"";
            ];
        source = Some i.source;
        next = [ "fennec discover --browse " ^ i.path ];
      }
  | None -> (
    match List.find_opt (fun (e : evidence) -> e.id = id || String.length e.id >= String.length id && String.sub e.id 0 (String.length id) = id) snapshot.evidence with
    | Some e ->
      Why
        {
          id = e.id;
          title = e.label;
          body = [ evidence_kind_to_string e.kind ^ " evidence"; e.text ];
          source = Some e.source;
          next = List.map (fun api -> "fennec discover --why " ^ api) e.apis;
        }
    | None ->
      let suggestions =
        snapshot.public_items |> List.map (fun (i : public_item) -> i.id) |> List.filter (fun x -> Normalize.contains_word ~word:(String.lowercase_ascii id) x) |> take 5
      in
      Insufficient
        {
          task = id;
          reason = "no current discover id matched";
          suggestions = (if suggestions = [] then [ "fennec discover \"task phrase\"" ] else suggestions);
          inspect = [];
        })

let query snapshot ~more task =
  let terms = Normalize.query task in
  let ordered_terms = Normalize.words task in
  let api_results = Retrieve.apis snapshot terms in
  let matched_seed_evidence =
    Retrieve.evidence snapshot terms []
    |> take 80
    |> List.map (fun r -> r.Retrieve.ev)
  in
  let seed_evidence =
    matched_seed_evidence
    |> List.fold_left
         (fun (acc : evidence list) (e : evidence) ->
           if List.exists (fun (x : evidence) -> x.id = e.id) acc then acc else e :: acc)
         []
    |> List.rev
  in
  let evidence_seed_items : public_item list =
    (List.map (fun (e : evidence) -> (e, evidence_coverage terms e)) seed_evidence)
    |> List.concat_map (fun ((ev : evidence), cov) ->
           ev.apis
           |> List.filter_map (find_api snapshot)
           |> List.filter_map (fun (i : public_item) ->
                  if
                    is_facade i.path
                    && (token_hits terms i.path > 0 || (cov >= 0.45 && depth i.path <= 2))
                  then
                    let leaf_hit = if Normalize.contains_word ~word:(String.lowercase_ascii (leaf i.path)) (evidence_text ev) then 1 else 0 in
                    let score =
                      (cov *. 10.0)
                      +. float_of_int (token_hits terms i.path * 20)
                      +. float_of_int (token_hits terms ev.source.path * 15)
                      +. float_of_int (leaf_hit * 28)
                      -. float_of_int (depth i.path)
                    in
                    Some (i, score)
                  else None))
    |> List.sort (fun (_, a) (_, b) -> compare b a)
    |> List.map fst
    |> uniq_items_by_id
    |> take 4
  in
  let uses : public_item list =
    Select.plan_uses ~terms:ordered_terms ~more ~api_results ~evidence_seed_items ~public_items:snapshot.public_items
  in
  let evidence_uses = uses in
  let selected_results =
    let selected = Hashtbl.create (List.length evidence_uses * 2) in
    List.iter (fun (i : public_item) -> Hashtbl.replace selected i.id ()) evidence_uses;
    api_results
    |> List.filter (fun r -> Hashtbl.mem selected r.Retrieve.item.id)
  in
  let evidence : evidence list =
    let max_per_source = if more then 2 else 1 in
    let selected_ids = Hashtbl.create (List.length evidence_uses * 2) in
    List.iter (fun (i : public_item) -> Hashtbl.replace selected_ids i.id ()) evidence_uses;
    Retrieve.evidence snapshot terms selected_results
    |> List.map (fun r -> (r.Retrieve.ev, evidence_card_score terms selected_ids r))
    |> List.filter (fun (e, _) -> more || evidence_presentable terms selected_ids e)
    |> List.sort (fun (_, a) (_, b) -> compare b a)
    |> List.map fst
    |> limit_evidence_per_source max_per_source
    |> take (if more then 8 else if mentions terms [ "vs"; "versus"; "choose"; "when" ] then 2 else 3)
  in
  let top = match api_results with x :: _ -> Some x | [] -> None in
  let second = match api_results with _ :: x :: _ -> Some x | _ -> None in
  let conf = confidence top second evidence in
  if conf = Conf_insufficient then
    Insufficient
      {
        task;
        reason = confidence_reason conf;
        suggestions = [ "fennec discover --browse Fennec"; "fennec discover \"SSR page\""; "fennec discover \"HTTP test\"" ];
        inspect =
          (Retrieve.evidence snapshot terms []
          |> take 3
          |> List.map (fun r -> { id = r.Retrieve.ev.id; label = r.ev.label; source = r.ev.source }));
      }
  else if mentions terms [ "vs"; "versus"; "choose"; "when" ] then (
    match compare_card snapshot task ordered_terms uses evidence with
    | Some c -> c
    | None ->
      Plan
        {
          task;
          answer = answer_for_plan snapshot task uses evidence;
          steps = plan_steps uses evidence;
          uses;
          evidence;
          avoid = avoid_notes evidence;
          confidence = conf;
          reason = confidence_reason conf;
          next = next_for_query task uses;
        })
  else
    Plan
      {
        task;
        answer = answer_for_plan snapshot task uses evidence;
        steps = plan_steps uses evidence;
        uses;
        evidence;
        avoid = avoid_notes evidence;
        confidence = conf;
        reason = confidence_reason conf;
        next = next_for_query task uses;
      }

let run snapshot opts =
  match (opts.browse, opts.why, opts.query) with
  | Some m, _, _ -> browse snapshot m ~more:opts.more
  | _, Some id, _ -> why snapshot id
  | _, _, Some task -> query snapshot ~more:opts.more task
  | _ -> browse snapshot "Fennec" ~more:false

let tiny_snapshot =
  {
    schema_version = 1;
    generated_at = "test";
    packages = [ { name = "fennec"; version = "0"; digest = "x" } ];
    public_items =
      [
        { id = "api:Paw.Basic_auth.make"; package = "fennec-paw"; library = "fennec-paw"; path = "Paw.Basic_auth.make"; kind = Value; signature = Some "val make"; doc = Some "basic authentication middleware"; source = Source_ref.make ~path:"f.mli" ~line:1 () };
        { id = "api:Paw.Endpoint.pipe_matched"; package = "fennec-paw"; library = "fennec-paw"; path = "Paw.Endpoint.pipe_matched"; kind = Value; signature = Some "val pipe_matched"; doc = Some "run middleware after route match"; source = Source_ref.make ~path:"e.mli" ~line:2 () };
      ];
    api_index = [];
    evidence =
      [
        { id = "test:auth"; kind = Test; package = "fennec"; label = "protect admin route with matched auth"; text = "protect admin route using pipe_matched Basic_auth.make returns 401 while unmatched stays 404"; apis = [ "api:Paw.Basic_auth.make"; "api:Paw.Endpoint.pipe_matched" ]; source = Source_ref.make ~path:"t.ml" ~line:3 () };
      ];
    evidence_index = [];
    api_evidence_index = [];
  }

let%test "auth query returns a plan" =
  match query tiny_snapshot ~more:false "protect admin route with auth" with
  | Plan { uses; confidence; _ } -> uses <> [] && confidence <> Conf_insufficient
  | _ -> false
