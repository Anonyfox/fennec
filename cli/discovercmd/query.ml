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
  starts_with path "Fennec." || starts_with path "Fur." || starts_with path "Pulse." || starts_with path "Fennec_hunt."

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

let path_in uses path =
  List.exists (fun (i : public_item) -> i.path = path || starts_with i.path (path ^ ".")) uses

let lead_path = function
  | item :: _ -> item.path
  | [] -> ""

let lead_is uses path =
  let p = lead_path uses in
  p = path || starts_with p (path ^ ".")

let has_use uses path = path_in uses path

let terms_match terms xs = List.exists (fun x -> List.mem x terms) xs

type plan_kind =
  | Matched_auth
  | Response_cookie
  | Session
  | Upload
  | Chunked_stream
  | Http_test
  | Local_state
  | Router
  | Generic

let infer_plan_kind terms uses evidence =
  let wants_test = terms_match terms [ "test"; "tests"; "assert"; "assertion"; "expect" ] in
  let has_route_evidence = List.exists (fun e -> e.kind = Route) evidence in
  let conn_lead = lead_is uses "Fennec.Conn" in
  if has_use uses "Fennec.Paw.Basic_auth" && has_use uses "Fennec.Endpoint" then Matched_auth
  else if conn_lead && terms_match terms [ "cookie"; "cookies" ] then Response_cookie
  else if lead_is uses "Fennec.Paw.Session" || has_use uses "Fennec.Paw.Session" then Session
  else if
    (lead_is uses "Fennec.Conn.files" || lead_is uses "Fennec.Conn.file")
    || (has_use uses "Fennec.Conn.files" && terms_match terms [ "upload"; "uploads"; "multipart"; "form" ])
  then Upload
  else if
    (lead_is uses "Fennec.Conn.send_chunked" || lead_is uses "Fennec.Conn.stream")
    || (has_use uses "Fennec.Conn.send_chunked" && terms_match terms [ "stream"; "streams"; "chunk"; "chunks"; "chunked"; "sse" ])
  then Chunked_stream
  else if (lead_is uses "Fennec_hunt.Http" || has_use uses "Fennec_hunt.Http") && wants_test then Http_test
  else if lead_is uses "Fur" && terms_match terms [ "counter"; "local"; "state" ] then Local_state
  else if lead_is uses "Fur.Router" || has_route_evidence then Router
  else Generic

let first_use uses =
  match uses with
  | item :: _ -> item.path
  | [] -> "the highest-ranked public API"

let default_summary task uses =
  Printf.sprintf "Start with %s for %s." (first_use uses) task

let starter_for = function
  | Response_cookie ->
    Some
      "let conn = Fennec.Conn.set_cookie conn \"seen\" \"1\" in\n\
       let conn = Fennec.Conn.delete_cookie conn \"old\" in\n\
       conn"
  | Session ->
    Some
      "let session = Fennec.Paw.Session.make ~secret:\"...\" ()\n\
       \n\
       (* Add the session paw early, then read/write session values downstream. *)"
  | Upload ->
    Some
      "match Fennec.Conn.file conn \"upload\" with\n\
       | Some part -> Fennec.Conn.text conn part.data\n\
       | None -> Fennec.Conn.text ~status:400 conn \"missing upload\""
  | Chunked_stream ->
    Some
      "Fennec.Conn.send_chunked conn ~content_type:\"text/event-stream\" (fun emit ->\n\
       \  emit \"data: ready\\n\\n\")"
  | Http_test ->
    Some
      "open Fennec_hunt.Http\n\
       \n\
       let%http \"health\" = fun () ->\n\
       \  check \"GET /health\" (fun () ->\n\
       \    get \"/health\" ~expect:[status 200])"
  | Router ->
    Some
      "(* app/products/id_.mlx becomes a dynamic route such as /products/:id. *)\n\
       let href = Fur.Router.path router \"/products/%s\" product_id"
  | Local_state ->
    Some
      "let count = Fur.signal 0\n\
       \n\
       (* Render with Fur.get count; update from browser event handlers. *)\n\
       Fur.update count succ"
  | Matched_auth | Generic -> None

let next_for_query task (uses : public_item list) =
  let why = match uses with i :: _ -> [ "fennec discover --why " ^ i.id ] | [] -> [] in
  why @ [ Printf.sprintf "fennec discover --more %S" task ]

let plan_summary kind task uses =
  match kind with
  | Matched_auth ->
    "Protect real routes with matched-route middleware: define the endpoint, then attach Basic_auth in the matched phase."
  | Response_cookie ->
    "Use Fennec.Conn response-cookie helpers for one-off browser cookies; use sessions for signed request-to-request state."
  | Session ->
    "Use Fennec.Paw.Session for signed cookie-backed session state such as login data, flash values, or preferences."
  | Upload ->
    "Use Fennec.Conn.files or Fennec.Conn.file to read uploaded multipart/form-data parts in the request handler."
  | Chunked_stream ->
    "Use Fennec.Conn.send_chunked to answer with a streamed chunked response body; use HTTP/browser tests only to prove it reassembles."
  | Local_state ->
    "Use Fur.signal for local component state; SSR renders the initial value and browser handlers update it after hydration."
  | Http_test ->
    "Use Fennec_hunt.Http for endpoint-level HTTP tests: register a suite, make requests, and assert the response."
  | Router ->
    "Use Fur.Router and generated route/path facts for dynamic routes and compiler-checked in-app links."
  | Generic -> default_summary task uses

let plan_why kind evidence =
  let proof =
    if List.exists (fun (e : evidence) -> e.kind = Test) evidence then [ "A matching test backs the recommendation." ]
    else if evidence <> [] then [ "A framework example backs the recommendation." ]
    else []
  in
  let reason =
    match kind with
    | Matched_auth ->
      [
        "Matched middleware protects existing routes without turning unrelated misses into auth failures.";
        "The endpoint API owns host/app routing, while Basic_auth is the reusable paw.";
      ]
    | Response_cookie ->
      [
        "Conn cookie helpers write Set-Cookie response headers without answering the request.";
        "Request cookie readers and response cookie writers are separate on purpose.";
      ]
    | Session ->
      [
        "The session paw signs the browser cookie and exposes session values downstream.";
        "It is the right abstraction for request-to-request state, unlike one-off response cookies.";
      ]
    | Upload ->
      [
        "Conn.files exposes parsed multipart file parts from the incoming request body.";
        "The Hunt multipart helpers are for tests that submit uploads, not for reading uploads in handlers.";
      ]
    | Chunked_stream ->
      [
        "send_chunked is the answerer that streams chunks without buffering the full body.";
        "Conn.stream and Hunt response-body helpers are observation surfaces around the streamed response.";
      ]
    | Local_state ->
      [
        "Fur.signal is local UI state owned by a component or page.";
        "Fur.get reads during render; Fur.update changes the value from event handlers.";
      ]
    | Http_test ->
      [
        "HTTP tests exercise the app through real requests and focused response assertions.";
        "The public surface is Fennec_hunt.Http, not its internal For_test helpers.";
      ]
    | Router ->
      [
        "Route files produce route patterns and typed path/link helpers.";
        "The router keeps app navigation compiler-checked instead of stringly scattered.";
      ]
    | Generic -> [ "The selected APIs and evidence have the strongest public match for this task." ]
  in
  take 3 (reason @ proof)

let answer_for_plan task terms uses evidence =
  let kind = infer_plan_kind terms uses evidence in
  {
    summary = plan_summary kind task uses;
    why = plan_why kind evidence;
    starter = starter_for kind;
    copy_next = next_for_query task uses;
  }

let compare_axis terms left right =
  let has x = List.mem x terms in
  if
    (has "local" || has "state")
    && (starts_with left.path "Fur" || starts_with right.path "Fur")
    && (starts_with left.path "Pulse.Live" || starts_with right.path "Pulse.Live")
  then
    ( "state scope",
      "Use Fur.signal for local browser/component state such as counters, toggles, tabs, and input drafts.",
      "Use Pulse.Live for server-backed data that should update across clients, such as task lists or notifications." )
  else
    ( "fit",
      "Use this when its public API and evidence match the task more directly.",
      "Use this when its public API and evidence better match the adjacent concern." )

let answer_for_compare terms left right =
  let axis, left_when, right_when = compare_axis terms left right in
  let summary =
    if axis = "state scope" then
      "Choose Fur.signal for local UI state; choose Pulse.Live for server-backed realtime data shared across clients."
    else
      Printf.sprintf "Compare %s and %s by which public surface fits the task evidence." left.path right.path
  in
  {
    summary;
    why = [ left_when; right_when ];
    starter = None;
    copy_next = [ "fennec discover --why " ^ left.id; "fennec discover --why " ^ right.id ];
  }

let plan_steps terms uses evidence =
  match infer_plan_kind terms uses evidence with
  | Matched_auth ->
    [
      "Define the endpoint/app route surface with Fennec.Endpoint.";
      "Attach Basic_auth in the matched phase so unrelated misses still behave like misses.";
      "Use the system/http evidence as the regression shape.";
    ]
  | Response_cookie ->
    [
      "Read request cookies with Conn.cookie/cookies only when you need browser-sent values.";
      "Write response cookies with Conn.set_cookie and expire them with Conn.delete_cookie.";
      "Use Session instead when the value is signed request-to-request application state.";
    ]
  | Session ->
    [
      "Create the session paw with a strong secret and add it early in the pipeline.";
      "Read and mutate session values downstream through the session API.";
      "Use plain Conn.set_cookie only for one-off response cookies.";
    ]
  | Upload ->
    [
      "Handle the multipart request in a paw or endpoint handler.";
      "Read all uploaded parts with Conn.files, or a named upload with Conn.file.";
      "Use Fennec_hunt.Http multipart helpers only in the regression test.";
    ]
  | Chunked_stream ->
    [
      "Answer the request with Conn.send_chunked.";
      "Emit each chunk from the producer callback; use text/event-stream for SSE.";
      "Cover it with HTTP or browser evidence that reads the complete response body.";
    ]
  | Local_state ->
    [
      "Create a Fur.signal for state owned by this component/page.";
      "Read it during render with Fur.get.";
      "Update it from browser handlers with Fur.set or Fur.update.";
    ]
  | Http_test ->
    [
      "Open Fennec_hunt.Http in an HTTP test file.";
      "Register a let%http suite and group checks with check.";
      "Make requests with get/post and assert responses with status, JSON, body, cookie, or timing helpers.";
    ]
  | Router ->
    [
      "Start from the generated route shape shown in the evidence.";
      "Use the public routing/link API from the recommendation list.";
      "Keep route modules as normal Dune modules so edits stay local and typed.";
    ]
  | Generic ->
    [
      "Start from the highest-ranked public API below.";
      "Use `--why` on that API before editing if the signature is unfamiliar.";
      "Follow the closest example/test evidence, then add the narrowest focused test.";
    ]

let avoid_notes evidence =
  evidence
  |> List.filter_map (fun e -> match e.kind with Hazard -> Some e.text | _ -> None)
  |> take 2

let evidence_card_score terms selected_ids (r : Retrieve.evidence_result) =
  let e = r.Retrieve.ev in
  let path = e.source.path in
  let linked = List.exists (fun api -> Hashtbl.mem selected_ids api) e.apis in
  let has x = List.mem x terms in
  let source =
    if has "session" && contains_sub ~needle:"fennec/server/session" path then 110.0
    else if has "cookie" && contains_sub ~needle:"examples/site/test/browser" path then -.80.0
    else if has "cookie" && contains_sub ~needle:"fennec/paw/conn.ml" path then 75.0
    else if has "cookie" && contains_sub ~needle:"fennec/core/cookie.ml" path then 70.0
    else if contains_sub ~needle:"examples/site/frontend/components/task_list" path && (has "pulse" || has "live") then 120.0
    else if contains_sub ~needle:"examples/site/test/browser/web_test" path && (has "local" || has "state" || has "counter") then 90.0
    else if contains_sub ~needle:"examples/site/frontend/components/counter" path && (has "local" || has "counter") then 85.0
    else if contains_sub ~needle:"examples/site/server.ml" path then 35.0
    else if contains_sub ~needle:"examples/site/test/system/domains_test" path then 35.0
    else if contains_sub ~needle:"examples/site/test/http" path then 30.0
    else if contains_sub ~needle:"examples/site/" path then 18.0
    else if starts_with path "fennec/" then -.12.0
    else 0.0
  in
  let kind_relevance =
    match e.kind with
    | Route when has "route" || has "path" || has "link" || has "dynamic" -> 120.0
    | Route -> 12.0
    | Test when has "test" -> 20.0
    | Doctest when has "example" -> 12.0
    | Example | Test | Doctest | Hazard -> 0.0
  in
  source
  +. kind_relevance
  +. (if linked then 24.0 else 0.0)
  +. (r.score *. 3.0)
  +. (match e.kind with Test -> 8.0 | Example -> 6.0 | Route -> 5.0 | Doctest -> 4.0 | Hazard -> -.20.0)

let find_api = Retrieve.find_api

let compare_card snapshot task terms uses evidence =
  match Select.compare_pair ~task ~terms ~uses ~public_items:snapshot.public_items with
  | None -> None
  | Some (left, right) ->
    let axis, left_when, right_when = compare_axis terms left right in
    let answer = answer_for_compare terms left right in
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
  let selected_results =
    let selected = Hashtbl.create (List.length uses * 2) in
    List.iter (fun (i : public_item) -> Hashtbl.replace selected i.id ()) uses;
    api_results
    |> List.filter (fun r -> Hashtbl.mem selected r.Retrieve.item.id)
  in
  let evidence : evidence list =
    let max_per_source = if mentions terms [ "vs"; "versus"; "choose"; "when" ] then 1 else 2 in
    let selected_ids = Hashtbl.create (List.length uses * 2) in
    List.iter (fun (i : public_item) -> Hashtbl.replace selected_ids i.id ()) uses;
    Retrieve.evidence snapshot terms selected_results
    |> List.map (fun r -> (r.Retrieve.ev, evidence_card_score terms selected_ids r))
    |> List.sort (fun (_, a) (_, b) -> compare b a)
    |> List.map fst
    |> limit_evidence_per_source max_per_source
    |> take (if more then 8 else 4)
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
          answer = answer_for_plan task terms uses evidence;
          steps = plan_steps terms uses evidence;
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
        answer = answer_for_plan task terms uses evidence;
        steps = plan_steps terms uses evidence;
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
        { id = "api:Fennec.Paw.Basic_auth.make"; package = "fennec"; library = "fennec"; path = "Fennec.Paw.Basic_auth.make"; kind = Value; signature = Some "val make"; doc = Some "basic authentication middleware"; source = Source_ref.make ~path:"f.mli" ~line:1 () };
        { id = "api:Fennec.Endpoint.pipe_matched"; package = "fennec"; library = "fennec"; path = "Fennec.Endpoint.pipe_matched"; kind = Value; signature = Some "val pipe_matched"; doc = Some "run middleware after route match"; source = Source_ref.make ~path:"e.mli" ~line:2 () };
      ];
    api_index = [];
    evidence =
      [
        { id = "test:auth"; kind = Test; package = "fennec"; label = "protect admin route with matched auth"; text = "protect admin route using pipe_matched Basic_auth.make returns 401 while unmatched stays 404"; apis = [ "api:Fennec.Paw.Basic_auth.make"; "api:Fennec.Endpoint.pipe_matched" ]; source = Source_ref.make ~path:"t.ml" ~line:3 () };
      ];
    evidence_index = [];
    api_evidence_index = [];
  }

let%test "auth query returns a plan" =
  match query tiny_snapshot ~more:false "protect admin route with auth" with
  | Plan { uses; confidence; _ } -> uses <> [] && confidence <> Conf_insufficient
  | _ -> false
