open Discover_model

type expected = {
  query : string;
  must_use : string list;
  must_evidence : string list;
  must_answer : string list;
  starter : bool;
  must_card : string;
  top_use : string option;
  default_use : string list;
  default_evidence : string list;
}

let tasks =
  [
    {
      query = "protect only matched admin routes with basic auth";
      must_use = [ "Fennec.Paw.Basic_auth"; "Fennec.Endpoint.pipe_matched" ];
      must_evidence = [ "server.ml"; "domains_test.ml" ];
      must_answer = [ "matched"; "Basic_auth" ];
      starter = false;
      must_card = "plan";
      top_use = Some "Fennec.Endpoint";
      default_use = [ "Fennec.Paw.Basic_auth"; "Fennec.Endpoint.pipe_matched" ];
      default_evidence = [ "server.ml"; "domains_test.ml" ];
    };
    {
      query = "set and delete a response cookie";
      must_use = [ "Fennec.Conn.set_cookie"; "Fennec.Conn.delete_cookie" ];
      must_evidence = [ "cookie" ];
      must_answer = [ "response-cookie"; "sessions" ];
      starter = true;
      must_card = "plan";
      top_use = Some "Fennec.Conn.set_cookie";
      default_use = [ "Fennec.Conn.set_cookie"; "Fennec.Conn.delete_cookie" ];
      default_evidence = [ "conn.ml"; "cookie.ml" ];
    };
    {
      query = "add signed cookie-backed sessions";
      must_use = [ "Fennec.Paw.Session" ];
      must_evidence = [ "session.ml" ];
      must_answer = [ "Session"; "signed" ];
      starter = true;
      must_card = "plan";
      top_use = Some "Fennec.Paw.Session.make";
      default_use = [ "Fennec.Paw.Session" ];
      default_evidence = [ "session.ml" ];
    };
    {
      query = "build an SSR page with a local counter";
      must_use = [ "Fur" ];
      must_evidence = [ "counter.mlx" ];
      must_answer = [ "Fur.signal"; "hydration" ];
      starter = true;
      must_card = "plan";
      top_use = Some "Fur.signal";
      default_use = [ "Fur.signal"; "Fur.get" ];
      default_evidence = [ "counter.mlx" ];
    };
    {
      query = "write an HTTP test";
      must_use = [ "Fennec_hunt.Http" ];
      must_evidence = [ "test/http" ];
      must_answer = [ "Fennec_hunt.Http"; "assert" ];
      starter = true;
      must_card = "plan";
      top_use = Some "Fennec_hunt.Http";
      default_use = [ "Fennec_hunt.Http" ];
      default_evidence = [ "test/http" ];
    };
    {
      query = "upload a file from a multipart form";
      must_use = [ "Fennec.Conn.files"; "Fennec.Conn.file" ];
      must_evidence = [ "multipart" ];
      must_answer = [ "Fennec.Conn.files"; "multipart/form-data"; "uploads" ];
      starter = true;
      must_card = "plan";
      top_use = Some "Fennec.Conn.files";
      default_use = [ "Fennec.Conn.files"; "Fennec.Conn.file" ];
      default_evidence = [ "multipart" ];
    };
    {
      query = "stream a response body in chunks";
      must_use = [ "Fennec.Conn.send_chunked" ];
      must_evidence = [ "send_chunked" ];
      must_answer = [ "Fennec.Conn.send_chunked"; "streamed"; "chunked" ];
      starter = true;
      must_card = "plan";
      top_use = Some "Fennec.Conn.send_chunked";
      default_use = [ "Fennec.Conn.send_chunked" ];
      default_evidence = [ "send_chunked" ];
    };
    {
      query = "add a dynamic route and typed path link";
      must_use = [ "Fur.Router" ];
      must_evidence = [ "products/id_.mlx" ];
      must_answer = [ "Fur.Router"; "typed" ];
      starter = true;
      must_card = "plan";
      top_use = Some "Fur.Router";
      default_use = [ "Fur.Router" ];
      default_evidence = [ "products/id_.mlx" ];
    };
    {
      query = "choose Pulse live data vs local Fur state";
      must_use = [ "Fur.signal"; "Pulse.Live" ];
      must_evidence = [ "web_test.ml"; "task_list.mlx" ];
      must_answer = [ "Fur.signal"; "Pulse.Live"; "server-backed" ];
      starter = false;
      must_card = "compare";
      top_use = Some "Fur.signal";
      default_use = [ "Fur.signal"; "Pulse.Live" ];
      default_evidence = [ "web_test.ml"; "task_list.mlx" ];
    };
  ]

let card_name = function
  | Plan _ -> "plan"
  | Compare _ -> "compare"
  | Browse _ -> "browse"
  | Why _ -> "why"
  | Insufficient _ -> "insufficient"

let card_uses = function
  | Plan { uses; _ } -> List.map (fun i -> i.path) uses
  | Compare { left; right; _ } -> [ left.path; right.path ]
  | Browse { items; _ } -> List.map (fun i -> i.path) items
  | _ -> []

let card_evidence = function
  | Plan { evidence; _ } | Compare { evidence; _ } | Browse { evidence; _ } ->
    List.map (fun (e : evidence) -> e.id ^ " " ^ e.label ^ " " ^ e.source.path) evidence
  | _ -> []

let card_answer = function
  | Plan { answer; _ } | Compare { answer; _ } ->
    let starter = match answer.starter with Some s -> [ s ] | None -> [] in
    String.concat " " (answer.summary :: (starter @ answer.why))
  | _ -> ""

let card_has_starter = function
  | Plan { answer = { starter = Some _; _ }; _ } -> true
  | _ -> false

let contains ~needle haystack =
  let needle = String.lowercase_ascii needle in
  let haystack = String.lowercase_ascii haystack in
  let n = String.length needle and h = String.length haystack in
  let rec go i =
    i + n <= h && (String.sub haystack i n = needle || go (i + 1))
  in
  n = 0 || go 0

let check snapshot =
  List.filter_map
    (fun t ->
      let more_card = Query.query snapshot ~more:true t.query in
      let default_card = Query.query snapshot ~more:false t.query in
      let name_ok = card_name more_card = t.must_card && card_name default_card = t.must_card in
      let uses = String.concat " " (card_uses more_card) in
      let evidence = String.concat " " (card_evidence more_card) in
      let default_uses = card_uses default_card in
      let default_evidence = String.concat " " (card_evidence default_card) in
      let answer = card_answer default_card in
      let use_ok = List.for_all (fun needle -> contains ~needle uses) t.must_use in
      let evidence_ok = List.for_all (fun needle -> contains ~needle evidence) t.must_evidence in
      let answer_ok = List.for_all (fun needle -> contains ~needle answer) t.must_answer in
      let starter_ok = (not t.starter) || card_has_starter default_card in
      let top_ok =
        match (t.top_use, default_uses) with
        | None, _ -> true
        | Some needle, top :: _ -> contains ~needle top
        | Some _, [] -> false
      in
      let default_use_ok =
        let default_uses_text = String.concat " " default_uses in
        List.for_all (fun needle -> contains ~needle default_uses_text) t.default_use
      in
      let default_evidence_ok = List.for_all (fun needle -> contains ~needle default_evidence) t.default_evidence in
      if name_ok && use_ok && evidence_ok && answer_ok && starter_ok && top_ok && default_use_ok && default_evidence_ok then None
      else
        Some
          (Printf.sprintf "%S expected %s/use [%s]/evidence [%s]/answer [%s]/starter %b/top %s/default use [%s]/default evidence [%s], got %s/use [%s]/evidence [%s]/answer [%s]/starter %b/default use [%s]/default evidence [%s]"
             t.query t.must_card (String.concat ", " t.must_use) (String.concat ", " t.must_evidence)
             (String.concat ", " t.must_answer) t.starter
             (Option.value t.top_use ~default:"<none>")
             (String.concat ", " t.default_use)
             (String.concat ", " t.default_evidence)
             (card_name more_card) uses evidence answer (card_has_starter default_card) (String.concat " " default_uses) default_evidence))
    tasks
