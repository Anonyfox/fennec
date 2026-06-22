open Discover_model

(* Golden task fixtures. These assert SOURCE-TRUTH, not prose: for a task-shaped query, discover must
   return the right CARD type, recommend APIs in the right framework AREA, and back them with the right
   evidence FILES. They deliberately do NOT pin an exact winning symbol or hand-written summary words —
   the ranker is generic (no per-task table), so golden checks the area it lands in, which stays stable
   as the framework evolves. Substrings match against the rendered API paths / evidence source paths. *)
type expected = {
  query : string;
  card : string;  (** "plan" or "compare" *)
  uses_include : string list;  (** each must be a substring of some recommended API path *)
  evidence_include : string list;  (** each must be a substring of some evidence source path *)
}

let tasks =
  [
    { query = "protect only matched admin routes with basic auth"; card = "plan";
      uses_include = [ "Basic_auth"; "use_matched" ]; evidence_include = [ "server.ml"; "domains_test" ] };
    { query = "set and delete a response cookie"; card = "plan";
      uses_include = [ "set_cookie"; "delete_cookie" ]; evidence_include = [ "cookie"; "conn" ] };
    { query = "add signed cookie-backed sessions"; card = "plan";
      uses_include = [ "Session" ]; evidence_include = [ "session" ] };
    { query = "build an SSR page with a local counter"; card = "plan";
      uses_include = [ "Fur" ]; evidence_include = [ "counter.mlx" ] };
    { query = "write an HTTP test"; card = "plan";
      uses_include = [ "Fennec_hunt.Http" ]; evidence_include = [ "test/http" ] };
    { query = "upload a file from a multipart form"; card = "plan";
      uses_include = [ "Multipart"; "Conn.file" ]; evidence_include = [ "multipart" ] };
    { query = "stream a response body in chunks"; card = "plan";
      uses_include = [ "Conn.stream" ]; evidence_include = [ "conn.ml" ] };
    { query = "add a dynamic route and typed path link"; card = "plan";
      uses_include = [ "Fur.Router" ]; evidence_include = [ "id_.mlx" ] };
    { query = "choose Pulse live data vs local Fur state"; card = "compare";
      uses_include = [ "Fur"; "Pulse" ]; evidence_include = [ "task_list" ] };
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
    List.map (fun (e : evidence) -> e.source.path) evidence
  | _ -> []

(* a card is "bounded" — the orientation contract is one screen, not a match dump *)
let card_lines card = 1 + List.length (String.split_on_char '\n' (Render_text.render card))

let contains ~needle haystack =
  let needle = String.lowercase_ascii needle and haystack = String.lowercase_ascii haystack in
  let n = String.length needle and h = String.length haystack in
  let rec go i = i + n <= h && (String.sub haystack i n = needle || go (i + 1)) in
  n = 0 || go 0

let all_in ~needles haystack = List.for_all (fun needle -> contains ~needle haystack) needles

let check snapshot =
  List.filter_map
    (fun t ->
      (* card type is judged on the DEFAULT card the user sees; presence of APIs/evidence on the --more
         card (a superset) so a tight default screen never fails a legitimately-present item *)
      let default_card = Query.query snapshot ~more:false t.query in
      let more_card = Query.query snapshot ~more:true t.query in
      let uses = String.concat " " (card_uses more_card) in
      let evidence = String.concat " " (card_evidence more_card) in
      let card_ok = card_name default_card = t.card && card_name more_card = t.card in
      let uses_ok = all_in ~needles:t.uses_include uses in
      let evidence_ok = all_in ~needles:t.evidence_include evidence in
      let bounded_ok = card_lines default_card <= 48 in
      if card_ok && uses_ok && evidence_ok && bounded_ok then None
      else
        Some
          (Printf.sprintf
             "%S expected %s/uses⊇[%s]/evidence⊇[%s]; got %s/uses [%s]/evidence [%s]%s"
             t.query t.card
             (String.concat ", " t.uses_include)
             (String.concat ", " t.evidence_include)
             (card_name default_card) uses evidence
             (if bounded_ok then "" else Printf.sprintf "/UNBOUNDED %d lines" (card_lines default_card))))
    tasks
