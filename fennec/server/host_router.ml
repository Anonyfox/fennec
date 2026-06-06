(* See host_router.mli. The validated, sorted host->endpoint table. The ONLY way to obtain one is
   [build], which enforces every routing invariant once — names non-empty/clean/unique, every
   pattern parses, AT MOST ONE catch-all, no two endpoints claiming the same pattern — and sorts the
   specific patterns most-specific-first. So the server holds a table whose illegal states are
   unrepresentable, and [route] is a total walk: most-specific match, else the single default, else
   None (an unknown host with no "*" → the caller 404s, never a misroute to the wrong tenant).

   [build] reports ALL errors at once so the developer fixes everything in one pass — not a
   fix-run-fix-run cycle. *)

module P = Host_pattern

type 'ep entry = { name : string; patterns : P.t list; ep : 'ep }

type 'ep t = {
  trie : 'ep Host_trie.t; (* O(1) exact / O(depth) suffix matching — replaces the linear scan *)
  default : 'ep option; (* the single "*" owner's payload, if any *)
  entries : 'ep entry list; (* as DECLARED (declaration order) — for dev port allocation + banner *)
}

type error =
  | Bad_name of string
  | Duplicate_name of string
  | No_patterns of string
  | Bad_pattern of string * string
  | Multiple_catch_all of string list
  | Conflicting_pattern of string * string * string

(* ──── name_ok ──── *)

let name_ok n = n <> "" && not (String.exists (fun c -> c = ' ' || c = '=' || c = '\t' || c = '\n') n)

(* ──── parse_entry ──── *)

let parse_entry (name, raws, ep) =
  let errs = ref [] in
  if not (name_ok name) then errs := Bad_name name :: !errs;
  if raws = [] then errs := No_patterns name :: !errs;
  let patterns =
    List.filter_map
      (fun r -> match P.of_string r with Ok p -> Some p | Error msg -> errs := Bad_pattern (name, msg) :: !errs; None)
      raws
  in
  match !errs with [] -> Ok { name; patterns; ep } | es -> Error es

(* ──── build ──── *)

let build (inputs : (string * string list * 'ep) list) : ('ep t, error list) result =
  let errs = ref [] in
  let entries =
    List.filter_map
      (fun x -> match parse_entry x with Ok e -> Some e | Error es -> errs := es @ !errs; None)
      inputs
  in
  (* duplicate names *)
  let seen_names = Hashtbl.create 8 in
  List.iter
    (fun e ->
      if Hashtbl.mem seen_names e.name then errs := Duplicate_name e.name :: !errs
      else Hashtbl.replace seen_names e.name ())
    entries;
  (* at most one catch-all *)
  let catch_alls = List.filter_map (fun e -> if List.mem P.Any e.patterns then Some e.name else None) entries in
  if List.length catch_alls > 1 then errs := Multiple_catch_all catch_alls :: !errs;
  (* no two endpoints claiming the same non-Any pattern *)
  let pat_owners = Hashtbl.create 16 in
  List.iter
    (fun e ->
      List.iter
        (fun p ->
          if p <> P.Any then
            match Hashtbl.find_opt pat_owners (P.to_string p) with
            | Some prev when prev <> e.name -> errs := Conflicting_pattern (P.to_string p, prev, e.name) :: !errs
            | _ -> Hashtbl.replace pat_owners (P.to_string p) e.name)
        e.patterns)
    entries;
  match List.rev !errs with
  | _ :: _ as all_errs -> Error all_errs
  | [] ->
    let all_patterns = List.concat_map (fun e -> List.filter_map (fun p -> if p = P.Any then None else Some (p, e.ep)) e.patterns) entries in
    let trie = Host_trie.build all_patterns in
    let default = List.find_map (fun e -> if List.mem P.Any e.patterns then Some e.ep else None) entries in
    Ok { trie; default; entries }

let build' pairs = build (List.map (fun (n, ps) -> (n, ps, n)) pairs)
let has_err k = function Error es -> List.exists k es | Ok _ -> false

(* build validation *)
let%test "valid table"                  = Result.is_ok (build' [ ("web", [ "*" ]); ("admin", [ "admin.acme.com" ]) ])
let%test "empty list is ok"             = Result.is_ok (build' [])
let%test "two catch-alls"               = has_err (function Multiple_catch_all _ -> true | _ -> false) (build' [ ("web", [ "*" ]); ("other", [ "*" ]) ])
let%test "same exact conflicts"         = has_err (function Conflicting_pattern _ -> true | _ -> false) (build' [ ("a", [ "x.com" ]); ("b", [ "x.com" ]) ])
let%test "duplicate name"               = has_err (function Duplicate_name _ -> true | _ -> false) (build' [ ("web", [ "a.com" ]); ("web", [ "b.com" ]) ])
let%test "empty name"                   = has_err (function Bad_name _ -> true | _ -> false) (build' [ ("", [ "*" ]) ])
let%test "name with space"              = has_err (function Bad_name _ -> true | _ -> false) (build' [ ("we b", [ "*" ]) ])
let%test "no patterns"                  = has_err (function No_patterns _ -> true | _ -> false) (build' [ ("web", []) ])
let%test "bad pattern"                  = has_err (function Bad_pattern _ -> true | _ -> false) (build' [ ("web", [ "a*b" ]) ])
let%test "exact + wildcard no conflict" = Result.is_ok (build' [ ("api", [ "api.acme.com" ]); ("rest", [ "*.acme.com" ]) ])

let%test_unit "multi-error: both reported" =
  match build' [ ("", [ "*" ]); ("a", [ "x.com" ]); ("b", [ "x.com" ]) ] with
  | Error es ->
    Fennec_hunt_unit.check "bad name present" (List.exists (function Bad_name _ -> true | _ -> false) es);
    Fennec_hunt_unit.check "conflict present" (List.exists (function Conflicting_pattern _ -> true | _ -> false) es);
    Fennec_hunt_unit.check "at least 2 errors" (List.length es >= 2)
  | Ok _ -> Fennec_hunt_unit.check "should have failed" false

(* ──── route ──── *)

let route (t : 'ep t) ~(host : string) : 'ep option =
  match Host_trie.lookup t.trie ~host with Some _ as hit -> hit | None -> t.default

(* route precedence *)
let%test "exact beats wildcard + default" =
  let t = Result.get_ok (build' [ ("web", [ "*" ]); ("admin", [ "admin.acme.com" ]); ("api", [ "*.acme.com" ]) ]) in
  route t ~host:"admin.acme.com" = Some "admin"
let%test "wildcard beats default" =
  let t = Result.get_ok (build' [ ("web", [ "*" ]); ("admin", [ "admin.acme.com" ]); ("api", [ "*.acme.com" ]) ]) in
  route t ~host:"x.acme.com" = Some "api"
let%test "unknown falls to '*'" =
  let t = Result.get_ok (build' [ ("web", [ "*" ]); ("admin", [ "admin.acme.com" ]); ("api", [ "*.acme.com" ]) ]) in
  route t ~host:"totally.else.com" = Some "web"
let%test "specificity not decl order" =
  let t2 = Result.get_ok (build' [ ("api", [ "*.acme.com" ]); ("admin", [ "admin.acme.com" ]); ("web", [ "*" ]) ]) in
  route t2 ~host:"admin.acme.com" = Some "admin" && route t2 ~host:"x.acme.com" = Some "api"
let%test "longer suffix wins" =
  let tn = Result.get_ok (build' [ ("broad", [ "*.acme.com" ]); ("narrow", [ "*.api.acme.com" ]) ]) in
  route tn ~host:"x.api.acme.com" = Some "narrow"
let%test "shorter suffix at its level" =
  let tn = Result.get_ok (build' [ ("broad", [ "*.acme.com" ]); ("narrow", [ "*.api.acme.com" ]) ]) in
  route tn ~host:"x.acme.com" = Some "broad"
let%test "no '*' -> unknown is None" =
  let t3 = Result.get_ok (build' [ ("admin", [ "admin.acme.com" ]) ]) in
  route t3 ~host:"nope.com" = None
let%test "no '*' -> match still routes" =
  let t3 = Result.get_ok (build' [ ("admin", [ "admin.acme.com" ]) ]) in
  route t3 ~host:"admin.acme.com" = Some "admin"

(* ──── entries ──── *)

let entries (t : 'ep t) = t.entries

let%test "entries keep decl order" =
  let te = Result.get_ok (build' [ ("web", [ "*" ]); ("admin", [ "admin.acme.com" ]) ]) in
  List.map (fun e -> e.name) (entries te) = [ "web"; "admin" ]

(* ──── describe_error ──── *)

let describe_error = function
  | Bad_name n -> Printf.sprintf "endpoint name %S is empty or contains whitespace/'='" n
  | Duplicate_name n -> Printf.sprintf "two endpoints share the name %S" n
  | No_patterns n -> Printf.sprintf "endpoint %S declares no host patterns" n
  | Bad_pattern (n, msg) -> Printf.sprintf "endpoint %S: %s" n msg
  | Multiple_catch_all names -> Printf.sprintf "more than one catch-all \"*\" (%s) — only one endpoint may be the default" (String.concat ", " names)
  | Conflicting_pattern (pat, a, b) -> Printf.sprintf "endpoints %S and %S both claim the host %S" a b pat

(* ──── describe_errors ──── *)

let describe_errors errs = String.concat "\n" (List.map describe_error errs)
