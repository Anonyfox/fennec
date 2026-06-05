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
  specific : (P.t * 'ep) list; (* non-Any patterns, sorted most-specific first *)
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

let name_ok n = n <> "" && not (String.exists (fun c -> c = ' ' || c = '=' || c = '\t' || c = '\n') n)

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
    let owned = List.concat_map (fun e -> List.filter_map (fun p -> if p = P.Any then None else Some (p, e)) e.patterns) entries in
    let specific =
      owned
      |> List.map (fun (p, e) -> (p, e.ep))
      |> List.stable_sort (fun (a, _) (b, _) -> compare (P.specificity b) (P.specificity a))
    in
    let default = List.find_map (fun e -> if List.mem P.Any e.patterns then Some e.ep else None) entries in
    Ok { specific; default; entries }

let route (t : 'ep t) ~(host : string) : 'ep option =
  match List.find_opt (fun (p, _) -> P.matches p ~host) t.specific with Some (_, ep) -> Some ep | None -> t.default

let entries (t : 'ep t) = t.entries

let describe_error = function
  | Bad_name n -> Printf.sprintf "endpoint name %S is empty or contains whitespace/'='" n
  | Duplicate_name n -> Printf.sprintf "two endpoints share the name %S" n
  | No_patterns n -> Printf.sprintf "endpoint %S declares no host patterns" n
  | Bad_pattern (n, msg) -> Printf.sprintf "endpoint %S: %s" n msg
  | Multiple_catch_all names -> Printf.sprintf "more than one catch-all \"*\" (%s) — only one endpoint may be the default" (String.concat ", " names)
  | Conflicting_pattern (pat, a, b) -> Printf.sprintf "endpoints %S and %S both claim the host %S" a b pat

let describe_errors errs = String.concat "\n" (List.map describe_error errs)
